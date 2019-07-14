(in-package #:ziz)

(defclass distribution ()
  ((name
     :initarg :name
     :initform "ziz"
     :accessor distribution-name)
   (version
     :initarg :version
     :initform 1
     :accessor distribution-version)
   (hostname
     :initform "localhost"
     :initarg :hostname
     :accessor distribution-hostname)
   (port
     :initarg :port
     :initform 8000
     :accessor distribution-port)
   (releases
     :initarg :releases
     :initform nil
     :accessor distribution-releases)
   (server
     :initform nil
     :accessor distribution-server)))

(defun create-distinfo (instance doc-root)
  (with-slots (name version hostname port) instance
    (with-open-file (stream (merge-pathnames "distinfo.txt" doc-root)
                            :direction :output :if-exists :supersede)
      (format stream "name: ~A~%version: ~A~%" name version)
      (format stream "archive-base-url: http://~A:~A/~%" hostname port)
      (format stream "canonical-distinfo-url: http://~A:~A/distinfo.txt~%" hostname port)
      (format stream "distinfo-subscription-url: http://~A:~A/distinfo.txt~%" hostname port)
      (format stream "release-index-url: http://~A:~A/releases.txt~%" hostname port)
      (format stream "system-index-url: http://~A:~A/systems.txt~%" hostname port))))

(defun load-asd (asd-path)
  (with-open-file (stream asd-path)
    (flet ((my-read (stream)
             (handler-bind
               ((error #'continue))
               (read stream nil :eof))))
      (do ((obj (my-read stream) (my-read stream))
           results)
          ((eql obj :eof) results)
        (when (and (listp obj)
                   (equalp (first obj) 'asdf:defsystem))
          (push obj results))))))

(defun add-to-system-index (instance doc-root stream name release-path asd-paths)
  (declare (ignore instance doc-root release-path))
  (dolist (asd-path asd-paths)
    (dolist (def (load-asd asd-path))
      (destructuring-bind (func system &rest rest &key depends-on &allow-other-keys) def
        (declare (ignore func rest))
        (format stream "~A ~A ~A~{ ~A~}~%"
          name
          (pathname-name asd-path)
          (asdf:coerce-name system)
          (mapcar #'asdf:coerce-name depends-on))))))

(defun create-release-tarball (doc-root release-path)
  (let* ((name (car (last (pathname-directory release-path))))
         (tarball-path (merge-pathnames (make-pathname :name name :type "tgz") doc-root)))
    (uiop:run-program
      (list
        "tar"
        "--exclude-vcs"
        "-C" (namestring (merge-pathnames (make-pathname :directory '(:relative :back)) release-path))
        "-czf" (namestring tarball-path)
        name))
    tarball-path))

(defun digest-file (digest path)
  (ironclad:byte-array-to-hex-string (ironclad:digest-file digest path)))

(defun add-to-release-index (instance doc-root stream name release-path asd-paths)
  (with-slots (hostname port) instance
    (let ((tarball-path (create-release-tarball doc-root release-path)))
      (format stream "~A http://~A:~A/~A ~A ~A ~A ~A~{ ~A~}~%"
        name
        hostname port (file-namestring tarball-path)
        (trivial-file-size:file-size-in-octets tarball-path)
        (digest-file :md5 tarball-path)
        (digest-file :sha1 tarball-path)
        name
        (mapcar #'file-namestring asd-paths)))))

(defun create-indicies (instance doc-root)
  (with-open-file (releases-stream (merge-pathnames "releases.txt" doc-root)
                    :direction :output :if-exists :supersede)
    (with-open-file (systems-stream (merge-pathnames "systems.txt" doc-root)
                      :direction :output :if-exists :supersede)
      (write-line "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]" releases-stream)
      (write-line "# project system-file system-name [dependency1..dependencyN]" systems-stream)
      (dolist (release-path (distribution-releases instance))
        (let ((asd-paths (directory (merge-pathnames "*.asd" release-path)))
              (name (car (last (pathname-directory release-path)))))
          (add-to-release-index instance doc-root releases-stream name release-path asd-paths)
          (add-to-system-index instance doc-root systems-stream name release-path asd-paths))))))

(defun start (instance)
  (with-slots (name hostname port releases server version) instance
    (let ((doc-root (merge-pathnames
                      (make-pathname
                        :directory (list :relative (format nil "~A_~A" name version)))
                      (uiop:temporary-directory))))
      (ensure-directories-exist doc-root)
      (create-distinfo instance doc-root)
      (create-indicies instance doc-root)
      (setf server (make-instance 'hunchentoot:easy-acceptor :port port :document-root doc-root))
      (hunchentoot:start server))))

(defun stop (instance)
  (hunchentoot:stop (distribution-server instance)))
