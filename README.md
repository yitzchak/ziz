# ziz

[![Quicklisp][quicklisp-badge]][quicklisp-ziz]

An ad hoc [Quicklisp][] distribution mainly intended for testing and building
[Quicklisp library bundles][].

## Usage

ziz will create a Quicklisp distrubution from a list of directories passed
as `:releases` to `with-distribution`. For more options please see see the
`distribution` class. Please note that ziz requires `tar` to be available in
the search path.

```common-lisp
(ql:quickload :ziz)

(ziz:with-distribution (dist :releases '("wibble"))
  (ql-dist:install-dist (ziz:distribution-info-url dist) :prompt nil)
  (ql:quickload :wible))
```


[quicklisp-badge]: http://quickdocs.org/badge/ziz.svg
[quicklisp-ziz]: http://quickdocs.org/ziz
[Quicklisp]: https://www.quicklisp.org/
[Quicklisp library bundles]: https://www.quicklisp.org/beta/bundles.html
