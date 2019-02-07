(in-package #:defpackage+-user-1)

(defpackage+ #:dungen-render
  (:local-nicknames (#:b #:bloom)
                    (#:bs #:bloom.shader)
                    (#:m #:game-math))
  (:use #:cl)
  (:inherit #:shadow.vari))
