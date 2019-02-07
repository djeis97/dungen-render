(asdf:defsystem #:dungen-render
  :description "A dungeon generator renderer."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.michaelfiano.com/projects/dungen-render"
  :source-control (:git "https://github.com/mfiano/dungen-render.git")
  :bug-tracker "https://github.com/mfiano/dungen-render/issues"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string
                       (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:golden-utils
               #:defpackage-plus
               #:umbra
               #:bloom
               #:dungen)
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "shaders")
     (:file "world")
     (:file "player")
     (:file "main")))))
