;;; -*- mode: lisp -*-

(asdf:defsystem :wallpaper-dl
  :author ("Ritchie Cai")
  :maintainer "Ritchie Cai"
  :description "Download wallpapers from interfacelift.com"
  :depends-on (:drakma :cl-ppcre :cl-html-parse)
  :components
  ((:file "pkgdcl")
   (:file "wallpaper-dl" :depends-on ("pkgdcl"))))
