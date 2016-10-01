;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;;; package.lisp

(defpackage #:melody
  (:use #:cl #:esrap)
  (:export #:play
           #:play-mml
           #:parse-mml
           #:demo-simple
           #:demo-song))
