;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;;; package.lisp

(defpackage #:mml-speaker
  (:use #:cl #:esrap)
  (:export #:play
           #:play-mml
           #:parse-mml
           #:demo-simple
           #:demo-song))
