;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;;; melody.lisp
;; (c} 2016 Sergi Reyner
;; MIT License
;; Work in progress!

(in-package #:melody)

;; requires beep (sudo apt install beep)

(defun play-mml (mml) (play-openal (remove nil (parse-mml mml))))
(defun play-mml-speaker (mml) (play (remove nil (parse-mml mml))))



