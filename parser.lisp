;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:melody)

;; some helper functions
(defun integer-from-chars (chars)
  "Make an integer from a well formed list of digit characters."
  (when (plusp (length chars))
    (reduce (lambda (a b)
              (+ (* 10 a)
                 (- (char-code b) 48)))
            chars
            :initial-value 0)))

(defun make-keyword (str)
  (intern (string-upcase str) "KEYWORD"))

(defun normalize-note (list)
  (if (atom list)
      (make-keyword list)
      (if (= (length list) 2)
          (remove nil (list (make-keyword (car list)) (cadr list)))
          (make-keyword (car list)))))

;;; MML Grammar
;; Lacks support for:
;; IGNORED:
;;   V volume
;;   & ligado
;; ERRORS:
;;   ^ ligado
;;   , separate voices
;;   MML@ header
;;   ; terminator
;; multiple voices
;; direct notation
;; tranposition
;; loops
;; effects
;; triplets (no one except ZZT  supports this, apparently)
;; headers (for formats that do not use # for sharp)
;; multiline comments

;; Since extensions are not fully compatible with each other,
;; I will define a "Standard MML" grammar, and additional grammars
;; for variations of it.

;; syntaxes
;; --------
;; http://apocalyptech.com/linux/zzt/manual/langref.html#play
;; https://en.wikipedia.org/wiki/Music_Macro_Language#Modern_MML <- favoured
;; https://en.wikipedia.org/wiki/Music_Macro_Language (others)
;; http://woolyss.com/chipmusic/chipmusic-mml/ppmck_guide.php
;; https://github.com/Natureshadow/mmllib
;; http://www.mirbsd.org/htman/i386/man4/spkr.htm
;; http://jiggawatt.org/muzak/xpmck/manual.html
;; http://www.gamefaqs.com/boards/663843-petit-computer/63524945
;; http://wiki.mabinogiworld.com/view/MML (comma, semicolon and MML@ originate here?)

(defparameter *current-length* 4)

(defrule digit (character-ranges (#\0 #\9)))
(defrule number (+ digit) (:function integer-from-chars))

(defrule sharp (or #\+ #\#) (:lambda (prod) (substitute #\+ #\# prod)))
(defrule flat #\-)
(defrule sostenuto #\.)
(defrule ligado #\& (:constant nil))

(defrule whitespace (+ (or #\space #\tab #\newline ligado))
  (:constant nil))

(defrule comment (and #\; (* (not #\Newline)))
  (:constant nil))

(defrule note-length (and (* digit) (* sostenuto))
  (:destructure (n dots)
                (let ((num (or (integer-from-chars n)
                               *current-length*)))
                  (mapc (lambda (_)
                          (declare (ignore _))
                          (setf num (/ num 1.5)))
                        dots)
                  num)))
(defrule note (and (or #\A #\B #\C #\D #\E #\F #\G
                       #\a #\b #\c #\d #\e #\f #\g)
                   (? (or sharp flat)))
  (:lambda (prod) (normalize-note (text prod))))
(defrule rest (or #\P #\R #\p #\r)
  (:function make-keyword))
(defrule term (and (or note rest) (? note-length)))

(defrule octave-relative (or #\< #\>)
  (:lambda (prod) (list (make-keyword prod))))
(defrule octave (and (or #\O #\o) number)
  (:function normalize-note))
(defrule volume (and (or #\V #\v) number)
  (:function normalize-note))
(defrule tempo (and (or #\T #\t) number)
  (:function normalize-note))
(defrule length (and (or #\L #\l) note-length)
  (:lambda (prod)
    (let ((norm (normalize-note prod)))
      (setf *current-length* (cadr norm))
      norm)))
(defrule control (or octave-relative octave volume tempo length))

(defrule riff (+ (or term control whitespace comment)))

(defun parse-mml (mml)
  (let ((*current-length* 4))
    (parse 'riff mml)))
