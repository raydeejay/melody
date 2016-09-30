;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:mml-speaker)

;;; "mml-speaker" goes here. Hacks and glory await!
(defconstant +12th-root-of-2+ 1.0594630943593)

(defparameter *debug* nil)
(defparameter *base-note* :a)
(defparameter *base-octave* 4)
(defparameter *base-freq* 440.0)
(defparameter *beep-program* "/usr/bin/beep")

;; the math is from:
;; http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
(defun translate-note (note octave)
  "Converts a sharp note into its non-qualified equivalent. Note that
  C- and B+ are particular cases that cross octaves."
  (let* ((table `((:b+ . :c) (:c- . :b)
                  (:d- . :c+) (:e- . :d+) (:f- . :e)
                  (:g- . :f+) (:a- . :g+) (:b- . :a+)))
         (note note)
         (octave octave)
         (entry (assoc note table)))
    (if (null entry)
        (values note octave)
        (values (cdr entry)
                (case note
                  (:c- (1- octave))
                  (:b+ (1+ octave))
                  (otherwise octave))))))

(defun distance (note &key (octave 4))
  (multiple-value-bind (note octave)
      (translate-note note octave)
    (let ((notes '(:c :c+ :d :d+ :e :f :f+ :g :g+ :a :a+ :b))
          (octave-delta (* 12 (- octave *base-octave*))))
      (+ (- (position note notes)
            (position *base-note* notes))
         octave-delta))))

(defun freq (note &key (octave *base-octave*))
  "Rest is implemented by means of a freq of 1Hz."
  (* *base-freq*
     (expt +12th-root-of-2+
           (distance note :octave octave))))

(defun duration (length tempo)
  "Determines the time in ms from the note length and the tempo."
  (let* ((crotchet (/ 60.0 tempo))
         (whole (* 4000.0 crotchet)))
   (/ whole length)))


;; player
(defun beep-speaker (hz ms)
  (sb-ext:run-program *beep-program* `("-f" ,hz "-l" ,ms)))

(defun beep (source pitch ms)
  (al:source source :pitch (float pitch))
  (al:source-play source)
  (sleep (/ ms 1000.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;
(defun play-speaker (music)
  (loop :with octave := 4
     :with length := 4
     :with tempo := 120
     :for element :in music
     :for command := (car element)
     :for parameter := (cadr element)
     :do (progn (when *debug* (format t "~S ~S~%" command parameter))
                (ecase command
                  ((:A :A+ :B :C :C+ :D :D+ :E :F :F+ :G :G+)
                   (beep (format nil "~A" (freq command :octave octave))
                         (format nil "~D" (duration (or parameter length)
                                                    tempo))))
                  ((:P :R)
                   (sleep (/ (duration (or parameter length) tempo) 1000.0)))
                  (:L (setf length parameter)
                      (setf *current-length* length))
                  (:O (setf octave parameter))
                  (:< (decf octave))
                  (:> (incf octave))
                  (:T (setf tempo parameter))
                  (:V #|ignore|#)))))

(defun play (music)
  (let ((*base-freq* 1)
        data format size rate)
    (alut:with-init
      (multiple-value-bind (snd for siz fre)
          ;; (alut:load-memory-from-file "/home/raydj/piano/a4-small.wav")
          ;; (alut:load-memory-waveform :sine 440.0 1.0 0.500)
          (alut:load-memory-from-file "/home/raydj/lisp/mml-speaker/sega2.wav")
        (setf data snd
              format for
              size siz
              rate (truncate fre))))
    (alc:with-device (device)
      ;; Here it is appropriate to check that the device is actually opened.
      ;; GET-ERROR for example.
      (alc:with-context (context device)
        (alc:make-context-current context)
        ;; Again: GET-ERROR.
        (al:with-buffer (buffer)
          (al:with-source (source)
            (al:buffer-data buffer format data size rate)
            (al:source source :buffer buffer)
            (al:source source :position #(1 1 1))
            (al:source source :velocity #(0 0 0))
            (al:listener :position #(1 1 1))
            (al:listener :orientation #(0 0 0 0 0 0))
            ;; Let the music play...
            (al:source source :gain 0.05)
            (unwind-protect
                 (loop :with octave := 4
                    :with length := 4
                    :with tempo := 120
                    :for element :in music
                    :for command := (car element)
                    :for parameter := (cadr element)
                    :do (progn (when *debug* (format t "~S ~S~%" command parameter))
                               (ecase command
                                 ((:A :B :C :D :E :F :G
                                      :A+ :B+ :C+ :D+ :E+ :F+ :G+
                                      :A- :B- :C- :D- :E- :F- :G-)
                                  (beep-openal source
                                               (freq command :octave octave)
                                               (duration (or parameter length) tempo)))
                                 ((:P :R)
                                  (sleep (/ (duration (or parameter length) tempo)
                                            1000.0)))
                                 (:L (setf length parameter)
                                     (setf *current-length* length))
                                 (:O (setf octave parameter))
                                 (:< (decf octave))
                                 (:> (incf octave))
                                 (:T (setf tempo parameter))
                                 (:V #|ignore|#))))
              (al:source-stop source))))))))
