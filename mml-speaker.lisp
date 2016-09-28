;;;; mml-speaker.lisp
;; (c} 2016 Sergi Reyner
;; MIT License
;; Work in progress!

(in-package #:mml-speaker)

;; requires beep (sudo apt install beep)

;;; "mml-speaker" goes here. Hacks and glory await!
(defconstant +12th-root-of-2+ 1.0594630943593)

(defparameter *debug* nil)
(defparameter *base-note* :a)
(defparameter *base-octave* 4)
(defparameter *base-freq* 440.0)
(defparameter *beep-program* "/usr/bin/beep")

;; the math is from:
;; http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
(defun distance (note &key (octave 4))
  (let ((notes '(:c :c+ :d :d+ :e :f :f+ :g :g+ :a :a+ :b))
        (octave-delta (* 12 (- octave *base-octave*))))
    (+ (- (position note notes)
          (position *base-note* notes))
       octave-delta)))

(defun freq (note &key (octave *base-octave*))
  "Rest is implemented by means of a freq of 1Hz."
  (if (member note '(:p :r))
      1
      (* *base-freq*
         (expt +12th-root-of-2+
               (distance note :octave octave)))))

(defun duration (length tempo)
  "Determines the time in ms from the note length and the tempo."
  (let* ((crotchet (/ 60.0 tempo))
         (whole (* 4000.0 crotchet)))
   (/ whole length)))


;; player
(defun beep (hz ms)
  (sb-ext:run-program *beep-program* `("-f" ,hz "-l" ,ms)))

(defun play (music)
  (loop :with octave := 4
     :with length := 4
     :with tempo := 120
     :for element :in music
     :for command := (car element)
     :for parameter := (cadr element)
     :do (progn (when *debug* (format t "~S ~S~%" command parameter))
                (ecase command
                  ((:A :A+ :B :C :C+ :D :D+ :E :F :F+ :G :G+ :P :R)
                   (beep (format nil "~A" (freq command :octave octave))
                         (format nil "~D" (duration (or parameter length)
                                                    tempo))))
                  (:L (setf length parameter)
                      (setf *current-length* length))
                  (:O (setf octave parameter))
                  (:< (decf octave))
                  (:> (incf octave))
                  (:T (setf tempo parameter))
                  (:V #|ignore|#)))))

(defun parse-mml (mml) (parse 'riff mml))
(defun play-mml (mml) (play (remove nil (parse-mml mml))))


;; some helper functions
(defun integer-from-chars (chars)
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


;; grammar
;; https://en.wikipedia.org/wiki/Music_Macro_Language#Modern_MML
;; lacks support for:
;; & ligado (will ignore)
;; ^ ligado (will error)
(defparameter *current-length* 4)

(defrule digit (character-ranges (#\0 #\9)))
(defrule number (+ digit) (:function integer-from-chars))

(defrule sharp (or #\+ #\#) (:lambda (prod) (substitute #\+ #\# prod)))
(defrule flat #\-)
(defrule sostenuto #\.)
(defrule ligado #\& (:constant nil))

(defrule whitespace (+ (or #\space #\tab #\newline ligado))
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
(defrule length (and (or #\L #\l) number)
  (:lambda (prod)
    (let ((norm (normalize-note prod)))
      (setf *current-length* (cadr norm))
      norm)))
(defrule control (or octave-relative octave volume tempo length))

(defrule riff (+ (or term control whitespace)))


;; demo code
(defun demo-simple ()
  (play-mml "a8r1 >b8r1<<c8"))

(defun demo-song ()
  "All meine Entchen, from the Wikipedia page, 2 octaves higher."
  (play-mml "o4l4t120 cdefg2g2 aaaag2 aaaag2 ffffe2e2 ddddc1"))

(defun demo-gakkou-kouka ()
  (play-mml "t160 o3l4 ed8ce8 gg8er8 aa8>c<a8 g2r aa8ga8 >cc8d<r8 ee8de8 c2r dd8dd8 dd8dr8 ed8ef8 g2r aa8ga8 >cc8<ar8 >dc8de8 d2<r >ee8dc8< ab8>cc8< gg8ea8 g2r >cc8<ge8 cd8ea8 gg8de8 c2r"))

(defun demo-key-of-twilight ()
  (play-mml "l8bf+b>c+de16d16c+<ab>f+edl16c+dedl8c+agf+edc+dec+b16c+16dc+<ba>c+<bag>f+edl16c+dedl8c+<ag>f+edl16c+dedl8eagf+edc+dec+b16c+16dc+<ba>c+<bab3b3"))

(defun demo-fake-wings ()
  (play-mml "t69>dl8fag4egfdecda4d4fag4egfdeeed4>d4dc4<aca+agge4fga4fag4egfdeeed4cd4a4<g>e<g>gf4ede2<<d2a2l8a+ced>f4<d2a2a+egdf4>fa+<f>d<a>fd4d4<a4f>d<g4>df4<ec4a+>d<eg<ga>edf4fd2d>d4<d>ce<g"))

(defun demo-axel-f ()
  (play-mml "f+8r8a8f+8f+16b8f+8e8f+8r8>c+8<f+8f+16>d8c+8<a8f+8>c+8f+8<f+16e8e16c+8g+8f+4r2f+8r8a8f+8f+16b8f+8e8f+8r8>c+8<f+8f+16>d8c+8<a8f+8>c+8f+8<f+16e8e16c+8g+8f+4r1 <<f+8r8>f+8<e8>e16<c+8>c+8<e8f+8r8>f+8r8<c+16>c+8e8f+8<d8r8>d8<e8>e16<c+8e8f+8>f+4r4r16e16c+8<b8a8f+8r8>f+8<e8>e16<c+8>c+8<e8f+8r8>f+8r8<c+16>c+8e8f+8<d8r8>d8<e8>e16<c+8e8f+8>f+4"))

(defun demo-butterfly-samurai ()
  (play-mml "t134g+l8ag+f+e4rg+4abg+e4rf+4g+f+d+b4f+l16g+rf+rerc+8r4<g+rg+r>c+8rbc+rd+re8r4e8rd+erf+rg+8r2f+8rl8f+d+b4d+4ef+ed+c+r4r8g+r16g+b>c+r16<g+b>c+l16erc+r<brg+8r4rf+8rf+rg+rb8rg+rerf+8g+8f+rerc+4"))

(defun demo-darude-sandstorm ()
  (play-mml "t120l16<eeeee8eeeeeee8aaaaaaa8ggggggg8ddeeeee8eeeeeee8aaeeeee8eeeeeee8aaeeeee8eeeeeee8aaaaaaa8ggggggg8ddeeeee8eeeeeee8aaeeeee8eeeeeee8aa"))

(defun demo-doremi ()
  (play-mml "t140c4d8e4c8e4c4e2d4e8f8f8e8d8f1e4f8g4e8g4e4g2f4g8a8a8g8f8a1g4c8d8e8f8g8a1a4d8e8f+8g8a8b1b4e8f+8g+8a8b8>c2<b8a+8a4f4b4g4>c1"))

(defun demo-digimon-theme ()
  (play-mml "t112l8cgf+rl16ccg8f+8g8ccg8f+8g8a+ra+r4rc8g8f+8r8ccg8f+8g8ccg8f+8g8a+ra+r4rg2g8a12l32ab8b>d8d16dc8cb8c4<a4>c2c8<b8b16b>c8cd8d16dc8cl8bgr4r8<cgf+rl16ccg8f+8g8ccg8f+8g8a+ra+"))

(defun demo-gallifrey ()
  (play-mml "a2ab8>c<gg>c8c8d16c16<bbb8baaa8>eccd8e8eccc8d16c16<b8bbb8baaa8>ge2ggc2gg<a2b8>c8d16c16<b8g2e8g8a2"))

(defun demo-guile-theme-sf2 ()
  (play-mml "t135v127f8l16fe8ef2e8f8fe8ef2e8fl8efdgg16fecfl16fe8ef2e8f8fe8ef2e8fl8efdgg16fec>d2def16ga4dc4a+a16a+e4fgce16ga+4ca4g4d2o4v105l4dddc<a+a+a+aggaa>dddc<a+a+a+aggaadddd<a+a+a+a+>ccccffeedd"))

(defun demo-blue-dabadee ()
  (play-mml "T120o6l8v127a#dga#>c<faa#4ga#>dd#<g>dc<a#dga#>c<faa#4ga#>dd#<g>dc<a#dga#>c<faa#4ga#>dd#<g>dc<a#dga#acfg4o4g2f2d#2c2g2f2d#2c2g2f2d#2c2g2f2d#4o3g2f2d#2c2g2f2d#2c2g2f2d#2c2g2f2d#4"))

(defun demo-kirby-victory-theme ()
  (play-mml "t130>f16g16a16b16a16b16>c8<g16e8f16g16a16b16a16b16>c8<e8f16g16a16b16a16b16>c8<g16e8>g16f8e16d8e16c8>c8d8d16g8g16c8c8g16f8f16g8g16c8g16c8d8d16g8g16c8<c16c8>c16<f16g16a16b16a16g16>c8<g16c8"))

(defun demo-nyan-cat ()
  (play-mml "t120v127o6f+8g+8d+16d+8<b16>d16c+16<b8b8>c+8d8d16c+16<b16>c+16d+16f+16g+16d+16f+16c+16d+16<b16>c+16<b16>d+8f+8g+16d+16f+16c+16d+16<b16>d16d+16d16c+16<b16>c+16d8<b16>c+16d+16f+16c+16d+16c+16<b16>c+8<b8"))

(defun demo-pacman ()
  (play-mml "t105b16>b16f+16d+16b32f+16d+8c16>c16<g16e16>c32<g16e8<b16>b16f+16d+16b32f+16d+8d+32e32f16f32f+32g16g32g+32a16b16o1b8>b16<b8>b16c8>c16<c8>c16<<b8>b16<b8>b16f+8g+8a+8b16"))

(defun demo-the-riddle ()
  (play-mml "t105F+G+AL8ABAG+F+EE4F+G+G+4F+G+A4ABAG+F+EF+4F+EE4F+G+A4ABAG+F+EE4F+G+G+4F+G+A4ABAG+F+EF+4F+EE4>C+EDC+<BAF+4EF+F+4R4RF+ARL4>C+C+<BL8RG+4G+BAG+F+4A4R>C+DC+<B>DC+4<RAG+4B>C+L4C+C+<BR8G+L8G+BAGG+F+EC+2"))
