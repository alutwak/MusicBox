
(defpackage #:audio-electric-musicbox
  (:documentation "Plays a music box")
  (:nicknames :aemb :ae-musicbox)
  (:use #:common-lisp #:aeie #:sc-user)
  (:export
   :init-music-box
   :play-score
   :note->freq
   :play-sequence

   ;; sequencer
   :sequencer
   :loop-time
   :time-stretch
   :finished-p
   :end-sequence))

(in-package :aemb)

(defmacro init-music-box ()
  (setf sc:*s* (sc:make-external-server "localhost" :port 48800))
  (sc:server-boot sc::*s*))

(defun equal-temp (base n)
  "Returns the equal temperment frequency for the nth step above (or below) the base note."
  (* base (expt 2.0 (/ n 12.0))))

;(defvar *base-note* (* 440 (equal-temp 3))) ;; Middle C

(defvar *ionian*
  '(0 2 4 5 7 9 11))

(defvar *dorian*
  '(0 2 3 5 7 9 10))

(defvar *phrygian*
  '(0 1 3 5 7 8 10))

(defvar *lydian*
  '(0 2 4 6 7 9 11))

(defvar *mixolydian*
  '(0 2 4 5 7 9 10))

(defvar *aolian*
  '(0 2 3 5 7 8 10))

(defvar *locrian*
  '(0 1 3 5 6 8 10))

(defstruct (scale (:constructor make-scale (key map)))
  key
  map)

(defstruct (note (:constructor make-note (num oct)))
  num
  oct)

(defun note->freq (note scale)
  (let* ((slen (length (scale-map scale)))
         (n (nth (mod (1- note) slen) (scale-map scale)))
         (oct (floor (/ note slen))))
    (when n
      (equal-temp (scale-key scale) (+ n (* 12 oct))))))

(defun play-score (score synth stretch scale-map loop-p)
  ""
  (labels ((schedule-note (sequence offset)
             (let* ((note (cdar sequence))
                    (play-time (+ offset (* stretch (caar sequence)))))
               (sc:at play-time (sc:synth synth :note (funcall scale-map note)))
               (if (cdr sequence)
                   (schedule-note (cdr sequence) play-time)
                   play-time)))
           (scheduling-loop ()
             (format t "scheduling loop~%")
             (let ((loop-time (* stretch (cdr (assoc 'loop-time score)))))
               (format t "looping with loop time: ~f~%" loop-time)
               (do* ((offset (+ 0.1 (sc:now)) (+ offset loop-time))
                     (keep-playing t (car loop-p)))
                    ((not keep-playing) offset)
                 (schedule-note (cdr (assoc 'sequence score)) offset)
                 (sleep loop-time)
                 (format t "keep looping? ~S~%" loop-p)))))
    (bt:make-thread #'scheduling-loop :name "loop")))
