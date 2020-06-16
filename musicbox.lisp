(in-package #:cl-user)

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

(defgeneric next-step (sequencer)
  (:documentation "Gets the next step from a sequencer"))

(defgeneric loop-time (sequencer)
  (:documentation "Gets the loop time of the sequencer"))

(defgeneric finished-p (sequencer)
  (:documentation "Returns whether the sequencer is finished playing"))

(defgeneric end-sequence (sequencer)
  (:documentation "Ends a sequence"))

(defgeneric note (step)
  (:documentation "Gets the next note from a sequence step"))

(defgeneric step-time (step)
  (:documentation "Gets the note time for the current step"))

(defgeneric last-note-p (step)
  (:documentation "Returns whether the step is the last note to play"))

(defgeneric loop-end-p (step)
  (:documentation "Returns whether the step is the end of the loop"))

(defclass sequencer ()
  ((sequence
    :initarg :sequence
    :accessor seq
    :documentation "The sequence")
   (next-step
    :documentation "The car of next-step will be the next step of the sequence")
   (loop-time
    :initarg :loop-time
    :documentation "The loop time of the sequence")
   (last-note-dur
    :initarg :last-note-dur
    :reader last-note-dur
    :documentation
    "The duration of the last note, which must be (+ (- loop-time (last-note-time sequence)) (first-note-time sequence))")
   (time-stretch
    :initform 1.0
    :initarg :time-stretch
    :accessor time-stretch
    :documentation "The time strecth of the sequencer (how much to stretch the times defined by the sequence")
   (finished-p
    :initform nil
    :accessor finished-p
    :documentation "Whether the sequence is complete")))

(defmethod loop-time ((seq sequencer))
  (* (time-stretch seq) (slot-value seq 'loop-time)))

(defmethod initialize-instance :after ((seq sequencer) &key score)
  (when score
    (let* ((loop-time (cdr (assoc 'loop-time score)))
           (sequence (cdr (assoc 'sequence score)))
           (total-note-time (reduce #'+ (mapcar 'car sequence))))
      (setf (seq seq) sequence)
      (setf (slot-value seq 'loop-time) loop-time)
      (setf (slot-value seq 'last-note-dur) (+ (- loop-time total-note-time) (caar sequence)))))
  (setf (slot-value seq 'next-step) (seq seq)))

(defmethod next-step ((seq sequencer))
  (let* ((next (slot-value seq 'next-step))
         (next-next (cdr next))
         (note (cdar next))
         (step-time (* (time-stretch seq) (caar next)))
         (duration (* (time-stretch seq)
                      (if next-next
                          (caar next-next)
                          (last-note-dur seq))))
         (loop-end-p (not (cdr next)))
         (last-note-p (finished-p seq)))
    (setf (slot-value seq 'next-step)
          (if loop-end-p
              (seq seq)
              (cdr next)))
    (make-instance 'seq-step
                   :note note
                   :step-time step-time
                   :duration duration
                   :loop-end-p loop-end-p
                   :last-note-p last-note-p)))

(defmethod end-sequence ((seq sequencer))
  (setf (finished-p seq) t))

(defclass seq-step ()
  ((note
    :initarg :note
    :accessor note
    :documentation "The note number that defines the note to play")
   (step-time
    :initarg :step-time
    :accessor step-time
    :documentation "The time for this step to play relative to the previous step")
   (duration
    :initarg :duration
    :accessor duration
    :documentation "The duration of this step (the time between this one and the next one).")
   (loop-end-p
      :initarg :loop-end-p
      :accessor loop-end-p
      :documentation "Whether this step is the end of the loop")
   (last-note-p
    :initarg :last-note-p
    :accessor last-note-p
    :documentation "Whether this step is the last of the sequence")))

(defclass seq-state ()
  ((sstep
    :initarg :step
    :initform nil
    :accessor sstep
    :documentation "The current step")
   (num
    :initarg :num
    :initform 0
    :accessor num
    :documentation "The step number")))

(defmethod update-seq-state ((state seq-state) step)
  (setf (sstep state) step)
  (incf (num state)))

(defvar *min-note-sleep-time* 0.05)

(defun play-sequence (sequencer synth scale-map)
  (let ((state (make-instance 'seq-state)))
      (labels ((schedule-note (offset this-loop-time)
                 (let* ((step (next-step sequencer))
                        (note (note step))
                        (tlt (+ this-loop-time (step-time step)))
                        (note-time (+ offset tlt))
                        (sleep-time (- note-time (sc:now))))
                   (sc:at note-time (sc:synth synth :note (funcall scale-map note)))
                   (update-seq-state state step)
                   (when (>= (duration step) *min-note-sleep-time*) (sleep sleep-time))
                   (cond ((last-note-p step)
                          this-loop-time)
                         ((loop-end-p step)
                          (loop-time sequencer))
                         (t (schedule-note offset tlt)))))
               (scheduling-loop ()
                 (do* ((offset (+ 0.1 (sc:now)) (+ offset wait-time))
                       (wait-time (schedule-note offset 0) (schedule-note offset 0)))
                      ((finished-p sequencer))
                   )))
        (values (bt:make-thread #'scheduling-loop :name "loop") state))))
