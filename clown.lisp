
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload :png :silent t)
(ql:quickload :array-operations :silent t)
(ql:quickload :cl-collider :silent t)
(ql:quickload :bt-semaphore :silent t)
(ql:quickload :unix-opts :silent t)

(opts:define-opts
  (:name :help
         :description "Print this help message"
         :short #\h
         :long "help")
  (:name :winds
         :description "The number of times to wind the key"
         :short #\w
         :long "winds"
         :arg-parser #'parse-integer
         :default 0))

(defvar *args* (opts:get-opts))

(when (getf *args* :help)
  (opts:describe)
  (quit))

(setq *compile-verbose* nil)
(setq *compile-print* nil)

(load (compile-file "image-extract.lisp"))
(load (compile-file "musicbox.lisp"))

(in-package :aemb)

(defvar *clown-scale*
  '(-5 -3 0 2 4 5 7 9 11 12 14 16))

(defvar *clown-key* 278.2)

(format t "~%~%~%Starting the server...")

(setf sc:*s* (sc:make-external-server "localhost" :port 48800))
(sc:server-boot sc::*s*)

(sleep 1) ; This just lets the junk printed by the server print before we move on


(format t "Creating the synth...~%")

(in-package :sc-user)
(named-readtables:in-readtable :sc)
(defvar *tine-samples*
  (aops:generate
   (lambda (i)
     (let ((fpath (format nil "AudioFiles/Tine~d.wav" (1+ i))))
       (format t "Buffer ~d: ~s" i fpath)
       (sc:buffer-read fpath)))
   12
   :position))

(sc:defsynth tine (note)
  (let* ((sample (play-buf.ar 1 note 1.0 :act :free)))
    (sc:out.ar 0 [sample sample])))
(in-package :aemb)

(defvar *score*)
(defvar *sequencer*)
(defvar *loop-thread*)
(defvar *seq-state*)

(handler-case
    (progn

      (format t "Extracting the image...~%")

      (defun try-load-score ()
        (handler-case
            (progn
              (load "clown-score.lisp")
              (format t "loaded clown score~%")
              t)
          (sb-int:simple-file-error () nil)))

      (format t "Attempting to load the score...~%")
      (unless (try-load-score)
        ;; We don't have the score saved, so let's generate it

        (format t "Score not found. Generating the score from \"music-box-panel.png\"")
        (let ((tine-layout (extract-image "music-box-panel.png"))) ; Extract the image
          ;; Generate the score
          (setq *score* `(,(assoc 'loop-time tine-layout) (sequence . ,(create-sequence (cdr (assoc 'tines tine-layout))))))

          ;; Save the score for later
          (with-open-file (f "clown-score.lisp" :direction :output)
            (format f "(defvar *score*)~%(setq *score* '~S)" *score*))))

      (format t "Creating the sequencer~%")

      (setq *sequencer* (make-instance 'sequencer :time-stretch 0.012 :score *score*))

      (format t "Playing the composition~%")

      (multiple-value-bind (loop-thread sstate)
          ;; Start sequencer, returning loop-thread and sstate
          (play-sequence
           *sequencer*                        ; sequencer
           'tine                              ; synth
           (lambda (n)                        ; note value is the tine buffer
             (aref sc-user::*tine-samples* (1- n))))
        (handler-case
            (flet ((finish ()
                     (finish-sequence *sequencer* 5.0) ; Slow sequence over 5 seconds
                     (bt:join-thread loop-thread)))
              (do ((old-sstep nil sstep)
                   (sstep (sstep sstate) (sstep sstate))
                   (num (num sstate) (num sstate)))
                  ((and (listen) (char= (read-char) #\q))
                   (finish))
                (when (and old-sstep (not (eq sstep old-sstep)))
                  (format t "~d: ~d (~f -> ~f)~%" num (note sstep) (step-time sstep) (duration sstep))
                  (sleep 0.01))))
          (sb-sys:interactive-interrupt () (finish))))
      (format t "Complete~%")

      (sc:server-quit sc:*s*))
  
  (sb-sys:interactive-interrupt () (sc:server-quit sc:*s*)))
