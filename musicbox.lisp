
(ql:quickload :cl-collider)

(load "notes.lisp")

;(defvar *s* (make-external-server "localhost" :port 57110 :just-connect-p t))
(setf sc:*s* (sc:make-external-server "localhost" :port 48800))
(sc:server-boot sc::*s*)

(defun equal-temp (n)
  (expt 2.0 (/ n 12.0)))

(defun create-score (staff-times)
  (reverse (do ((score nil)
        (last-time 0))
       ((notany #'cdr staff-times) score)
     (let* ((next-time most-positive-double-float)
            (next-bar))
       (pop (cdr                               ; This is the next time
             (dolist (bar staff-times next-bar) ; Search the staff for the next time
               ;(break)
               (when (and (consp (cdr bar)) (< (cadr bar) next-time))
                 (setq next-bar bar)
                 (setq next-time (cadr bar))))))
       (setq score (cons `(,(- next-time last-time) . ,(car next-bar)) score))
       (setq last-time next-time)))))

(defvar *base-note* (* 440 (equal-temp 3))) ;; Middle C

(defvar *scale-map*
  '((-8 . -15)
    (-7 . -13)
    (-6 . -12)
    (-5 . -10)
    (-4 . -8)
    (-3 . -7)
    (-2 . -5)
    (-1 . -3)
    (0 . -1)
    (1 . 0)
    (2 . 2)
    (3 . 4)
    (4 . 5)
    (5 . 7)
    (6 . 9)
    (7 . 11)
    (8 . 12)))

(defvar *note-map*
  (reverse (do ((nmap nil)
                (note 1 (1+ note)))
               ((> note 12) nmap)
             (setq nmap
                   (cons
                    (cons
                     note
                     (* *base-note* (equal-temp (cdr (assoc (- note 9) *scale-map*)))))
                    nmap)))))

(sc:defsynth tine (freq)
  (let* ((env (sc:env-gen.kr (sc:env [1 0] [3]) :act :free))
         (impls (sc:impulse.ar 0 0 env))
         (res (sc:dyn-klank.ar [[freq] nil [1]] impls)))
    (sc:out.ar 0 [res res])))

(defun play-score (score stretch note-map)
  (when (consp score)
    (format t "~S~%" score)
    (let* ((time (* stretch (caar score)))
           (note (cdar score))
           (freq (cdr (assoc note note-map))))
      (format t "~d/~f~%" note freq)
      (sc:at (+ 1 (sc:now)) (sc:synth 'tine :freq freq))
      (when (cdr score)
        (format t "next~%")
        (let ((next-time (+ (now) (* stretch (caadr score)))))
          (sc:callback next-time #'play-score (cdr score) stretch notemap))))))
