;(declaim (optimize (debug 3)))

;; (ql:quickload "png")
;; (ql:quickload :array-operations)

(in-package #:cl-user)

(defpackage #:audio-electric-image-extract
  (:documentation "Extracts notes from an image")
  (:nicknames :aeie :ae-image-extract)
  (:use #:common-lisp)
  (:export
   :extract-image
   :create-sequence
   :loop-time
   :tines
   :sequence))

(in-package #:aeie)

(defclass element ()
  ((mass :initform 0
         :initarg :mass
         :reader mass
         :writer (setf mass)
         :documentation "The mass of the element")
   (belongs-to :initform nil
               :reader belongs-to
               :writer (setf belongs-to)
               :documentation
               "A list, the car of which points to the owning higher-order cluster of this cluster")))

(defclass pixel (element)
  ((moment :initform '(0 0)
           :initarg :moment
           :reader moment
           :writer (setf moment)
           :documentation "The location of the pixel. This location is itself a moment.")))

(defgeneric pspace (element)
  (:documentation "Returns the element space of an element"))

(defgeneric (setf pspace) (value element)
  (:documentation "Sets the element space of an element"))

(defgeneric print-element (element)
  (:documentation "Prints the state of an element"))

(defmethod pspace ((element pixel))
  (moment element))

(defgeneric event-time (element)
  (:documentation "The \"time\" of an element in the image"))

(defmethod event-time ((el element))
  (cadr (moment el)))

(defmethod print-element ((element pixel))
  (let ((mom (moment element))
        (mass (mass element))
        (bt (car (belongs-to element))))
    (format t "~S [~d]: " mom mass)
    (if bt (print-element bt) (format t "~%"))))

;;;=========================== pixel-space ======================================

(defstruct (pixel-space (:constructor make-pixel-space (&optional left right top bottom)))
  (left most-positive-fixnum)
  (right 0)
  (top most-positive-fixnum)
  (bottom 0))

(defun init-pixel-space ()
  (make-pixel-space (sqrt most-positive-double-float) 0 (sqrt most-positive-double-float) 0))

(defun pixel-space->list (ps)
  `(,(pixel-space-left ps) ,(pixel-space-right ps) ,(pixel-space-top ps) ,(pixel-space-bottom ps)))

(defun calc-pixel-space (elements)
  (let ((bottom (reduce #'max (mapcar (lambda (p) (car (moment p))) elements)))
        (top (reduce #'min (mapcar (lambda (p) (car (moment p))) elements)))
        (right (reduce #'max (mapcar (lambda (p) (cadr (moment p))) elements)))
        (left (reduce #'min (mapcar (lambda (p) (cadr (moment p))) elements))))
    (make-pixel-space left right top bottom)))

(defmacro expand-pixel-space (ps pixel)
  `(let ((i (car (moment ,pixel)))
         (j (cadr (moment ,pixel))))
     (setf (pixel-space-left ,ps) (min (pixel-space-left ,ps) j))
     (setf (pixel-space-right ,ps) (max (pixel-space-right ,ps) j))
     (setf (pixel-space-top ,ps) (min (pixel-space-top ,ps) i))
     (setf (pixel-space-bottom ,ps) (max (pixel-space-bottom ,ps) i))))

(defun pixel-space-width (ps)
  (1+ (- (pixel-space-right ps) (pixel-space-left ps))))

(defun pixel-space-height (ps)
  (1+ (- (pixel-space-bottom ps) (pixel-space-top ps))))

(defun pixel-space-area (ps)
  (* (pixel-space-width ps) (pixel-space-height ps)))

(defun pixel-space-length (ps)
  (max (pixel-space-width ps) (pixel-space-height ps)))

(defun pixel-space-center (ps)
  `(,(/ (+ (pixel-space-top ps) (pixel-space-bottom ps)) 2.0)
    ,(/ (+ (pixel-space-left ps) (pixel-space-right ps)) 2.0)))


;;;================== Image importing ======================================

(defun get-image (path)
  (let ((im-3d (png:decode-file path)))
    (aops:reshape im-3d (butlast (array-dimensions im-3d)))))

(defun get-nonzero-pixels (image thresh)
  (aops:reduce-index
      (lambda (&optional res pixel)
        (if pixel
            (cons pixel res)
            res))
      (i j)
    (let ((val (aref image i j)))
      (if (> val thresh)
          (make-instance 'pixel :moment (list i j) :mass (float (/ val 256)))
          nil))))

;;;================== Cluster Base Class ===================================

(defclass cluster (element)
  ((sum :initarg :sum
        :initform 0
        :reader sum
        :writer (setf sum)
        :documentation
        "The weighted sum of all the locations in the cluster. Together with the mass, this defines the cluster's moment")
   (nelem :initarg :nelem
          :initform 0
          :reader nelem
          :writer (setf nelem)
          :documentation "The number of elements in the cluster")
   (pspace :initarg :pspace
           :initform nil
           :documentation "The element space of this cluster, which defines the space in which the bounds of the cluster")))

(defmethod initialize-instance :after ((element cluster) &key pslist)
  (when pslist
    (setf (pspace element) (apply #'make-pixel-space pslist))))

(defmethod pspace ((element cluster))
  (slot-value element 'pspace))

(defmethod (setf pspace) (value (element cluster))
  (setf (slot-value element 'pspace) value))

(defgeneric zero-cluster (cluster)
  (:documentation "Zero out a cluster to initialize it for the update cycle."))

(defgeneric add-element (cluster element)
  (:documentation "Add an element to a cluster"))

;; (defgeneric moment (cluster)
;;   (:documentation "Returns the weighted center of the cluster"))

(defgeneric height (cluster)
  (:documentation "Returns the height of the cluster"))

(defgeneric width (cluster)
  (:documentation "Returns the width of the cluster"))

(defgeneric area (cluster)
  (:documentation "Returns the total area of the rectangular space containing the cluster"))

(defgeneric density (cluster)
  (:documentation "Returns the total density of the cluster"))

(defmethod density ((cluster cluster))
  (/ (slot-value cluster 'nelem) (area cluster)))

(defgeneric dist (cluster object)
  (:documentation "Returns the distance between a cluster and another object"))

(defgeneric split (cluster)
  (:documentation "Splits a cluster into two, modifying the given cluster and returning a new cluster"))

;;; ================= Pixel Cluster =========================================

(defclass pixel-cluster (cluster)
  ((sum :initform '(0 0)
        :reader sum
        :writer (setf sum))
   (pspace :initform (init-pixel-space))))

(defmethod zero-cluster ((cluster pixel-cluster))
  (setf (sum cluster) '(0 0))
  (setf (mass cluster) 0)
  (setf (nelem cluster) 0)
  (setf (pspace cluster) (init-pixel-space))
  cluster)

(defun modify-pixel-cluster (cluster pixel fn)
  "High-order function to modify the moment values of a pixel cluster"
  (let ((mass (mass pixel)))
    (setf (sum cluster)
          (mapcar (lambda (c-s p-l)
                    (funcall fn c-s (* mass p-l)))
                  (sum cluster)
                  (moment pixel)))
    (setf (mass cluster)
          (funcall fn
                   (mass cluster)
                   mass))))

(defmethod add-element ((cluster pixel-cluster) (element pixel))
  (modify-pixel-cluster cluster element #'+)
  (incf (nelem cluster))
  (expand-pixel-space (pspace cluster) element)
  cluster)

(defmethod moment ((cluster pixel-cluster))
  (if (= (mass cluster) 0)
      `(,most-positive-double-float ,most-positive-double-float)
      (mapcar (lambda (l)
                (/ l (mass cluster)))
              (sum cluster))))

(defmethod height ((cluster pixel-cluster))
    (pixel-space-height (pspace cluster)))

(defmethod width ((cluster pixel-cluster))
  (pixel-space-width (pspace cluster)))

(defmethod area ((cluster pixel-cluster))
  (pixel-space-area (pspace cluster)))

(defmethod dist ((cluster pixel-cluster) (object list))
  (if (= (mass cluster) 0)
      most-positive-double-float
      (sqrt
       (reduce
        #'+
        (mapcar
         (lambda (a b)
           (let ((dif (- a b)))
             (* dif dif)))
         (moment cluster)
         object)))))

(defmethod dist ((cluster pixel-cluster) (object pixel))
  (dist cluster (moment object)))

(defmethod dist ((cluster pixel-cluster) (object pixel-cluster))
  (dist cluster (moment object)))

(defmethod split ((cluster pixel-cluster))
  (let* ((m (moment cluster))
         (w (width cluster))
         (h (height cluster))
         (split
          (cond ((> w h)
                 `((,(car m) ,(pixel-space-left (pspace cluster)))
                   (,(car m) ,(pixel-space-right (pspace cluster)))))
                (t
                 `((,(pixel-space-top (pspace cluster)) ,(cadr m))
                   (,(pixel-space-bottom (pspace cluster)) ,(cadr m)))))))
    (format t "Splitting cluster ~S into ~S~%" m split)
    ;; Set this cluster to one of the new locations
    (add-element (zero-cluster cluster) (make-instance 'pixel :moment (car split) :mass 1))

    ;; Find an empty cluster and set that one to the other new location. This is what we return
    (make-instance 'pixel-cluster :sum (cadr split) :mass 1)))

(defmethod print-element ((cluster pixel-cluster))
  (let ((m (moment cluster))
        (n (nelem cluster))
        (d (* 100 (density cluster)))
        (ps (pspace cluster)))
    (format t "~S: ~d pix, ~f%~%" m n d)
    (apply #'format (append '(t "    pspace: [<~d ~d> ^~d ~dv]~%") (pixel-space->list ps)))
    (format t "    bar: ")
    (print-element (car (belongs-to cluster)))))

(defun element-stats (elements)
  "Prints all of the clusters in a collection of clusters"
  (mapc #'print-element elements)
  (format t "number of elements: ~d~%" (length elements)))

(defun draw-pixel-clusters (clusters image path)
  (let ((im-out (png:make-image (png:image-height image) (png:image-width image) 3)))
    (aops:each-index (i j k)
      (setf (aref im-out i j k) (aref image i j)))
    (dolist (clstr clusters)
      (let* ((loc (mapcar #'round (moment clstr)))
             (i (car loc))
             (j (cadr loc)))
        (when (and loc (apply #'array-in-bounds-p (cons image loc)))
          (mapc (lambda (k) ; Set G and B channels to 0
                  (setf (aref im-out i j k) 0))
                '(1 2))
          (setf (aref im-out i j 0) 255) ; Set R channel to 256
          )))
    (png:encode-file im-out path)))

;;; =============================== Bar =========================================================

;; Bar is a vertically-oriented cluster
(defclass bar (cluster)
  ((order :initform 0
          :reader order
          :writer (setf order))))

(defmethod zero-cluster ((cluster bar))
  (setf (sum cluster ) 0)
  (setf (mass cluster) 0)
  (setf (nelem cluster) 0)
  (setf (pspace cluster) (init-pixel-space))
  cluster)

(defmethod add-element ((cluster bar) (element pixel-cluster))
  (let ((mass (mass element)))
    (incf (sum cluster) (* mass (car (moment element))))
    (incf (mass cluster) mass))
  (incf (nelem cluster))
  (expand-pixel-space (pspace cluster) element)
  cluster)

(defmethod moment ((cluster bar))
  (if (= (mass cluster) 0)
      most-positive-double-float
      (/ (sum cluster) (mass cluster))))

(defmethod height ((cluster bar))
  (pixel-space-height (pspace cluster)))

(defmethod width ((cluster bar))
  1)

(defmethod area ((cluster bar))
  (height cluster))

(defmethod dist ((cluster bar) (n number))
  (abs (- (moment cluster) n)))

(defmethod dist ((cluster bar) (object bar))
  (dist cluster (moment object)))

(defmethod dist ((cluster bar) (object pixel-cluster))
  (dist cluster (car (moment object))))

(defmethod split ((cluster bar))
  (let* ((m (moment cluster))
         (bottom (pixel-space-bottom (pspace cluster)))
         (top (pixel-space-top (pspace cluster)))
         (split `(,(/ (+ m bottom) 2) ,(/ (+ m top) 2))))
    (format t "Splitting cluster ~S into ~S~%" m split)
    (add-element (zero-cluster cluster) (make-instance 'pixel-cluster :sum `(,(car split) 0) :mass 1))
    (make-instance 'bar :sum (cadr split) :mass 1)))

(defmethod print-element ((cluster bar))
  (let ((mom (moment cluster))
        ;(density (density cluster))
        (nelem (nelem cluster))
        (order (order cluster))
        ;(top (pixel-space-top (pspace cluster)))
        ;(bottom (pixel-space-bottom (pspace cluster)))
        )
    (format t "~d: (~f) [~d]~%" order mom nelem )))

;;; =============================== K-Means Higher Order Functions ===============================

(defun init-pixel-clusters (pixels k)
  (let ((ps (calc-pixel-space pixels)))
    (list (make-instance 'pixel-cluster :sum (pixel-space-center ps) :mass 1 :pslist (pixel-space->list ps)))))

(defun init-bar-clusters (pclusters k)
  (let* ((ps (calc-pixel-space pclusters))
         (bar-height (/ (pixel-space-height ps) k))
         (bars nil))
    (do ((bar-center (+ (pixel-space-top ps) (/ bar-height 2)) (+ bar-center bar-height)))
        ((> bar-center (pixel-space-bottom ps)) bars)
      (setq bars (cons (make-instance 'bar
                                      :sum bar-center
                                      :mass 1 
                                      :pslist (list bar-center bar-center bar-center bar-center))
                       bars)))))

(defun init-clusters (elements &optional (k 1))
  (cond ((eq (type-of (car elements))  'pixel)
         (init-pixel-clusters elements k))
        ((eq (type-of (car elements)) 'pixel-cluster)
         (init-bar-clusters elements k))))

(defun reassign-elements (elements clusters)
  (let ((nchanged 0))
    (dolist (el elements nchanged)
      (let ((crnt-clst (belongs-to el)))
        (labels ((find-nearest (clstrs best)
                   (if (null clstrs)
                       (if (eq crnt-clst (belongs-to el)) 0 1) ; We've checked all clusters, return 1 if cluster has changed
                       (let ((dist (dist (car clstrs) el))) ; Check this cluster
                         (if (< dist best)
                             (progn ; This is better than the best, so set this to best
                               (setf (belongs-to el) clstrs)
                               (find-nearest (cdr clstrs) dist))
                             (find-nearest (cdr clstrs) best))))))
          (setq nchanged (+ nchanged (find-nearest clusters most-positive-double-float))))))))

(defun update-clusters (clusters elements)
  (mapc #'zero-cluster clusters) ; Zero out the clusters
  (dolist (el elements)
    (add-element (car (belongs-to el)) el)))

(defun k-means (elements clusters)
  (do ((nchanged (reassign-elements elements clusters) (reassign-elements elements clusters)))
      ((= nchanged 0) clusters)
    (format t "~d~%" nchanged)
    (update-clusters clusters elements)))

(defun remove-close-clusters (clusters thresh)
  (let ((nrem 0))
    (labels ((rem-close (clusters)
               (let ((c (car clusters)))
                 (labels ((find-and-rem (clstrs)
                            (cond ((null (cadr clstrs))
                                   nil)
                                  ((< (dist c (cadr clstrs)) thresh)
                                   (format t "Removing ~S~%" (moment (cadr clstrs)))
                                   (setf (cdr clstrs) (cddr clstrs))
                                   (incf nrem)
                                   (find-and-rem (cdr clstrs)))
                                  (t (find-and-rem (cdr clstrs))))))
                   (find-and-rem clusters)))))
      (mapl #'rem-close clusters)
      nrem)))

(defun split-sparse-clusters (clusters thresh)
  (let ((new-clstrs nil)
        (nsplit 0))
    (dolist (clstr clusters)
      (when (< (density clstr) thresh)
        (incf nsplit)        
        (setq new-clstrs (cons (split clstr) new-clstrs))))
    (nconc clusters new-clstrs)
    nsplit))

(defun remove-small-clusters (clusters thresh)
  (delete-if (lambda (c)
               (< (nelem c) thresh))
             clusters))

(defun nk-means (elements min-density min-dist min-size)
  (let ((clusters (init-clusters elements)))
    (do ((ncreated 1 (prog1
                         (split-sparse-clusters clusters min-density)
                       (k-means elements clusters))))
        ((= ncreated 0)))
    (do ((ndestroyed 1 (prog1
                           (remove-close-clusters clusters min-dist)
                         (k-means elements clusters))))
        ((= ndestroyed 0) (remove-small-clusters clusters min-size)))))


(defun extract-image (image-path &optional (pixel-thresh 10) (min-density 0.6) (min-dist 50) (min-size 50) (nbars 12))
  (let* ((image (get-image image-path))                         ; The image
         (loop-time (png:image-width image))                    ; The loop time will be the width of the image
         (pixels (get-nonzero-pixels image pixel-thresh))       ; Extract the pixels of interest

         ;; Cluster the pixels and sort the clusters by time (horizontal location)
         (clusters (sort (nk-means pixels min-density min-dist min-size)
                         (lambda (clst1 clst2)
                           (< (event-time clst1) (event-time clst2)))))

         ;; Cluster the pixel clusters into bars and sort from bottom to top (lowest note to highest)
         (bars (sort (k-means clusters (init-clusters clusters nbars))
                     (lambda (bar1 bar2)
                       (> (moment bar1) (moment bar2)))))

         (n 1))                 ; init for bar orders

    ;; Assign bar orders
    (dolist (bar bars)
      (setf (order bar) n)
      (incf n))

    ;; Return the sequence and loop time
    (list
     (cons 'loop-time loop-time)
     (cons 'tines clusters))))

(defun create-sequence (tines)
  (let ((last-time 0))  ; init last-time
    (mapcar
     (lambda (note)
       (let ((time (event-time note))
             (ord (order (car (belongs-to note)))))
         (prog1
             `(,(- time last-time) . ,ord)
           (setq last-time time))))
     tines)))
