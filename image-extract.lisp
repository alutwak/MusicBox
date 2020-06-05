;(declaim (optimize (debug 3)))

(ql:quickload "png")
(ql:quickload :array-operations)

(defstruct (pixel (:constructor make-pixel (loc mass))) 
  loc
  mass
  (cluster nil))

(defstruct (pspace (:constructor make-pspace (&optional left right top bottom)))
  (left most-positive-fixnum)
  (right 0)
  (top most-positive-fixnum)
  (bottom 0))

(defun pspace->list (ps)
  `(,(pspace-left ps) ,(pspace-right ps) ,(pspace-top ps) ,(pspace-bottom ps)))

(defun calc-pspace (pixels)
  (let ((bottom (reduce #'max (mapcar (lambda (p) (car (pixel-loc p))) pixels)))
        (top (reduce #'min (mapcar (lambda (p) (car (pixel-loc p))) pixels)))
        (right (reduce #'max (mapcar (lambda (p) (cadr (pixel-loc p))) pixels)))
        (left (reduce #'min (mapcar (lambda (p) (cadr (pixel-loc p))) pixels))))
    (make-pspace left right top bottom)))

(defmacro expand-pspace (ps pixel)
  `(let ((i (car (pixel-loc ,pixel)))
         (j (cadr (pixel-loc ,pixel))))
     (setf (pspace-left ,ps) (min (pspace-left ,ps) j))
     (setf (pspace-right ,ps) (max (pspace-right ,ps) j))
     (setf (pspace-top ,ps) (min (pspace-top ,ps) i))
     (setf (pspace-bottom ,ps) (max (pspace-bottom ,ps) i))))

(defun pspace-width (ps)
  (1+ (- (pspace-right ps) (pspace-left ps))))

(defun pspace-height (ps)
  (1+ (- (pspace-bottom ps) (pspace-top ps))))

(defun pspace-area (ps)
  (* (pspace-width ps) (pspace-height ps)))

(defun pspace-length (ps)
  (max (pspace-width ps) (pspace-height ps)))

(defun pspace-center (ps)
  `(,(/ (+ (pspace-top ps) (pspace-bottom ps)) 2.0)
    ,(/ (+ (pspace-left ps) (pspace-right ps)) 2.0)))

(defun get-image (path)
  (let ((im-3d (png:decode-file path)))
    (aops:reshape im-3d (butlast (array-dimensions im-3d)))))

(defun get-nonzero-pixels (image)
  (aops:reduce-index
      (lambda (&optional res pixel)
        (if pixel
            (cons pixel res)
            res))
      (i j)
    (let ((val (aref image i j)))
      (if (> val 10)
          (make-pixel (list i j) (float (/ val 256)))
          nil))))

;;;================== Cluster Base Class ===================================

(defgeneric zero-cluster (cluster)
  :documentation "Zero out a cluster to initialize it for the update cycle.")

(defgeneric add-element (cluster element)
  :documentation "Add an element to a cluster")

(defgeneric cluster-moment (cluster)
  :documentation "Returns the weighted center of the cluster")

(defgeneric cluster-height (cluster)
  :docuentation "Returns the height of the cluster")

(defgeneric cluster-width (cluster)
  :documentation "Returns the width of the cluster")

(defgeneric cluster-area (cluster)
  :documentation "Returns the area of the cluster")

(defgeneric cluster-area (cluster)
  :documentation "Returns the total area of the rectangular space containing the cluster")

(defgeneric cluster-density (cluster)
  :documentation "Returns the total density of the cluster")

(defgeneric cluster-dist (cluster object)
  :documentation "Returns the distance between a cluster and another object")

;;; ================= Pixel Cluster =========================================


(defstruct (cluster (:constructor make-cluster (&optional sum mass pslist)))
  (sum '(0 0))
  (mass 0)
  (pixels 0)
  (ps (apply #'make-pspace pslist)))

(defun zero-cluster (cluster)
  (setf (cluster-sum cluster) '(0 0))
  (setf (cluster-mass cluster) 0)
  (setf (cluster-pixels cluster) 0)
  (setf (cluster-ps cluster) (init-pspace))
  cluster)

(defun modify-cluster (cluster pixel fn)
  (let ((mass (pixel-mass pixel)))
    (setf (cluster-sum cluster)
          (mapcar (lambda (c-s p-l)
                    (funcall fn c-s (* mass p-l)))
                  (cluster-sum cluster)
                  (pixel-loc pixel)))
    (setf (cluster-mass cluster)
          (funcall fn
                   (cluster-mass cluster)
                   mass))))

(defun add-pixel (cluster pixel)
  (modify-cluster cluster pixel #'+)
  (incf (cluster-pixels cluster))
  (expand-pspace (cluster-ps cluster) pixel)
  cluster)

(defun cluster-moment (cluster)
  (if (= (cluster-mass cluster) 0)
      nil
      (mapcar (lambda (l)
                (/ l (cluster-mass cluster)))
              (cluster-sum cluster))))

(defun cluster-height (cluster)
  (pspace-height (cluster-ps cluster)))

(defun cluster-width (cluster)
  (pspace-width (cluster-ps cluster)))

(defun cluster-area (cluster)
  (pspace-area (cluster-ps cluster)))

(defun cluster-density (cluster)
  (/ (cluster-pixels cluster) (cluster-area cluster)))

(defun cluster-dist (cluster pixel-or-cluster)
  (if (= (cluster-mass cluster) 0)
      most-positive-double-float
      (sqrt
       (reduce
        #'+
        (mapcar
         (lambda (a b)
           (let ((dif (- a b)))
             (* dif dif)))
         (cluster-moment cluster)
         (cond ((pixel-p pixel-or-cluster)
                (pixel-loc pixel-or-cluster))
               ((cluster-p pixel-or-cluster)
                (cluster-moment pixel-or-cluster))
               (t
                `(,most-positive-double-float ,most-positive-double-float))))))))

(defun print-cluster (cluster)
  (let ((m (cluster-moment cluster))
        (p (cluster-pixels cluster))
        (d (cluster-density cluster))
        (ps (cluster-ps cluster)))
    (format t "~S: ~d ~f " m p d)
    (apply #'format (append '(t "[<~d >~d ^~d v~d]~%") (pspace->list ps)))))

(defun cluster-stats (clusters)
  (mapc #'print-cluster clusters)
  (format t "~d valid clusters~%" (length clusters)))

(defun draw-clusters (clusters image path)
  (let ((im-out (png:make-image (png:image-height image) (png:image-width image) 3)))
    (aops:each-index (i j k)
      (setf (aref im-out i j k) (aref image i j)))
    (dolist (clstr clusters)
      (let* ((loc (mapcar #'round (cluster-moment clstr)))
             (i (car loc))
             (j (cadr loc)))
        (when loc
          (mapc (lambda (k) ; Set G and B channels to 0
                  (setf (aref im-out i j k) 0))
                '(1 2))
          (setf (aref im-out i j 0) 255) ; Set R channel to 256
          )))
    (png:encode-file im-out path)))


;;; =============================== K-Means Higher Order Functions ===============================

(defun k-means (elements clusters reassign-fn update-fn)
  (do ((nchanged (funcall reassign-fn elements clusters) (funcall reassign-fn elements clusters)))
      ((= nchanged 0) clusters)
    (format t "~d~%" nchanged)
    (funcall update-fn clusters elements))
  clusters)

(defun nk-means (elements min-density min-dist reassign-fn update-fn split-fn remove-fn finish-fn)
  (flet ((k-m (els clsts)
           (k-means els clsts reassign-fn update-fn)))
    (let ((clusters (init-clusters elements)))
      (do ((ncreated 1 (prog1
                            (funcall split-fn clusters min-density)
                          (k-m pixels clusters))))
           ((= ncreated 0)))
      (do ((ndestroyed 1 (prog1
                             (funcall remove-fn clusters min-dist)
                           (k-m pixels clusters))))
          ((= ndestroyed 0) (funcall finish-fn clusters))))))

;;; =========================== K-Means for pixel-clusters ==================================

(defun init-pixel-clusters (pixels)
  (let ((ps (calc-pspace pixels)))
    (list (make-cluster (pspace-center ps) 1 (pspace->list ps)))))

(defun reassign-pixels (pixels clusters)
  (let ((nchanged 0))
    (dolist (p pixels nchanged)
      (let ((crnt-clst (pixel-cluster p)))
        (labels ((find-nearest (clstrs best)
                   (if (null clstrs)
                       (if (eq crnt-clst (pixel-cluster p)) 0 1) ; We've checked all clusters, return 1 if cluster has changed
                       (let ((dist (cluster-dist (car clstrs) p))) ; Check this cluster
                         (if (< dist best)
                             (progn ; This is better than the best, so set this to best
                               (setf (pixel-cluster p) clstrs)
                               (find-nearest (cdr clstrs) dist))
                             (find-nearest (cdr clstrs) best))))))
          (setq nchanged (+ nchanged (find-nearest clusters most-positive-double-float))))))))

(defun update-pixel-clusters (clusters pixels) ; &optional anneal
  (mapc #'zero-cluster clusters) ; Zero out the clusters
  (dolist (p pixels)
    (add-pixel (car (pixel-cluster p)) p)))

(defun remove-close-pixel-clusters (clusters thresh)
  (let ((nrem 0))
    (labels ((rem-close (clusters)
               (let ((c (car clusters)))
                 (labels ((find-and-rem (clstrs)
                            (cond ((null (cadr clstrs))
                                   nil)
                                  ((< (cluster-dist c (cadr clstrs)) thresh)
                                   (format t "Removing ~S~%" (cluster-moment (cadr clstrs)))
                                   (setf (cdr clstrs) (cddr clstrs))
                                   (incf nrem)
                                   (find-and-rem (cdr clstrs)))
                                  (t (find-and-rem (cdr clstrs))))))
                   (find-and-rem clusters)))))
      (mapl #'rem-close clusters)
      nrem)))

(defun remove-small-pixel-clusters (clusters thresh)
  (delete-if (lambda (c)
               (< (cluster-pixels c) thresh))
             clusters))

(defun split-sparse-pixel-clusters (clusters thresh)
  (let ((new-clstrs nil)
        (nsplit 0))
    (dolist (clstr clusters)
      (when (< (cluster-density clstr) thresh)
        (incf nsplit)
        (let* ((m (cluster-moment clstr))
               (w (cluster-width clstr))
               (h (cluster-height clstr))
               (split (cond ((> w h)
                             `((,(car m) ,(pspace-left (cluster-ps clstr)))
                               (,(car m) ,(pspace-right (cluster-ps clstr)))))
                            (t
                             `((,(pspace-top (cluster-ps clstr)) ,(cadr m))
                               (,(pspace-bottom (cluster-ps clstr)) ,(cadr m)))))))

        (format t "Splitting cluster ~S into ~S~%" m split)
        ;; Set this cluster to one of the new locations
        (add-pixel (zero-cluster clstr) (make-pixel (car split) 1))

        ;; Find an empty cluster and set that one to the other new location
        (setq new-clstrs (cons (make-cluster (cadr split) 1) new-clstrs)))))
    (nconc clusters new-clstrs)
    nsplit))

(defun nk-means-pixels (pixels min-density min-dist min-size)
  (nk-means pixels min-density min-dist min-size
            #'reassign-pixels #'update-pixel-clusters #'split-sparse-pixel-clusters
            #'remove-close-pixel-clusters (lambda (clusters) (remove-small-pixel-clusters clusters min-size))))

(defvar im (get-image "~/Documents/AudioDesigns/code/MusicBox/MusicBox-improved.png"))
(defvar pixels (get-nonzero-pixels im))
(defvar clusters nk-means pixels)
(defvar ordered-notes (sort (copy-list clusters) (lambda (c1 c2) (< (car (cluster-moment c1)) (car (cluster-moment c2))))))

(defun meanheight (clusters start n)
  (let* ((len (length clusters))
         (sublist (butlast (nthcdr start clusters) (- len start n))))
    (/                                  ; Divide the length
     (reduce #'+                        ; Compute the sum
             (mapcar (lambda (c)        ; Extract the cluster moments' heights
                       (car (cluster-moment c)))
                     sublist))
     n)))

(defvar bar-cluster-sizes ; (<note number> . <number of clusters>
  '((12 . 2)  
    (11 . 4)
    (10 . 4)
    (9 . 3)
    (8 . 4)
    (7 . 6)
    (6 . 2)
    (5 . 7)
    (4 . 7)   
    (3 . 5)   
    (2 . 6)   
    (1 . 6)))

(defun calc-bar-levels (b-c-s clusters)
  (let ((n (cdar b-c-s)))
   (cons
    `(,(caar b-c-s) ,(meanheight clusters 0 n))
    (if (cdr b-c-s)
        (calc-bar-levels (cdr b-c-s) (nthcdr n clusters))
        nil))))

(defun split-bars (b-c-s clusters)
  (let ((n (cdar b-c-s)))
   (cons
    `(,(caar b-c-s) ,(butlast clusters (- (length clusters) n)))
    (if (cdr b-c-s)
        (split-bars (cdr b-c-s) (nthcdr n clusters))
        nil))))

(defvar staff-clusters (split-bars bar-cluster-sizes (mapcar #'cluster-moment ordered-notes)))
(defvar staff-times (mapcar
                     (lambda (s-c)
                       `(,(car s-c)
                         ,(sort (mapcar (lambda (mom)
                                     (cadr mom))
                                   (cadr s-c))
                                #'<)))
                     staff-clusters))
