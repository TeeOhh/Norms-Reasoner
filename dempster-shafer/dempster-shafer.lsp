;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: dempster-shafer.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: August 10, 2020 16:57:52
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2022-01-20 18:29:40 -0600 (Thu, 20 Jan 2022) $
;;;;  $LastChangedBy: olson $
;;;; ---------------------------------------------------------------------------
(defpackage dempster-shafer
  (:nicknames d-s)
  (:use :common-lisp))

(in-package :d-s)

;; This contains the code for computing dempster-shafer uncertainty intervals (belief and plausibility for propositions within a frame of discernment) to be used for reasoning with uncertainty. 
;; Wikipedia description here: https://en.wikipedia.org/wiki/Dempster%E2%80%93Shafer_theory

;; Input: 1. frame of discernment (FOD) - (<prop1> <prop2> ... <propn>)
;;        2. microtheories to gather mass assignments from
;;        3. microtheory to output belief and plausibility values to
;; Output: Belief and Plausibility value for each proposition in frame of discernment
;; (belief <prop> <value>)
;; (plausibility <prop> <value>)

;; An example call using up to date (as of early 2021) norm frame representations:
;; (full-dempster-shafer '((evaluation norm123 Impermissible)) '((evaluation norm123 Obligated) (evaluation norm123 Optional (evaluation norm123 Impermissible)) 'DempsterShaferTestsMT)

;; start of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hypter params adopted from Falkenhainer's 1986 JTMS using Dempster-Shafer
;; *** Not used yet, as of early 2021
(defvar *propagation-delta* 1.0d-3  "belief changing less than this amount = no change")
(defvar *belief-threshold* 0.90 "believe if the belief is greater than or equal to threshold")
(defvar *round-off-error* 1.0d-9 "allow for slight double-precision round-off error")

(defvar *d-s-verbose* nil)

;; Runs full dempster's rule of combination on all mass assignments and returns uncertainty interval for a single proposition
(defun full-dempster-shafer (prop-set frame-of-discernment in-mt &key (verbose nil))
  (let* ((powerset-table (create-powerset-table frame-of-discernment))
        (mass-assignments (gather-belief-assignments powerset-table in-mt)))
    (setf *d-s-verbose* verbose)
    (when *d-s-verbose*
      (format t "Basic belief assignments~%")
      (format t "========================~%")
      (maphash #'print-hash-entry mass-assignments)
      (terpri))
    (compute-uncertainty-interval prop-set mass-assignments powerset-table)))

;; Same as previous but computes and returns uncertainty intervals for each prop set in the frame of discernment
;; Returns a hash table that looks like this: {(evaluation Norm511274 Obligated) : (0.9 . 1.0), (evaluation Norm511274 Optional) : (0.0 . 0.1), (evaluation Norm511274 Impermissble) : (0.0 . 0.1)}
(defun full-dempster-shafer-FOD (frame-of-discernment in-mt &key (verbose nil))
  (let* ((powerset-table (create-powerset-table frame-of-discernment))
        (mass-assignments (gather-belief-assignments powerset-table in-mt))
        (uncertainty-intervals (make-hash-table)))
    (setf *d-s-verbose* verbose)
    (when *d-s-verbose*
      (format t "Basic belief assignments~%")
      (format t "========================~%")
      (maphash #'print-hash-entry mass-assignments)
      (terpri))
    ;; for prop-set in frame-of-discernment
    (dolist (prop-set frame-of-discernment)
      (setf (gethash prop-set uncertainty-intervals) (compute-uncertainty-interval (list prop-set) mass-assignments powerset-table)))
    uncertainty-intervals))

; Same as previous but computes and returns uncertainty intervals for each prop set in the powerset of the frame of discernment
; Returns a hash table that looks like this: {(evaluation Norm511274 Obligated) : (0.9 . 1.0), (evaluation Norm511274 Optional) : (0.0 . 0.1), (evaluation Norm511274 Impermissble) : (0.0 . 0.1), ((evaluation Norm511274 Obligated) (evaluation Norm511274 Optional)) : (0.9 . 1.0), ((evaluation Norm511274 Impermissible) (evaluation Norm511274 Optional)) : (0.0 . 0.1), ...}
(defun full-dempster-shafer-FODpowerset (frame-of-discernment in-mt &key (verbose nil))
  (let* ((powerset-table (create-powerset-table frame-of-discernment))
        (mass-assignments (gather-belief-assignments powerset-table in-mt))
        (uncertainty-intervals (make-hash-table)))
    (setf *d-s-verbose* verbose)
    (when *d-s-verbose*
      (format t "Basic belief assignments~%")
      (format t "========================~%")
      (maphash #'print-hash-entry mass-assignments)
      (terpri))
    ;; for key = id, val = prop-set in powerset of frame-of-discernment
    (maphash #'(lambda (key val)
               (setf (gethash val uncertainty-intervals) (compute-uncertainty-interval val mass-assignments powerset-table)))
           powerset-table)
    uncertainty-intervals))

(defun powerset (s)
  (if s (mapcan (lambda (x) (list (cons (car s) x) x)) 
                (powerset (cdr s))) 
      '(())))

(defun create-powerset-table (set)
  ;; Calls powerset to create a powerset and creates a hash table in which each set from the powerset has its own unique id
  (let ((i 0)
        (power-set (powerset set))
        (powerset-table (make-hash-table)))
    (dolist (set power-set)
      (setf (gethash i powerset-table) set)
      (incf i))
    powerset-table))

(defun construct-deontic-fod (concept-id)
  ;; Takes in: "norm123", Outputs: the frame of discernment with the 3 deontic modals
  ;; *** Can make this modular by retrieving the concepts from an ontology in a microtheory. But for now, this will be hard coded.
  (list `(evaluation ,concept-id Obligated) `(evaluation ,concept-id Optional) `(evaluation ,concept-id Impermissible)))

(defun construct-prevalence-fod (concept-id)
  (list `(prevalence ,concept-id Continuously)
        `(prevalence ,concept-id Often)
        ;;`(prevalence ,concept-id RegularFrequency)
        `(prevalence ,concept-id Sometimes-GenericFrequency)
        ;;`(prevalence ,concept-id RandomFrequency)
        `(prevalence ,concept-id Rarely)
        ;;`(prevalence ,concept-id AlmostNever)
        `(prevalence ,concept-id Never)))

(defun construct-deontic-prop (concept-id evals)
  ;; Takes in: 'norm123 and 'Obligated or with a list of evals 'norm123 and '(Obligated Optional)
  ;; Returns: '((evaluation norm123 Obligated)) or with the list: '((evaluation norm123 Obligated) (evaluation norm123 Optional))
  ;; if its a list, need to construct the set of evaluation props
  (let ((return-list '()))
    (if (listp evals)
      (dolist (eval evals)
        (push `(evaluation ,concept-id ,eval) return-list))
        ;; otherwise, it's just one evaluation
      (push `(evaluation ,concept-id ,evals) return-list))
    return-list))

(defun normalize (belief-assignment degree-of-conflict)
  ;; for item in belief-assignment, item = item / degree-of-conflict
  (when (> degree-of-conflict 0.0)
    (when *d-s-verbose*
      (format t "Normalizing belief assignment: ~S with degree of conflict: ~S ~%" belief-assignment degree-of-conflict))
        (loop for i from 0 to (- (length belief-assignment) 1)
              do (let ((pre-norm (aref belief-assignment i)))
                   (setf (aref belief-assignment i) (/ pre-norm (- 1 degree-of-conflict))))))
  belief-assignment)

(defun print-hash-entry (key value)
  (format t "The value associated with key ~S is ~S~%" key value))

(defun pprint-mass-assignment (mass-assignment powerset-table)
  (maphash #'(lambda (key val)
               (format t "Total mass of ~S: ~S~%" val (aref mass-assignment key)))
           powerset-table))
  
(defun gather-belief-assignments (powerset-table mt)
  ;; definitions:
  ;;	1. Focal element = Every set in the power set with mass > 0
  ;;	2. Basic belief assignment =
  ;;		2.1 a function that maps each element of power set to interval [0,1]
  ;;		2.2 maps empty set to 0
  ;;		2.3 sum of all mass assignments for elements of power set sum to 1
  
  ;; params:
  ;; 	1. power-set: the power set of the frame of discernment
  ;;	2. mt: the microtheory to gather the evidenceFor statements from
  
  ;; outputs: 
  ;;	1. a hash table where keys = evidence sources and values = bindings of the form: ((?prop prop-set) (?mass mass-value)), serving as a basic belief assignment (BBA)
  
  ;; step 1: gather all evidenceFor statements and index by ?source in a hash table
  
  ;; *** This is slow, why not just gather the evidenceFor statements that we need?
  
  ;; *** Issue here is that keys must be symbols I believe. Thus if one wants to have a list be the source of an evidenceFor statement, this will not work
  (let ((basic-belief-assignments (make-hash-table)))
    (maphash #'(lambda (id prop-set)
                 (dolist (element (fire::query '(d::evidenceFor ?source ?prop ?mass) :context mt))
                   (when (set-equal (cdr (assoc '?prop element)) prop-set)
                     (let* ((source (cdr (assoc '?source element)))
                            (mass (cdr (assoc '?mass element))))
                       (when (not (gethash source basic-belief-assignments)) (setf (gethash source basic-belief-assignments) (make-array `(,(hash-table-count powerset-table)) :initial-element 0.0)))
                       (setf (aref (gethash source basic-belief-assignments) id) mass)))))
             powerset-table)
    
    ;; step 2: add mass to ignorance where total mass < 1 (entire FOD)
    (maphash #'(lambda (source mass-assignment)
                  ;;set the mass of the entire FOD equal to 1 - the mass of all other assignments
                 (let ((total-mass (reduce '+ mass-assignment)))
                   (when (< total-mass 1.0)
                     (setf (aref (gethash source basic-belief-assignments) 0) (- 1 total-mass)))))
             basic-belief-assignments)

    basic-belief-assignments))
    
(defun set-intersection (prop-set1 prop-set2)
  (let ((intersection '()))
    (dolist (prop1 prop-set1)
      (dolist (prop2 prop-set2)
        ;; keep track of intersecting sets
        (when (set-equal prop1 prop2) (push prop1 intersection))))
    intersection))

(defun set-equal (set1 set2)
  (and (= (length set1) (length set2))
       (every #'(lambda (x)
                  (some #'(lambda (y) (equal x y)) (remove-duplicates set2 :test 'equal)))
              (remove-duplicates set1 :test 'equal))))

(defun subset-of-set (set1 set2)
  (and (every #'(lambda (x)
                  (some #'(lambda (y) (equal x y)) set2))
              set1)))

(defun dempsters-rule-of-combination (m1 m2 powerset-table)
  ;; compute the orthogonal sum of m1 and m2
  ;; given m1 = (ARRAY m1(prop1) m1(prop2) ... m1(propN)) where N = |power set|
  
  ;; given m2 = (ARRAY m2(prop1) m2(prop2) ... m2(propN)) where N = |power set|
  
  ;; compute (orthogonal-sum m1 m2)

  (if (equal (length m1) (length m2))
    (let ((orthogonal-sum (make-array `(,(length m1)) :initial-element 0.0))
          (degree-of-conflict 0.0))
      (loop for i from 0 to (- (length m1) 1)
            do (loop for j from 0 to (- (length m2) 1)
            ;; check to see if mass of i or j is zero, if so, then no reason to continue cuz product is zero
            when (and (> (aref m1 i) 0.0) (> (aref m2 j) 0.0))
            do (let ((intersection (set-intersection (gethash i powerset-table) (gethash j powerset-table))))
                 ;; ***COULD BE FASTER HERE IF A HASHTABLE MAPPING TWO SETS TO THEIR INTERSECTION WAS USED FOR LOOKUP
                 (if intersection
                   (maphash #'(lambda (key val) 
                                (when (set-equal val intersection)
                                  (let ((cur (aref orthogonal-sum key))) 
                                        (setf (aref orthogonal-sum key) (+ cur (* (aref m1 i) (aref m2 j)))))))
                            powerset-table)
                     
                     ;; intersection = empty set i.e. found conflict, add it to degree-of-conflict
                     (incf degree-of-conflict (* (aref m1 i) (aref m2 j)))))))
      
      ;; Normalize
      (setf orthogonal-sum (normalize orthogonal-sum degree-of-conflict)))
      (format t "Error: ~S and ~S not the same length~%" m1 m2)))

(defun dempsters-rule-of-combination-all (mass-assignments powerset-table)
  ;;runs dempsters rule of combination through all mass assignments
  (let ((list-of-arrays '()))
    (maphash #'(lambda (key val)
                 (push val list-of-arrays))
             mass-assignments)
    (reduce #'(lambda (x y)
                (dempsters-rule-of-combination x y powerset-table))
            list-of-arrays)))

(defun compute-uncertainty-interval (prop-set mass-assignments powerset-table)
  ;; cons of belief and plausibility, where car is the belief and cdr is the plausibility
  (cond ((= (hash-table-count mass-assignments) 0) nil)
          (t
            (let ((combined-mass-assignment (dempsters-rule-of-combination-all mass-assignments powerset-table)))
              (when *d-s-verbose*
                (terpri)
                (format t "Total mass assignment~%")
                (format t "=====================~%")
                (pprint-mass-assignment combined-mass-assignment powerset-table)
                (terpri)
                (format t "Confidence interval for: ~S~%" prop-set)
                (format t "========================~%"))
              
              (cons (compute-belief prop-set combined-mass-assignment powerset-table)
                    (compute-plausibility prop-set combined-mass-assignment powerset-table))))))

(defun compute-belief (prop-set combined-mass-assignment powerset-table)
  (let ((belief 0.0))
    (maphash #'(lambda (id set)
                 (when (or (set-equal prop-set set) (subset-of-set set prop-set))
                            (incf belief (aref combined-mass-assignment id))))
             powerset-table)
    belief))

(defun compute-plausibility (prop-set combined-mass-assignment powerset-table)
  (let ((plausibility 0.0))
    (maphash #'(lambda (id set)
                 ;; *** Changed. This should have been combining all sets that intersect that set.
                 ;; old wrong way: (when (or (subset-of-set prop-set set) (subset-of-set set prop-set))
               (when (set-intersection prop-set set)
                  (incf plausibility (aref combined-mass-assignment id))))
           powerset-table)
    plausibility)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code