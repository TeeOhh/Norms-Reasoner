;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: dempster-shafer-accessors.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: December 21, 2020 16:09:56
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: December 29, 2020 16:09:56
;;;;  $LastChangedBy: olson $
;;;; ---------------------------------------------------------------------------

(in-package :fire)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modified from... e:\qrg\ea\v8\reasoning-source.lsp
;; More examples in... e:\qrg\fire\v3\analogy\sme-accessors.lsp
;; More examples in... e:\qrg\companions\v1\nuSketch-hookup\multimodal-instruction-games-source.lsp

;; Just call fire::init-ds-source(r) where r is the desired reasoner

;; fire::*reasoner* is the global variable for working reasoner
;; so... (fire::init-ds-source fire::*reasoner*)

(defclass ds-source (source)
  ()
  (:documentation "Special FIRE reasoner source for Depster-Shafer outsourced predicates"))


(defun init-ds-source (reasoner)
  (unless (contains-ds-source? reasoner)
    (add-ds-source reasoner))
  :ok)

(defun contains-ds-source? (reasoner)
  (find-if #'(lambda (source)
               (typep source 'ds-source))
           (sources reasoner)))

(defmethod add-ds-source ((reasoner reasoner))
  (let ((source (make-instance 'ds-source :reasoner reasoner)))
    (add-source source reasoner)
    (register-ds-handlers source reasoner)
    source))

;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun register-ds-handlers (source reasoner)
  ;; Get Dempster-Shafer confidence interval
  ;; (DS-confidenceIntervalOfProp ?beliefs-mt ?prop-set ?frame-of-discernment ?conf-interval)
  (register-simple-handler d::DS-confidenceIntervalOfProp
                                norm-get-confidence-interval
                                (:known :known :known :variable))
  ;; Check if the proposition is believed based on evidence
  ;; (DS-believed ?beliefs-mt ?prop-set ?frame-of-discernment)
  (register-simple-handler d::DS-believed
                                norm-belief-test
                                (:known :known :known))
  ;; When proposition is unbound, find the "most specific" one that is believed
  (register-simple-handler d::DS-believed
                                norm-get-mostspecific-belief
                                (:known :variable :known))
  
  ;; Get most believed proposition(s) from frame of discernment
  ; (DS-mostBelievedProp ?beliefs-mt ?frame-of-discernment ?prop)
  (register-simple-handler d::DS-mostBelievedProp
                                norm-get-most-believed
                                (:known :known :variable))
  
  source)

(defmethod global-predicate? ((thing (eql 'd::DS-confidenceIntervalOfProp))) t) ; global because we pass in the microtheory.

;; Compute Dempster-Shafer confidence interval
(defsource-handler norm-get-confidence-interval (beliefs-mt prop-set fod)
  (let* ((reasoner (reasoner source))
         (interval (d-s::full-dempster-shafer prop-set fod beliefs-mt))
         (prop (list 'd::DS-confidenceIntervalOfProp beliefs-mt prop-set fod interval)))
    (when interval
      (unless (ltre::true? prop (ltre reasoner))
        (tell prop reasoner :outsourced :all))
        ;;generate-cached-single-ask-response ???
        (generate-single-ask-response
          (list (cons (fifth query) interval))
          context))))

(defmethod global-predicate? ((thing (eql 'd::DS-believed))) t) ; global because we pass in the microtheory.

(defsource-handler norm-belief-test (beliefs-mt prop-set fod)
  ;; *** For now, defaulting to 0.9 for belief
  ;; *** Long term, use hyperparameter *belief-threshold* in d-s package so it can be modified
  (let* ((conf-interval (d-s::full-dempster-shafer prop-set fod beliefs-mt))
         (belief-val (car conf-interval))
         (pls-val (cdr conf-interval)))
    (when conf-interval
      (when (>= (/ (+ belief-val pls-val) 2) 0.9)
        (let* ((reasoner (reasoner source))
               (prop (list 'd::DS-believed beliefs-mt prop-set fod)))
          (unless (ltre::true? prop (ltre reasoner))
            (tell prop reasoner :outsourced :all))
          (generate-single-ask-response nil prop))))))

(defsource-handler norm-get-mostspecific-belief (beliefs-mt fod)
  ; proposition is unbound, so get the most specific, believed prop
  (let* ((max-belief 0.9)
        (prop-length-tracker (+ (length fod) 1))
        (prop nil)
        (result nil)
        (reasoner (reasoner source))
        (conf-intervals (d-s::full-dempster-shafer-FODpowerset fod beliefs-mt)))
    ;; key = prop-set and value = confidence interval
    
    (maphash #'(lambda (key val)
                 ;; entire FOD (complete ignorance) will always be most believed, don't care about it
                 (when (not (equal key fod))
                   (let* ((belief-val (car val)))
                     (when belief-val
                       ;; grab the smallest size (i.e., the most specific set from fod)
                       (when (and (>= belief-val max-belief)
                                  (< (length key) prop-length-tracker))
                         (setf prop-length-tracker (length key))
                         (setf max-belief belief-val)
                         (setf prop (list 'd::DS-believed beliefs-mt key fod))
                         (setf result (list (cons (third query) key))))))))
           conf-intervals)
    (when (and prop result)
      (unless (ltre::true? prop (ltre reasoner))
            (tell prop reasoner :outsourced :all))
      (generate-single-ask-response result context))))
    
(defmethod global-predicate? ((thing (eql 'd::DS-mostBelievedProp))) t) ; global because we pass in the microtheory.

(defsource-handler norm-get-most-believed (beliefs-mt fod)
  (let* ((max-belief 0.0)
        (props nil)
        (results nil)
        (reasoner (reasoner source))
        (conf-intervals (d-s::full-dempster-shafer-FOD fod beliefs-mt)))
    ;; key = prop-set and value = confidence interval
    (maphash #'(lambda (key val)
                 (let* ((belief-val (car val)))
                   (when belief-val
                    (when (= belief-val max-belief)
                    (push (list 'd::DS-mostBelievedProp beliefs-mt fod key) props)
                    (push (list (cons (fourth query) key)) results))
                    (when (> belief-val max-belief)
                      (setf props nil)
                      (setf results nil)
                      (setf max-belief belief-val)
                      (push (list 'd::DS-mostBelievedProp beliefs-mt fod key) props)
                      (push (list (cons (fourth query) key)) results)))))
           conf-intervals)
    (when (and prop results)
      (dolist (prop props)
        (unless (ltre::true? prop (ltre reasoner))
          (tell prop reasoner :outsourced :all)))
      (generate-multiple-ask-responses results context))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
