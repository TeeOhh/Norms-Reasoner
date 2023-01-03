;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: norms-source.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: April 20, 2022 13:00:52
;;;;   Purpose: handlers needed for reasoning between conjunctions (worlds)
;;;;            i.e., determining if a conjunction entails another
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2022-12-07 15:35:36 -0600 (Wed, 07 Dec 2022) $ 
;;;;  $LastChangedBy: Olson $
;;;; ---------------------------------------------------------------------------

(in-package :fire)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defclass norm-source (source)
  ()
  (:documentation "Special FIRE reasoner source for outsourced predicates used in Deontic Reasoning."))

(defun init-norm-source (reasoner)
  (unless (contains-norm-source? reasoner)
    (add-norm-source reasoner))
  :ok)

(defun contains-norm-source? (reasoner)
  (find-if #'(lambda (source)
               (typep source 'norm-source))
           (sources reasoner)))

(defmethod add-norm-source ((reasoner reasoner))
  (let ((source (make-instance 'norm-source :reasoner reasoner)))
    (add-source source reasoner)
    (register-norm-handlers source reasoner)
    source))

;; Handlers
;; =====================================
(defun register-norm-handlers (source reasoner)
  (register-simple-handler data::implies-PropProp 
    stmt-implies-stmt (:known :known))
  
  (register-simple-handler data::equiv-PropProp 
    equiv-prop-prop (:known :known)))

;; Definition: A statement X implies (really, entails) statement Y when every interpretation in which X is true, Y is also true.
;; This function thus asserts every statement of X, which is a conjunction, into a temporary microtheory. It then takes Y and does some cleaning to get it ready for query. Finally, it queries Y in the temporary microtheory and if it succeeds, asserts (implies X Y) in the ltre
(defsource-handler stmt-implies-stmt (spec-stmt genl-stmt)
  (let* ((cleaned-spec-stmt (norms::do-conj-surgery spec-stmt))
         (cleaned-genl-stmt (norms::do-conj-surgery genl-stmt))
         (temp-mt-name (gentemp "tmp-context" 'd))
         (reified-spec-stmts (norms::change-vars-to-symbols cleaned-spec-stmt))
         ;; *** should really keep track of bindings when variables are changed to entities
         (reasoner (reasoner source))
         (prop (kb::->data (list 'd::implies-PropProp spec-stmt genl-stmt))))
    (cond ((equal cleaned-spec-stmt `(d::and)) ;; i.e., (and) implies everything
           (unless (ltre::true? prop (ltre reasoner))
            (tell prop reasoner :outsourced :all))
           (generate-single-ask-response nil prop))
          (t 
            (norms::construct-conj-as-case reified-spec-stmts temp-mt-name)
            ;; *** assumes spec-stmt and genl-stmt are conjunctions
            ;; *** can i query for each conjunct individually since bindings will be in environment?
            (let ((query-form (cons `d::and (mapcan #'car (mapcar #'(lambda (x) (norms::make-ist-info-stmts x :mt temp-mt-name)) (cdr cleaned-genl-stmt))))))
              (when (fire::query (kb::->data query-form) :number 1 :context temp-mt-name)
                (unless (ltre::true? prop (ltre reasoner))
                  (tell prop reasoner :outsourced :all))
                (generate-single-ask-response nil prop)))))))

;; Definition: A statement X is equivalent to statement Y when every interpretation in which X is true, Y is also true and vice versa.

(defsource-handler equiv-prop-prop (spec-prop genl-prop)
  (let ((reasoner (reasoner source))
        (prop (kb::->data (list 'd::equiv-PropProp spec-prop genl-prop))))
    ;; x = y when (entails x y) AND (entails y x)
    (when (and (implies-helper spec-prop genl-prop) (implies-helper genl-prop spec-prop))
      (unless (ltre::true? prop (ltre reasoner))
        (tell prop reasoner :outsourced :all))
      (generate-single-ask-response nil prop))))

(defun implies-helper (spec-prop genl-prop)
  (let* ((cleaned-spec-prop (norms::do-conj-surgery spec-prop))
         (cleaned-genl-prop (norms::do-conj-surgery genl-prop))
         (temp-mt-name (gentemp "tmp-context" 'd))
         (reified-spec-props (norms::change-vars-to-symbols cleaned-spec-prop))
         )
    (norms::construct-conj-as-case reified-spec-props temp-mt-name)
    ;; Hueristic..
    ;; If there's just 1 statement in the conjunction, query that rather than inside an "and"
    ;; Otherwise, quwery the entire conjunction
    ;; *** This may not be needed now that cleaning is more sophisticated
    (if (and (= (length cleaned-genl-prop) 2) (equal (car cleaned-genl-prop) `d::and))
        (fire::query (kb::->data (cadr cleaned-genl-prop)) :number 1 :context temp-mt-name)
        (or (equal cleaned-spec-prop `(d::and)) 
            (fire::query (kb::->data cleaned-genl-prop) :number 1 :context temp-mt-name)))))