;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: utils.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: April 20, 2022 13:00:52
;;;;   Purpose: 1. necessary encoding and transformation of logical forms for norm frames
;;;;              - e.g., from variables binded to norm frame slots, it constructs the multiple norm frame logical statements for each slot
;;;;              - e.g., conjunction surgery
;;;;            2. norm frame storing
;;;;            3. genls hierarchy analysis
;;;;              - e.g., find lowest shared genls between two concepts
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2022-12-08 22:28:18 -0600 (Thu, 08 Dec 2022) $ 
;;;;  $LastChangedBy: Olson $
;;;; ---------------------------------------------------------------------------
(in-package :norms)

(defun add-morals (agent &optional (moral-mts `(d::PositiveMoralNormsMt d::NegativeMoralNormsMt)))
  (dolist (mt moral-mts)
    (kb::store-fact (kb::->data `(genlMt ,agent ,mt)) `d::UniversalVocabularyMt)
    ;(fire::tell-it (kb::->data `(genlMt ,agent ,mt)))
    ))

(defun remove-morals (agent &optional (moral-mts `(d::PositiveMoralNormsMt d::NegativeMoralNormsMt)))
  (dolist (mt moral-mts)
    (kb::forget-fact (kb::->data `(genlMt ,agent ,mt)) `d::UniversalVocabularyMt)
    ;(fire::untell-it (kb::->data `(genlMt ,agent ,mt)))
    ))

;; =========================================================
;; making logical statements for norm frames
;; =========================================================
(defun make-evaluation-set-stmt (norm-id evaluation)
  (cond ((equalp evaluation 'Permissible) `((d::evaluation ,norm-id d::Obligatory) (d::evaluation ,norm-id d::Optional)))
        ((equalp evaluation 'Omissible) `((d::evaluation ,norm-id d::Impermissible) (d::evaluation ,norm-id d::Optional)))
        (t `((d::evaluation ,norm-id ,evaluation)))))

(defun make-evidence-stmt (norm-id sentence-id evaluation mass)
  ;; *** Mappings from 2nd-order evals to sets
  `(d::evidenceFor (d::PresentationEventFn ,sentence-id ,(gentemp "event" 'd)) ,(make-evaluation-set-stmt norm-id evaluation) ,mass))

(defun make-evaluation-stmt (norm-id evaluation)
  `(d::evaluation ,norm-id ,evaluation))

(defun make-norm-stmt (norm-id)
  `(d::isa ,norm-id d::Norm))

(defun make-behavior-stmt (norm-id behavior-conjunct)
  `(d::behavior ,norm-id ,behavior-conjunct))

(defun make-context-stmt (norm-id context-conjunct)
  `(d::context ,norm-id ,context-conjunct))

(defun make-text-stmt (sentence-id sentence-text)
  `(d::sentenceText ,sentence-id ,sentence-text))

(defun make-sender-stmt (sentence-id sender)
  `(d::senderOfInfo ,sentence-id ,sender))

;; For Moral vs Conventional experiments, need true labels
;; 1st-principles = '(principle 1, principle 2, ...)
(defun make-principles-stmt (norm-id 1st-principle)
  `(d::firstPrincipleInvolved ,norm-id ,1st-principle))

(defun construct-conj-as-case (conjunction mt-name)
  (fire:forget (kb::->data `(genlMt MC-Rules-BackgroundKnowledgeMt UniversalVocabularyMt)))
  (fire:forget (kb::->data `(genlMt MC-Rules-BackgroundKnowledgeMt BaseKB)))
  (norms::make-wm-context '(MC-Rules-BackgroundKnowledgeMt MC-Facts-BackgroundKnowledgeMt) :name mt-name :beliefs conjunction :nuke-first? t))

(defun construct-knowledge-stmt (beliefs-mt m-norm moral-mt)
  (let ((behavior (car (fire:query (kb::->data `(behavior ,m-norm ?b)) :context moral-mt :response `d::?b)))
        (context (car (fire:query (kb::->data `(context ,m-norm ?c)) :context moral-mt :response `d::?c)))
        (evaluation (car (fire:query (kb::->data `(evaluation ,m-norm ?e)) :context moral-mt :response `d::?e))))
    (kb::->data `(normativeKnowledge ,beliefs-mt ,behavior ,context ,evaluation))))

(defun construct-eval-response (believed-eval)
  (cond ((null believed-eval) "Unsure")
        ((equal believed-eval `d::Impermissible) `d::Impermissible)
        ((fire:query (kb::->data `(deonticSubsumes ,believed-eval Permissible)) :context `d::NormativeMt) 
         `d::Permissible)
        (t believed-eval)))

;; =================================
;; Inserting evidence for norms into kb
;; =================================
;; One cannot insert norms into logical environments that contain moral axioms
(defvar *black-listed-evidence-mts* '(PositiveMoralNormsMt MoralNormsMt NegativeMoralNormsMt))
(defun insert-norm-frame-in-mt (mt &key context-conjunct behavior-conjunct evaluation (text nil) (sender nil) (mass 0.9) (1st-principles nil))
  (when (member mt *black-listed-evidence-mts*)
    (warn "You do not have access to add norms to ~s.~%" mt)
      (return-from insert-norm-frame-in-mt))
  ;; check if behavior with equivalent context and behavior exist
  ;; *** Should really look in spindles below it to merge across mts
  ;; *** Variablize any entities first
  ;; *** This does not correctly unify within nested conjunctions and thus some norms are not merged correctly… And I'm assuming when you query for a belief it will only retrieve norms based on direct ordering. e.g., (and (and (isa ?eat EatingEvent) (doneBy ?eat ?agent))) does not unify with  (and (doneBy ?eat ?agent) (isa ?eat EatingEvent))
  ;; check implies 
  (fire:tell-it (kb::->data `(genlMt ,mt NormativeMt)))
  (let* ((norm-exists (car (fire::query (kb::->data `(d::and (d::isa ?norm Norm) (d::context ?norm ?c) (d::equiv-PropProp ?c ,context-conjunct) (d::behavior ?norm ?b) (d::equiv-PropProp ?b ,behavior-conjunct))) :context mt :response `d::?norm)))
         (norm-id (or norm-exists (gentemp "norm" 'd)))
         (sentence-id (gentemp "sentence" 'd))
         )
    (cond (norm-exists (format t "Existing evidence for a norm found: ~A.~%" norm-exists))
      (t (format t "No existing evidence for this norm, creating a new one.~%")
       (kb::store-fact (kb::->data (make-norm-stmt norm-id)) mt)
       (kb::store-fact (kb::->data (make-context-stmt norm-id context-conjunct)) mt)
       (kb::store-fact (kb::->data (make-behavior-stmt norm-id behavior-conjunct)) mt)))
    (kb::store-fact (kb::->data (make-evidence-stmt norm-id sentence-id evaluation mass)) mt)
    (dolist (principle 1st-principles)
      (let ((principle-stmt (make-principles-stmt norm-id principle)))
        (when (not (fire:ask-it (kb::->data `(ist-Information ,mt ,principle-stmt))))
          (kb::store-fact principle-stmt mt))))
    (when sender (kb::store-fact (kb::->data (make-sender-stmt sentence-id sender)) mt))
    (when text (kb::store-fact (kb::->data (make-text-stmt sentence-id text)) mt))))

;; =================================
;; Making cases and queries
;; =================================
(defun forget-wm-context (name &key (reasoner fire:*reasoner*))
  (dolist (f (fire:ask-it `(d::wmOnly (d::ist-Information ,name ?x)) 
               :response '?x :reasoner reasoner))
    (fire:untell-it f :context name :reasoner reasoner)))

(defun make-wm-context (genl-mts &key (name (gentemp "tmp-context" 'd))
                                 (beliefs nil) (reasoner fire:*reasoner*)
                                 (nuke-first? t))
  (when nuke-first?
    (forget-wm-context (kb::->data name) :reasoner reasoner))
  (dolist (g genl-mts)
    (fire:tell-it (kb::->data `(d::genlMt ,name ,g)) :reasoner reasoner))
  ;; If the conjunction is negated, need to just store the entire negated conjunction in the mt
  ;; *** In future, need to handle nested modals better
  (cond ((equal (car beliefs) `d::not) 
         (let ((cleaned-beliefs (do-conj-surgery beliefs)))
           (fire:tell-it (kb::->data cleaned-beliefs) :context name :reasoner reasoner)))
        ((equal (car beliefs) `d::and) 
         (let ((cleaned-beliefs (do-conj-surgery beliefs)))
           (dolist (b (cdr cleaned-beliefs))
             (fire:tell-it (kb::->data b) :context name :reasoner reasoner))))
        (t (format t "Error: arg must be a conjunction or negation of a conjunction. Instead got: ~S~%" beliefs)))
  name)

;; This is a key function for doing norm reasoning
;; It is used to process a logical statement and get it ready for query
;; For example, it takes (isa ?bob1 MaleHuman) and functionally returns the statements below (though with other control predicates): 
;; ((isa ?bob1 ?bob1-type) (genls ?bob1-type MaleHuman))
;; So it helps norm reasoning to prove entailments via the genls and genlPreds hierarchy
(defun make-ist-info-stmts (stmt &key (mt '?mt) (inverted nil))
  ;; ***Assumes stmt is a statement of a binary predicate
  ;; by default, creates statements that see if stmt is more general than ?mt
  ;; if inverted, creates statements that look in the opposite direction to see if ?mt is more general than stmt
  (let* ((ist-info-stmts)
         (non-context-stmt) ;non-contextualized statement
         (pred (car stmt))
         (first-arg (cadr stmt))
         (second-arg (caddr stmt)))
    (cond ((equalp pred `data::isa)
      (let* ((type-arg (combine-symbols (symbol-to-var first-arg) '-type))
             (variabalized-stmt (list `data::isa first-arg type-arg)))
        (push (kb::->data (list `data::useTransitiveInference (list `data::ist-Information mt variabalized-stmt))) ist-info-stmts)
        (if inverted
          ;; it's an isa, so make statements that do transitive inference for the object type
            (push (kb::->data (list `data::allFactsAllowed (list `data::useTransitiveInference (list `data::ist-Information `data::MC-BackgroundKnowledgeMt (list `data::genls second-arg type-arg))))) ist-info-stmts)
            (push (kb::->data (list `data::allFactsAllowed (list `data::useTransitiveInference (list `data::ist-Information `data::MC-BackgroundKnowledgeMt (list `data::genls type-arg second-arg))))) ist-info-stmts))
        (setf non-context-stmt variabalized-stmt)))
          ((equalp pred `d::not)
           (push (kb::->data (list `data::ist-Information mt stmt)) ist-info-stmts)
           (setf non-context-stmt stmt))
          ;; it's a statement other than isa, make statements that do transitive inference for the predicate
          (t (let* ((pred-arg (combine-symbols '?pred- pred))
             (pred-stmt (cons pred-arg (rest stmt))))
           (push (kb::->data (list `data::ist-Information mt pred-stmt)) ist-info-stmts)
           (push (kb::->data (list `data::allFactsAllowed (list `data::useTransitiveInference (list `data::ist-Information `data::MC-BackgroundKnowledgeMt (list `data::genlPreds pred-arg pred))))) ist-info-stmts)
           (setf non-context-stmt stmt))))
    ;; car of what this return is ist-info statements, the cdr is the non-contextualized statements in conjunct form
    (cons ist-info-stmts non-context-stmt)))

(defun get-fact-justification (fact)
  (mapcar (lambda (x) (ltre::datum-lisp-form (ltre::tms-node-datum x))) (ltre::assumptions-of-node (ltre::datum-tms-node (ltre::referent fact nil (fire:ltre fire:*reasoner*))))))

(defun extract-justification (justification-list)
  (let ((moral-norms))
    (dolist (fact justification-list)
      (when (member `d::DS-believed fact)
        (return-from extract-justification nil))
      (push (car (fire:query (kb::->data `(unifies ,fact (inKB (ist-Information ?mt (isa ?m-norm MoralNorm))))) :response `d::?m-norm)) moral-norms))
    ;; return of nil = from evidence, list of moral norms = from internal moral standards
    (remove nil moral-norms)))

(defun extract-justification-knowledge-stmt (epistemic-state agent-mt)
  (let ((justifications (norms::get-fact-justification (kb::->data `(ist-Information NormativeMt ,epistemic-state)))))
    (mapcar #'(lambda (norm) 
                (norms::construct-knowledge-stmt agent-mt norm `d::MoralNormsMt)) 
            (norms::extract-justification justifications))))

;; =================================
;; Symbol play
;; =================================
(defun var? (var)
  (and (symbolp var) (equal (subseq (symbol-name var) 0 1) "?")))

(defun symbol-to-var (symbol)
  (if (not (var? symbol))
      (combine-symbols '? symbol)
      symbol))

;; E.g., action-entity -> ?action
(defun entity-to-var (symbol)
  (let ((end (search "-entity" (symbol-name symbol))))
    (if (and end (not (equal (subseq (symbol-name symbol) 0 1) "?")))
        (combine-symbols '? (subseq (symbol-name symbol) 0 end))
        symbol)))

(defun change-entities-to-vars (s-expr)
  (cond ((consp s-expr) (mapcar #'change-entities-to-vars s-expr))
        (t (entity-to-var s-expr))))

(defun var-to-symbol (var)
  (if (symbolp var)
    (if (equal (subseq (symbol-name var) 0 1) "?")
      (combine-symbols (subseq (symbol-name var) 1) '-entity )
      var)
      var))

(defun change-vars-to-symbols (s-expr)
  (cond ((consp s-expr) (mapcar #'change-vars-to-symbols s-expr))
        (t (var-to-symbol s-expr))))

(defun make-vars-unique (s-expr)
  (let ((unique-symbol (gentemp "" 'd)))
    (make-vars-unique-helper s-expr unique-symbol)))

(defun make-vars-unique-helper (s-expr unique-symbol)
  (cond ((consp s-expr) (mapcar #'(lambda (x) (make-vars-unique-helper x unique-symbol)) s-expr))
        (t (make-var-unique s-expr unique-symbol))))

(defun make-var-unique (var unique-symbol)
    (if (equal (subseq (symbol-name var) 0 1) "?") 
      (combine-symbols var unique-symbol)
      var))
  
(defun combine-symbols (&rest objects)
  (intern (format nil "~{~a~}" objects) 'd))

;; =================================
;; Conjunct play
;; =================================

;; Test case for do-conj-surgery
; (norms::do-conj-surgery
;                 '(and (isa bob1 Male) (and (isa jill1 Female) (isa jill1 FemaleHuman))
;                       (not (and (isa bob1 Human) (isa jill1 Human))) (not (isa jill1 Male))
;                       (not (and (isa jill1 MaleHuman))) (not (not (isa jill1 PrettyHuman)))
;                       (and (and (isa bob1 HumanMale)) (isa bob1 UglyPerson))))
; Should yield:
; (and (not (isa jill1 MaleHuman)) (not (isa jill1 Male)) (not (and (isa bob1 Human) (isa jill1 Human))) (isa bob1 Male)
;      (isa jill1 Female) (isa jill1 FemaleHuman) (isa jill1 PrettyHuman) (isa bob1 HumanMale) (isa bob1 UglyPerson))



(defun merge-conjunctions (conj-1 conj-2)
  (do-conj-surgery `(d::and ,conj-1 ,conj-2)))

(defun do-conj-surgery (conj &key (verbose))
  ;; first do clean conjunction with the 3 types of transformations below
  ;; then optimize ordering of conjuncts for query
  (remove nil (clean-conjunction conj :verbose verbose)))

(defun clean-conjunction (conjunction &key verbose)
  ;; *** In future, can use conjunct ordering code in this file below to speed up queries
  ;; first check if conjunction or negated conjunction
  (if (or (equal (car conjunction) `d::and) 
          (and (equal (car conjunction) `d::not) (equal (caadr conjunction) `d::and)))
      (let ((result (clean-conjunction-helper conjunction nil verbose)))
        (if (consp (car result))
            (car result)
            result))
      (format t "~A is malformed. Must be a conjunction or negated conjunction.~%" conjunction)))

;; This function takes in a conjunction and does a tree talk to do two-ish types of transformations and returns a logically equivalent, cleaned conjunction
;; 1: not-not-x->x: (not (not x)) -> x
;; 1.5: not-and-lit->not-lit: (not (and x)) -> (not x)
;; 2: and-and-x->and-x: (and (and x)) -> (and x)
(defun clean-conjunction-helper (conjunction &optional and-before? verbose)
  (if conjunction
    (cond ((modal-stmt? conjunction);; a modal statement like (and nested-stmt) or (not nested-stmt)
           (let ((modal (car conjunction)))
                   ;; === Rule 1: not-not-x->x i.e., Remove double-negation ===
             (cond ((and (true-neg-conj? conjunction) (true-neg-conj? (cadr conjunction)))
                    (when verbose
                      (format t "not-not-x->x rule hit with: ~A. And before = ~A~%" conjunction and-before?))
                    (mapcan #'(lambda (x) (clean-conjunction-helper x nil verbose)) (cdadr conjunction)))
                   ((and (true-neg-conj? conjunction) (equal `d::and (caadr conjunction))
                         (> (length (cadr conjunction)) 2))
                    (when verbose
                      (format t "not-and-x cleaning rule hit with: ~A. And before = ~A~%" conjunction and-before?))
                    (list (cons modal (mapcar #'(lambda (x) (clean-conjunction-helper x nil verbose)) (cdr conjunction)))))
                   ;; === Rule 1.5: not-and-lit->not-lit
                   ((and (true-neg-conj? conjunction) (equal `d::and (caadr conjunction))
                         (= (length (cadr conjunction)) 2))
                    (when verbose
                      (format t "not-and-lit->not-lit cleaning rule hit with: ~A. And before = ~A~%" conjunction and-before?))
                    (list (cons modal (clean-conjunction-helper (cadadr conjunction) t verbose))))
                   ;; === Rule 2: and-and-x->and-x Remove nested conjunctions ===
                   ((and (equal modal `d::and) and-before?) ;; modal indicates conjunction and is nested within another
                    (when verbose
                      (format t "and-and-x->and-x rule hit with: ~A. And before = ~A~%" conjunction and-before?))
                    (mapcan #'(lambda (x) (clean-conjunction-helper x t verbose)) (cdr conjunction))
                    )
                   ((and (equal modal `d::and) (not and-before?)) ;; modal indicates conjunction and is not nested within another
                    (when verbose 
                      (format t "non-nested conjunction case hit with: ~A. And before = ~A~%" conjunction and-before?))
                    (cons modal (mapcan #'(lambda (x) (clean-conjunction-helper x t verbose)) (cdr conjunction))))
                   (t ;; None of the important modal cases, add the modal and recursively run on rest of list
                      (when verbose
                        (format t "base modal case hit with: ~A. And before = ~A~%" conjunction and-before?))
                      (list (append (list modal) (mapcan #'(lambda (x) (clean-conjunction-helper x nil verbose)) (cdr conjunction))))))))
          (t ;; Base case: not a stmt nested in a modal, simply return it
             (when verbose (format t "base non-modal case hit with: ~A. And before = ~A~%" conjunction and-before?))
             (if (and (empty-conj? conjunction) and-before?)
                 nil
                 (list conjunction))))
      nil))

(defun modal-stmt? (stmt)
  ;; *** Is this the right check? Should really check for consp of any conjunct, rather than first?
  (and (consp stmt) 
       (consp (cadr stmt))))

(defun true-neg-conj? (conjunct)
  (and (consp conjunct)
       (member (first conjunct) 'data::(uninferredSentence unknownSentence not))
       t))

(defun empty-conj? (conjunction)
  (and (consp conjunction)
       (= (length conjunction) 1)
       (equal (car conjunction) `d::and)))

;; *** Below is optimization code taken from :agents package in companions\v1\agents\query-optimizer.lsp
;; Test case from there: 
; (defvar cl-user::test-conj6
;     'data::((cityOwner ?objectProtected (IndexicalFn currentRole))
;             (different ?protector-Agentive ?objectProtected)  ; different needs to move down so its args are bound
;             (isa ?objectProtected FreeCiv-City)
;             (isa ?protector-Agentive FreeCiv-MilitaryUnit)
;             (objectFoundInLocation ?protector-Agentive ?objectProtected)))

(defun optimize-conjuncts (conjuncts &optional bound-vars)
  (when conjuncts
    (let ((next-conjunct (next-conjunct conjuncts bound-vars)))
      (cons next-conjunct
            (optimize-conjuncts (remove next-conjunct conjuncts)
                                (union (fire::formula-variables next-conjunct) bound-vars))))))

(defun next-conjunct (conjuncts bound-vars)
  (when conjuncts
    (flet ((num-unbound (conj) (count-if-not #'(lambda (var) (member var bound-vars)) (fire::formula-variables conj))))
      (let* ((non-neg (remove-if #'neg-conj? conjuncts))
             (neg (set-difference conjuncts non-neg))
             (non-structural (remove-if #'structural-conj? non-neg))
             (structural (set-difference non-neg non-structural))
             (best-non (minimum non-structural :key #'num-unbound))
             (best-struc (minimum structural :key #'num-unbound))
             (best-neg (and neg (minimum neg :key #'num-unbound)))
             (unbnd-vars-neg (and best-neg (num-unbound best-neg))))
        (if (and best-neg (zerop unbnd-vars-neg))
          best-neg
          (if (or (null best-non) (and best-struc (< (num-unbound best-struc) (num-unbound best-non))))
            best-struc
            (or best-non
                best-neg)))))))

(defun structural-conj? (conjunct)
  (and (consp conjunct)
       (or (member (first conjunct) 'data::(isa genls genlPreds genlMt elementOf))
           (fire::numerical-eval-predicate? (first conjunct))
           (fire::lisp-test-statement? conjunct))
       t))

(defun neg-conj? (conjunct)
  (and (consp conjunct)
       (member (first conjunct) 'data::(uninferredSentence unknownSentence not different))
       t))

(defun minimum (lst &key (key #'identity))
  (distinguished-member lst :key key :test #'<))

(defun distinguished-member (lst &key (key #'identity) (test #'neq))
  (let* ((value-so-far (car lst))
         (unique? t))
    (dolist (item (cdr lst))
      (cond ((ignore-errors
               (funcall test (funcall key item) (funcall key value-so-far)))
             (setq value-so-far item)
             (setq unique? t))
            ((eq (funcall key item) (funcall key value-so-far))
             (setq unique? nil))))
      (values value-so-far unique?)))

(defun neq (a b) (not (eq a b)))

;; =====================================================
;; Would love to make norm frame creation and evidence storing a macro like this in future
;; As of 12/2/22, not in use though - T.O.
;; =====================================================

(defun defNormEvidence->assertions (form)
  (destructuring-bind (context-conjunct behavior-conjunct evaluation &key (text nil) (sender nil) (mass 0.9))
      (cdr form)
    (when (or (null context-conjunct) (null behavior-conjunct) (null evaluation))
      (warn "defNormEvidence must specify at least the contextual preconditions (as a conjunction) of the norm, the behavior (as a conjunction) of the norm, and an evaluation (deontic status) of the norm.~%~s" form)
      (return-from defNormEvidence->assertions))
    (let* ((assertions)
           (norm-exists (fire::query (kb::->data `(d::and 
                                                    (d::context ?norm ,context-conjunct)
                                                    (d::behavior ?norm ,behavior-conjunct))) :response '?norm))
           (norm-id (or (car norm-exists) (gentemp "norm" 'd)))
           (sentence-id (gentemp "sentence" 'd)))
      ;;  could check those seen in file already or query for it
      (when (not norm-exists)
            (push (kb::->data (make-norm-stmt norm-id)) assertions)
            (push (kb::->data (make-context-stmt norm-id context-conjunct)) assertions)
            (push (kb::->data (make-behavior-stmt norm-id behavior-conjunct)) assertions))
      ;; *** Currently assumes only 1st-order evaluations... Need to do a lookup to see what the FoD equivalent statement would be
      (push (kb::->data (make-evidence-stmt norm-id sentence-id evaluation mass)) assertions)
      (when sender (push (kb::->data (make-sender-stmt sentence-id sender)) assertions))
      (when text (push (kb::->data (make-text-stmt sentence-id text)) assertions))
      assertions)))

;; =================================
;; Genls play
;; =================================
(defun get-shared-genls (list-of-concepts)
	(let ((genls-list (mapcar #'(lambda (x) (fire::all-genls x 'UniversalVocabularyMt)) list-of-concepts)))
		(intersection-of-lists genls-list 'start)))

(defun test-genls (list-of-concepts genl)
  (let ((genl-list-test (mapcan #'(lambda (x)
              (if (fire::ask-it `(allFactsAllowed (useTransitiveInference (ist-Information EverythingPSC (genls ,x ,genl)))))
                    (list x) nil)) list-of-concepts)))
    genl-list-test))

(defun intersection-of-lists (lists shared)
	(when (equal shared 'start) (setf shared (car lists)))
	(cond ((null lists) shared)
				(t (intersection-of-lists (cdr lists) (intersection shared (first lists))))))

(defun get-min-overlapping-genl (list-1 list-2)
	(let* ((genl-intersection (set-difference (get-shared-genls list-1) (get-shared-genls list-2)))
	       (least-spec-amount (length list-2))
           (least-spec-cols '())
	       (least-genl nil))
	  (dolist (genl genl-intersection)
	    (let ((specs (test-genls list-2 genl)))
	      (when (< (length specs) least-spec-amount)
         	(setf least-spec-cols specs)
	        (setf least-spec-amount (length specs))
	        (setf least-genl genl))))
	  
   	  (format t "Genl with least coverage: ~S~%" least-genl)
   	  (format t "Least coverage: ~S~%" least-spec-amount)
      (format t "Collections covered: ~S~%" least-spec-cols)))



;; *** OLD CONJUNCTION SURGERY STUFF as of 12/5/22. This did not do a tree-walk so only handled first occurence of needed transformations
; (defun merge-conjuncts (conj-1 conj-2)
;   (cond ((and (equalp (car conj-1) `d::not) (equalp (car conj-2) `d::not))
;          `(d::and ,conj-1 ,conj-2)) ;; (not (and X)) + (not (and Y)) = (and (not (and X)) (not (and Y)))
;         ((and (equalp (car conj-1) `d::not) (equalp (car conj-2) `d::and)) 
;          (remove nil (cons `d::and (cons conj-1 (cdr conj-2))))) ;; (not (and X)) + (and Y) = (and (not (and X)) Y)
;         ((and (equalp (car conj-2) `d::not) (equalp (car conj-1) `d::and)) ;; same but opposite of above
;          (remove nil (cons `d::and (cons conj-2 (cdr conj-1)))))
;         (t (remove nil (cons `d::and (append (cdr conj-1) (cdr conj-2)))) ;; base case when both conjuncts are non-negated. (and X) + (and Y) = (and X Y)
;            )))

; (defun do-conj-surgery (conj)
;   (mapcar #'(lambda (x) (reduce-negated-conj x)) 
;           (purge-nested-conj conj)))

;; i.e., (and (and X)) -> (and X)
;; e.g., (norms::purge-nested-conj '(and (and (isa bob1 MaleHuman)))) -> (and (isa bob1 MaleHuman))
; (defun purge-nested-conj (conj)
;   (if (equal (car conj) `d::and)
;       (let ((new-conj `d::(and)))
;         (dolist (stmt (cdr conj))
;           (cond ((equal stmt `d::(and))) ;; base case, empty conjunction
;                 ((equal (car stmt) `d::and) ;; nested conj case, just add all elements to conj above
;                  (dolist (nested-stmt (cdr stmt))
;                    (push nested-stmt new-conj)))
;                 ; ((equal (car stmt) `d::not)
;                 ;  (push `d::not new-conj)
;                 ;  (dolist (nested-stmt (cdr stmt))
;                 ;    (push nested-stmt new-conj)))
;                 (t (push stmt new-conj) ;; non-conj nested statement
;                    )))
;         (reverse (kb::->data new-conj)))
;       conj))

; ;; i.e., negated conjuncts containing a single statement can be taken out of conjunct
; ;; (not (and stmt)) -> (not stmt)
; (defun reduce-negated-conj (negated-conj)
;   (if (and (consp negated-conj) (= (length negated-conj) 2) (true-neg-conj? negated-conj))
;       `(d::not ,(reduce-conj-literal (cadr negated-conj)))
;       negated-conj))

; ;; (and single-stmt) -> single-stmt
; (defun reduce-conj-literal (conj-literal)
;   (if (and (consp conj-literal) (= (length conj-literal) 2) (equal (car conj-literal) `d::and))
;       (cadr conj-literal)
;       conj-literal))

; ;; (not (not ...)) -> (not ...)
; (defun reduce-double-neg (negated-stmts)
;   (if (and (consp negated-stmts) (= (length negated-stmts) 2) (true-neg-conj? negated-stmts) (true-neg-conj? (cadr negated-stmts)))
;       `(d::not ,(cadadr negated-stmts))
;       negated-stmts))

; ;; (and (and ...)) -> (and ...)
; (defun reduce-double-conj (conj)
;   (if (and (consp conj) (equal (car conj) `d::and) (equal (caadr conj) `d::and))
;       `(d::and ,(cadadr conj))
;       conj))
