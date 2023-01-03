;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: loading-storing.lsp
;;;;    System: 
;;;;    Author: Taylor Olson
;;;;   Created: May 16, 2022 13:00:52
;;;;   Purpose: Loading certain domains needed for norm stuff i.e., norm learning vs mct stuff
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2022-12-06 00:48:03 -0600 (Tue, 06 Dec 2022) $ 
;;;;  $LastChangedBy: Olson $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

(defpackage norms
  (:use :common-lisp))

(defun load-dempster-shafer (&optional (top-dir "e:\\Moral-Intuition-Construction\\"))
  (load (compile-file (concatenate 'string top-dir "dempster-shafer\\dempster-shafer.lsp")))
  (load (compile-file (concatenate 'string top-dir "dempster-shafer\\dempster-shafer-source.lsp"))))

(defun load-norm-outsourced-preds (&optional (top-dir "e:\\Moral-Intuition-Construction\\"))
  (load (compile-file (concatenate 'string top-dir "norms-source.lsp"))))

;; =============================================================
;; Start of loading and storing code needed for AAAI-23 MCT experiment
;; =============================================================
;; load stuff needed to do moral-conventional learning and reasoning
(defun setup-norm-mct (&optional (top-dir "e:\\Moral-Intuition-Construction\\"))
  ;; lisp files
  (open-nextkb)
  (load-dempster-shafer top-dir)
  (load-norm-outsourced-preds top-dir)
  ;; add outsourced predicates
  (fire::add-ds-source fire:*reasoner*)
  (fire::add-norm-source fire:*reasoner*)
  (load (compile-file (concatenate 'string top-dir "utils.lsp")))
  (load (compile-file (concatenate 'string top-dir "norm-mc-tests.lsp")))
  ;; krf files
  (update-flatfiles-in-dir (concatenate 'string top-dir "flat-files\\")))

;; Loads file contents lists
(defun load-contents-in-file (file-loc &aux contents next-expr)
  (with-open-file (stream file-loc)
    (do () ()
      (setq next-expr (read stream nil nil))
      (if next-expr (push next-expr contents) (return-from load-contents-in-file (nreverse contents))))))

;; Takes in a file of insert-norm-frame-in-mt funcalls and executes them, making norm frame knowledge assertions
(defun run-evidence-storing-from-file (in-file &key (path nil))
  (let ((funcalls (load-contents-in-file (concatenate 'string path in-file))))
   (dolist (fcall funcalls)
     (eval fcall))))

;; Takes in a microtheory and dumps all of the facts out to a krf file
;; Used to store a belief microtheory containing norm frames and evidence out to memory
(defun dump-evidence-to-ff (microtheory out-file &key (path nil))
  (let ((flat-file-path (concatenate 'string path out-file)))
    (ensure-directories-exist flat-file-path)
    (format t "Looking in microtheory: ~S~%" microtheory)
    (let* ((facts (reverse (kb::list-mt-facts microtheory)))
           (spec-mt-stmts (fire:ask-it `(genlMt ?spec-mt ,microtheory))))
      (unless (every 'listp facts)
        (error "Facts not all lists: ~%."))
      ;; *** Should remove duplicate norm frames here and merge evidence
      
      (with-open-file (f-out flat-file-path 
                             :direction :output 
                             :if-exists :rename 
                             :if-does-not-exist :create)
      	(format f-out ";;; Mt ~A, dumped ~A ~A from ~A.~%" microtheory (fire::concise-date-string) (fire::concise-time-string) (machine-instance))
        (format f-out "(in-microtheory ~S)~%" microtheory)
        ;; For spindles
        (dolist (stmt spec-mt-stmts)
          (format f-out "~s~%" stmt))            
        (dolist (fact facts) ;(reverse facts)
         ;(format t "Writing ~s to file~%" fact)
         (format f-out "~s~%" fact))))))