;;;
;;; ZhengMa for uim, based on generic.scm.
;;;
;;; Author: Xin Wang <dram.wang@gmail.com>
;;;
;;; Install:
;;;   uim-module-manager --register zhengma --path $HOME/.uim.d/plugin
;;;

(require "generic.scm")
(require "zhengma-rule.scm")

(define zhengma-candidate-max 5)
(define zhengma-commit-key? (make-key-predicate '(" ")))
(define zhengma-commit-seq-key? (make-key-predicate '("return")))
(define zhengma-commit-seq-and-off-key? (make-key-predicate '("<Shift>return")))
(define zhengma-commit-second-key? (make-key-predicate '(";")))
(define zhengma-off-key? (make-key-predicate '("<Control> ")))
(define zhengma-on-key? (make-key-predicate '("<Control> ")))

(define zhengma-is-temporary-off #f)
(define zhengma-temporary-off-key?
  (make-key-predicate '("escape" "<Control>c")))
(define zhengma-resume-key?
  (make-key-predicate '("<Control>scroll-lock" "<Control><Shift>scroll-lock")))

;;
;; Activate candidate selector. In this IM, the candidate selector is always
;; showed when IM is on.
;;
;; NOTE:
;; This is an error if we activate candidate selector with zero length cands.
;; So here we set len to at least 1 when call `im-activate-candidate-selector'.
;; And at `zhengma-get-candidate-handler' we just return a blank string if no
;; cands exists.
;;
(define zhengma-activate-candidate
  (lambda (pc)
    (let ((len (length (generic-context-cands pc))))
      (im-activate-candidate-selector
        pc
        (if (<= len 0) 1 len)
        zhengma-candidate-max))))

;;
;; Get candidate handler used by candidate-selector. Return a blank string if
;; index is out of bound.
;;
(define zhengma-get-candidate-handler
  (lambda (pc idx accel-enum-hint)
    (let* ((cands (generic-context-cands pc)))
      (if (>= idx (length cands))
        (list " " " " "")
        (list (nth idx cands) (digit->string (+ idx 1)) "")))))

(define zhengma-context-flush!
  (lambda (pc)
    (generic-context-flush pc)
    (zhengma-activate-candidate pc)))

(define zhengma-proc-off-mode
  (lambda (pc key state)
    (cond
      ((zhengma-on-key? key state)
       (generic-context-set-on! pc #t)
       (zhengma-activate-candidate pc))
      ((zhengma-resume-key? key state)
       (if zhengma-is-temporary-off
         (begin
           (generic-context-set-on! pc #t)
           (zhengma-activate-candidate pc))))
      (else (generic-commit-raw pc)))))

(define zhengma-update-cands!
  (lambda (pc)
    (let* ((rkc (generic-context-rk-context pc))
           (cs (rk-current-seq rkc))
           (cands (if cs (cadr cs) '())))
      (generic-context-set-cands! pc cands)
      (zhengma-activate-candidate pc))))

(define zhengma-turn-off
  (lambda (pc)
    (zhengma-context-flush! pc)
    (generic-context-set-on! pc #f)
    (im-deactivate-candidate-selector pc)))

;;
;; Process key shortcuts in input state. Return #t if key is processed.
;;
(define zhengma-proc-input-state-key-shortcuts
  (lambda (pc key state)
    (cond
      ((zhengma-off-key? key state)
       (set! zhengma-is-temporary-off #f)
       (zhengma-turn-off pc)
       #t)
      ((zhengma-temporary-off-key? key state)
       (set! zhengma-is-temporary-off #t)
       (zhengma-turn-off pc)
       (im-commit-raw pc)
       #t)
      ((generic-backspace-key? key state)
       (if (rk-backspace (generic-context-rk-context pc))
         (zhengma-update-cands! pc)
         (im-commit-raw pc))
       #t)
      ((zhengma-commit-key? key state)
       (let ((cands (generic-context-cands pc))
             (seq (rk-context-seq (generic-context-rk-context pc))))
         (if (not (null? seq))
           (if (not (null? cands))
             (begin
               (im-commit pc (nth 0 cands))
               (zhengma-context-flush! pc)))
           (im-commit-raw pc)))
       #t)
      ((zhengma-commit-seq-key? key state)
       (let ((seq (rk-context-seq (generic-context-rk-context pc))))
         (if (not (null? seq))
           (im-commit pc (string-list-concat seq))
           (im-commit-raw pc))
         (zhengma-context-flush! pc))
       #t)
      ((zhengma-commit-seq-and-off-key? key state)
       (let ((seq (rk-context-seq (generic-context-rk-context pc))))
         (if (not (null? seq))
           (im-commit pc (string-list-concat seq))
           (im-commit-raw pc))
         (zhengma-context-flush! pc)
         (zhengma-turn-off pc))
       #t)
      ((zhengma-commit-second-key? key state)
       (let ((cands (generic-context-cands pc)))
         (if (>= (length cands) 2)
           (begin
             (im-commit pc (cadr cands))
             (zhengma-context-flush! pc))))
       #t)
      ((ichar-numeric? key)
       (let ((cands (generic-context-cands pc))
             (n (numeric-ichar->integer key)))
         (if (and (>= n 1)
                  (<= n (length cands)))
           (begin
             (im-commit pc (nth (- n 1) cands))
             (zhengma-context-flush! pc))
           (im-commit-raw pc)))
       #t)
      ((symbol? key)
       (im-commit-raw pc)
       #t)
      ((and (modifier-key-mask state)
            (not (shift-key-mask state)))
       (im-commit-raw pc)
       #t)
      (else #f))))

(define zhengma-append-seq!
  (lambda (pc key)
    (let* ((rkc (generic-context-rk-context pc))
           (seq (rk-context-seq rkc)))
      (rk-context-set-seq! rkc (cons (charcode->string key) seq)))))

(define zhengma-proc-input-state-normal-keys
  (lambda (pc key state)
    (let ((rkc (generic-context-rk-context pc)))
      (zhengma-append-seq! pc key)

      (zhengma-update-cands! pc)

      (if (and (= (length (rk-context-seq rkc)) 4)
               (= (length (generic-context-cands pc)) 1))
        (begin
          (im-commit pc (nth 0 (generic-context-cands pc)))
          (zhengma-context-flush! pc))))))

;;
;; Process keys in input state.
;;
(define zhengma-proc-input-state
  (lambda (pc key state)
    (if (not (zhengma-proc-input-state-key-shortcuts pc key state))
      (zhengma-proc-input-state-normal-keys pc key state))))

;;
;; Show first candidate or full key sequence.
;; 
(define zhengma-update-preedit
  (lambda (pc)
    (let ((rkc (generic-context-rk-context pc))
          (cands (generic-context-cands pc))
          (n (generic-context-rk-nth pc)))
      (im-clear-preedit pc)
      (im-pushback-preedit
        pc preedit-reverse
        (if (not (null? cands))
          (car cands)
          (rk-pending rkc)))
      (im-update-preedit pc))))

(define zhengma-key-press-handler
  (lambda (pc key state)
    (if (ichar-control? key)
      (im-commit-raw pc)
      (if (generic-context-on pc)
        (zhengma-proc-input-state pc key state)
        (zhengma-proc-off-mode pc key state)))
    (zhengma-update-preedit pc)
    ()))

(define zhengma-init-handler
  (lambda (id im arg)
    (generic-context-new id im zhengma-rule #f)))

(register-im
  'zhengma
  "zh_CN:zh_TW:zh_HK"
  "UTF-8"
  (N_ "ZhengMa2")
  (N_ "ZhengMa input method (Chinese) (alt)")
  zhengma-init-handler

  generic-init-handler
  #f  ;; release-handler
  context-mode-handler
  zhengma-key-press-handler
  generic-key-release-handler
  generic-reset-handler
  zhengma-get-candidate-handler
  generic-set-candidate-index-handler
  context-prop-activate-handler
  #f
  generic-focus-in-handler
  generic-focus-out-handler
  generic-place-handler
  generic-displace-handler
  )

(generic-configure-widgets)
