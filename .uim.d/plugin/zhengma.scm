;;;
;;; ZhengMa for uim with vi mode auto switching support, based on generic.scm.
;;;
;;; Author: Xin Wang <dram.wang@gmail.com>
;;;
;;; Install:
;;;   uim-module-manager --register zhengma --path /home/dram/.uim.d/plugin
;;;

(require "generic.scm")

(require "fileio.scm")

(define zhengma-open-vi-mode-file
  (lambda ()
    (file-open "/tmp/uim-vi-mode"
	       (file-open-flags-number '($O_RDWR $O_CREAT))
	       (file-open-mode-number '($S_IRUSR $S_IWUSR)))))

(define zhengma-vi-mode-fd (zhengma-open-vi-mode-file))

(define zhengma-get-vi-op
  (lambda ()
    (let ((fd zhengma-vi-mode-fd))
      (file-position-set! fd 0 (assq-cdr '$SEEK_SET
					 file-position-whence-alist))
      (let ((content (file-read fd 1)))
	(file-position-set! fd 0 (assq-cdr '$SEEK_SET
					   file-position-whence-alist))
	(file-write fd (list #\space))
	(cond
	  ((eof-object? content) 'vi-op-mode-unknown)
	  ((char=? (car content) #\t) 'vi-op-turn-on-im)
	  (else 'vi-op-mode-unknown))))))

(define zhengma-tell-vi-status
  (lambda (stat)
    (let ((fd zhengma-vi-mode-fd))
      (file-position-set! fd 1 (assq-cdr '$SEEK_SET
					 file-position-whence-alist))
      (file-write fd (list stat)))))

(define zhengma-proc-input-state-without-preedit
  (lambda (pc key state rkc)
    (cond
     ((generic-off-key? key state)
      (zhengma-tell-vi-status #\F)
      (generic-context-set-on! pc #f)
      #f)
     ((generic-cancel-key? key state)
      (zhengma-tell-vi-status #\T)
      (generic-context-flush pc)
      (generic-context-set-on! pc #f)
      (generic-commit-raw pc)
      #f)
     (else (generic-proc-input-state-without-preedit pc key state rkc)))))

(define zhengma-proc-input-state-with-preedit
  (lambda (pc key state rkc)
    (cond
      ((generic-off-key? key state)
       (zhengma-tell-vi-status #\F)
       (let ((pending (rk-pending rkc)))
         (if (not (string=? pending ""))
           (begin
             (im-commit pc pending)
             (generic-context-flush pc)))
         (generic-context-set-on! pc #f)
         #f))
      ((and (ichar-printable? key)
            (= (ichar-downcase key) (char->integer #\;)))
       (let ((cands (generic-context-cands pc)))
         (if (>= (length cands) 2)
           (begin
             (if (pair? (car cands))
               (im-commit pc (car (cadr cands)))
               (im-commit pc (cadr cands)))
             (im-deactivate-candidate-selector pc)
             (generic-context-flush pc)
             (generic-context-set-multi-cand-input! pc #f))))
       #f)
      (else (generic-proc-input-state-with-preedit pc key state rkc)))))

(define zhengma-proc-input-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
    	   (seq (rk-context-seq rkc))
	   (res #f))
      (and
       (if (string=? (rk-pending rkc) "")
	   (zhengma-proc-input-state-without-preedit pc key state rkc)
	   (zhengma-proc-input-state-with-preedit pc key state rkc))
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
         (if res
	     (begin
	       (im-commit pc (string-list-concat seq))
	       (generic-context-set-rk-nth! pc 0)
	       (generic-context-set-candidate-op-count! pc 0)
	       (generic-context-set-cands! pc '())
	       (im-deactivate-candidate-selector pc)))
         (let ((cs (rk-current-seq rkc)))
           ;; single candidate
           (if (and (not (= (length (rk-context-seq rkc)) 4))
                    (not (rk-partial? rkc))
                    cs
                    (null? (cdr (cadr cs))))
             (generic-context-set-cands! pc (cadr cs))
             (generic-update-input-state-cands pc key state rkc seq res))))))))

(define zhengma-proc-specific-multi-cand-input-state
  (lambda (pc key state rkc)
    (cond
      ((generic-off-key? key state)
       (zhengma-tell-vi-status #\F)
       (let ((cands (generic-context-cands pc))
             (n (generic-context-rk-nth pc)))
         (if (and
               (not (null? cands))
               (> n -1))
           (begin
             (if (pair? (car cands))
               (im-commit pc (car (nth n cands)))
               (im-commit pc (nth n cands)))
             (generic-context-flush pc))
           (if (not (string=? (rk-pending rkc) "")) ;; flush pending rk
             (generic-context-flush pc)))
         (generic-context-set-on! pc #f)
         (im-deactivate-candidate-selector pc)
         #f))
      (else
        (generic-proc-specific-multi-cand-input-state pc key state rkc)))))

(define zhengma-proc-multi-cand-input-state
  (lambda (pc key state)
    (let* ((rkc (generic-context-rk-context pc))
           (seq (rk-context-seq rkc))
           (cands (generic-context-cands pc))
           (res #f))
      (and
       (zhengma-proc-specific-multi-cand-input-state pc key state rkc)
       (begin
	 (set! res
	       (rk-push-key!
		rkc
		(charcode->string key)))
         (if res
	     ;; commit matched word and continue new rk
	     (begin
               (if (< (generic-context-rk-nth pc) (length res))
                 (im-commit pc (nth (generic-context-rk-nth pc) res))
	         ;(im-commit pc (car (nth (generic-context-rk-nth pc) cands)))
	         ;(im-commit pc (nth 0 res))
                 ) ;; XXX: what is the expected behavior here?
	       (generic-context-set-rk-nth! pc 0)
	       (generic-context-set-candidate-op-count! pc 0)
	       (im-deactivate-candidate-selector pc)
               (generic-context-set-multi-cand-input! pc #f)
               (generic-update-input-state-cands pc key state rkc seq res))
             (generic-update-multi-cand-state-cands pc key state rkc)))))))

(define zhengma-key-press-handler
  (lambda (pc key state)
    (if (ichar-control? key)
	(im-commit-raw pc)
	(if (generic-context-on pc)
	  (if (generic-context-multi-cand-input pc)
	    (zhengma-proc-multi-cand-input-state pc key state)
	    (zhengma-proc-input-state pc key state))
          ;; test (zhengma-get-vi-op) first to always consume that op character
	  (if (and (eq? (zhengma-get-vi-op) 'vi-op-turn-on-im)
                   (not (generic-cancel-key? key state)))
	    (begin
	      (generic-context-set-on! pc #t)
	      (zhengma-proc-input-state pc key state)
	      (generic-update-preedit pc))
	    (generic-proc-off-mode pc key state))))
    (generic-update-preedit pc)
    ()))

(define zhengma-init-handler
  (lambda (id im arg)
    (generic-context-new id im "zhengma.table" #f)))

(define generic-init-handler
  (lambda (id im init-handler)
    (init-handler id im #f)))

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
  generic-get-candidate-handler
  generic-set-candidate-index-handler
  context-prop-activate-handler
  #f
  generic-focus-in-handler
  generic-focus-out-handler
  generic-place-handler
  generic-displace-handler
  )

(generic-configure-widgets)
