(require 'cl)

(defvar im-agent-process nil)
(defvar im-agent-process-data nil)

(defvar im-agent-uppercase-triggle-p t)

(defvar-local im-agent-overlay nil)

;; start server
(if (process-live-p "im-agent")
    (kill-process "im-agent"))
(let ((proc (start-process "im-agent" "*im-agent*" "emacs-im-agent")))
  (set-process-query-on-exit-flag proc nil)
  ;; Wait until server is ready.
  (accept-process-output proc))

(defun im-agent-setup-process ()
  (if (or (not im-agent-process)
	  (not (process-live-p im-agent-process)))
      (let ((proc (make-network-process
		   :name "im-agent"
		   :service "/tmp/emacs-im-agent.socket"
		   :family 'local
		   :noquery t)))
	(set-process-filter-multibyte proc t)
	(set-process-coding-system proc 'utf-8 'utf-8)
	(set-process-filter proc 'im-agent-process-filter)
	(setq im-agent-process proc))))

;; send data to im-agent-process and wait until reply arrives
(defun im-agent-request (content)
  (im-agent-setup-process)
  (process-send-string im-agent-process content)
  (accept-process-output im-agent-process 1.5)
  im-agent-process-data)

(defun im-agent-process-filter (proc content)
  (setq im-agent-process-data (eval (read content))))

(defun im-agent-prompt (preedit candidates)
  (do ((prompt preedit)
       (index 1 (1+ index))
       (cands candidates (cdr cands)))
      ((null cands) prompt)
    (setq prompt
	  (concat prompt
		  " "
		  (format "%d.%s" index (car cands))))))

(defun im-agent-input-method (key)
  (if (null im-agent-overlay)
      (setq im-agent-overlay (make-overlay 0 0)))

  ;; Only trigger input method for specific keys.
  (if (and (or (< key ?a) (> key ?z))
	   (or (not im-agent-uppercase-triggle-p)
	       (or (< key ?A) (> key ?Z)))
	   (not (member key (string-to-list ",.\\\"':;!?<>()"))))
      (list key)

    (do* (keyseq
	  input-method-function ; disable input method temporarily
	  (overlay im-agent-overlay)
	  (data (im-agent-request (char-to-string key)) ; send first key
		(im-agent-request keyseq))
	  (preedit (car data) (car data))
	  (candidates (cadr data) (cadr data)))

	;; Check for candidates is needed, because preedit will be
	;; empty when turing page.
	((and (string= preedit "") (null candidates))
	 ;; Do commit.
	 (let ((commit (caddr data)))
	   (if commit (insert commit)))

	 ;; Remove overlay before input method exits.
	 (delete-overlay overlay)

	 ;; Key is processed, return an empty list.
	 ())

      ;; Display first condidate on overlay.
      (let ((s (if candidates (car candidates) preedit)))
	(put-text-property 0 (length s) 'face 'underline s)
	(overlay-put overlay 'before-string s)
	(move-overlay overlay (point) (point)))

      ;; FIXME: Leave out of this will make read prompt like a mess.
      (message (format "%s" candidates))

      ;; Show candicates and fetch another key.
      (setq keyseq
	    (let ((key (read-key-sequence
			(im-agent-prompt preedit candidates))))
	      (cond ((stringp key) key)
		    ((equal key [escape]) "\x1B")
		    (t (return (listify-key-sequence key)))))))))

(defun im-agent-inactivate ()
  ;; Delete session when input method is turned off.
  (if (process-live-p im-agent-process)
      (delete-process im-agent-process)))

(defun im-agent-help () nil)

(defun im-agent-use-package (package-name)
  (im-agent-setup-process)
  (setq input-method-function 'im-agent-input-method)
  (setq inactivate-current-input-method-function 'im-agent-inactivate)
  (setq describe-current-input-method-function 'im-agent-help))

(register-input-method
 "im-agent" "euc-cn" 'im-agent-use-package "【中】" "IMAgent")

(provide 'im-agent)
