(require 'cl)

(defvar sunpinyin-process nil)
(defvar sunpinyin-process-data nil)

(defvar-local sunpinyin-overlay nil)

;; start server
(if (process-live-p "sunpinyin-server")
    (kill-process "sunpinyin-server"))
(let ((proc (start-process "sunpinyin-server" "*sunpinyin-server*"
			   "emacs-sunpinyin")))
  (set-process-query-on-exit-flag proc nil)
  ;; Wait until server is ready.
  (accept-process-output proc))

(defun sunpinyin-setup-process ()
  (if (or (not sunpinyin-process)
	  (not (process-live-p sunpinyin-process)))
      (let ((proc (make-network-process
		   :name "sunpinyin"
		   :service "/tmp/emacs-sunpinyin.socket"
		   :family 'local
		   :noquery t)))
	(set-process-filter-multibyte proc t)
	(set-process-coding-system proc 'utf-8 'utf-8)
	(set-process-filter proc 'sunpinyin-process-filter)
	(setq sunpinyin-process proc))))

;; send data to sunpinyin-process and wait until reply arrives
(defun sunpinyin-request (content)
  (sunpinyin-setup-process)
  (process-send-string sunpinyin-process content)
  (accept-process-output sunpinyin-process)
  sunpinyin-process-data)

(defun sunpinyin-process-filter (proc content)
  (setq sunpinyin-process-data (eval (read content))))

(defun sunpinyin-prompt (preedit candidates)
  (do ((prompt preedit)
       (index 1 (1+ index))
       (cands candidates (cdr cands)))
      ((null cands) prompt)
    (setq prompt
	  (concat prompt
		  " "
		  (format "%d.%s" index (car cands))))))

(defun sunpinyin-input-method (key)
  (if (null sunpinyin-overlay)
      (setq sunpinyin-overlay (make-overlay 0 0)))

  ;; Only trigger input method for specific keys.
  (if (and (or (< key ?a) (> key ?z))
	   (not (member key (string-to-list ",.\\\"':;!?"))))
      (list key)

    (do* (keyseq
	  input-method-function ; disable input method temporarily
	  (overlay sunpinyin-overlay)
	  (data (sunpinyin-request (char-to-string key)) ; send first key
		(sunpinyin-request keyseq))
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
      (let ((s (if candidates (car candidates) "")))
	(put-text-property 0 (length s) 'face 'underline s)
	(overlay-put overlay 'before-string s)
	(move-overlay overlay (point) (point)))

      ;; FIXME: Leave out of this will make read prompt like a mess.
      (message (format "%s" candidates))

      ;; Show candicates and fetch another key.
      (setq keyseq
	    (let ((key (read-key-sequence
			(sunpinyin-prompt preedit candidates))))
	      (cond ((stringp key) key)
		    ((equal key [escape]) "\x1B")
		    (t (return (listify-key-sequence key)))))))))

(defun sunpinyin-inactivate ()
  ;; Delete session when input method is turned off.
  (if (process-live-p sunpinyin-process)
      (delete-process sunpinyin-process)))

(defun sunpinyin-help () nil)

(defun sunpinyin-use-package (package-name)
  (sunpinyin-setup-process)
  (setq input-method-function 'sunpinyin-input-method)
  (setq inactivate-current-input-method-function 'sunpinyin-inactivate)
  (setq describe-current-input-method-function 'sunpinyin-help))

(register-input-method
 "sunpinyin" "euc-cn" 'sunpinyin-use-package "【中】" "SunPinyin")

(provide 'sunpinyin)
