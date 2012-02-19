#!/bin/sh
#| -*- mode: lisp -*-
tmux new-session -d -s volume-control || exit 1
tmux send-keys -t volume-control "ccl -l $0" C-m
exit
|#

#-alsa-mixer
(progn
  (pushnew :alsa-mixer *features*)
  (open-shared-library "libasound.so")
  (open-shared-library "libalsavolume.so"))

(defun get-control-volume (control)
  (with-cstrs ((selem control))
    (external-call "alsa_mixer_get_volume" :address selem :int)))

(defun get-master-volume () (get-control-volume "Master"))
(defun get-pcm-volume () (get-control-volume "PCM"))

(defun set-control-volume (control vol)
  (with-cstrs ((selem control))
    (external-call "alsa_mixer_set_volume" :address selem :int vol)))

(defun set-master-volume (vol) (set-control-volume "Master" vol))
(defun set-pcm-volume (vol) (set-control-volume "PCM" vol))

(defun adjust-master-volume (delta)
  (set-master-volume (max 0 (min 100 (+ delta
					(get-master-volume))))))



(defun raise-volume () (adjust-master-volume 3))

(defun lower-volume () (adjust-master-volume -3))

(defun toggle-mute ()
  (set-pcm-volume (if (= (get-pcm-volume) 0) 100 0)))

(defun send-notify ()
  (let ((msg (format nil "Current: ~[mute~:;~a%~]"
		     (get-pcm-volume) (get-master-volume))))
    (run-program "notify-send" (list "-t" "500" "-i" "info" "Volume" msg))))



(defun raise () (raise-volume) (send-notify))
(defun lower () (lower-volume) (send-notify))
(defun toggle () (toggle-mute) (send-notify))
