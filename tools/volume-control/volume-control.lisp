#!/bin/sh
#| -*- mode: lisp -*-
exec tmux new-session -d -s volume-control "LD_LIBRARY_PATH=$HOME/lib ccl -l $0"
|#

#-alsa-mixer
(progn
  (pushnew :alsa-mixer *features*)
  (open-shared-library "libasound.so")
  (open-shared-library "libalsavolume.so"))

#-libnotify
(progn
  (pushnew :libnotify *features*)
  (open-shared-library "libnotify.so")
  (with-cstrs ((progname "volume-control"))
    (external-call "notify_init" :address progname :int)))

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
  (let ((handle (with-cstrs
                    ((summary "Volume")
                     (body (format nil "Current: ~[mute~:;~a%~]"
                                   (get-pcm-volume) (get-master-volume)))
                     (icon "info"))
                  (external-call "notify_notification_new"
                                 :address summary
                                 :address body
                                 :address icon
                                 :address))))
    (external-call "notify_notification_set_timeout"
                   :address handle :int 500 :void)
    (external-call "notify_notification_show"
                   :address handle :address (%null-ptr) :int)))


(defun raise () (raise-volume) (send-notify))
(defun lower () (lower-volume) (send-notify))
(defun toggle () (toggle-mute) (send-notify))
