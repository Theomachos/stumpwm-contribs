;; wicd helper commands  for StumpWM.
;;
;; Copyright (C) 2012 Joseph Gay
;;
;; Maintainer: Joseph Gay
;;

;; This module is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This module is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;
;; Just add the following line to your .stumpwmrc file:
;;
;; (load-module "wicd")
;;
;; Requires wicd-cli to be installed on the system
;;

;;; Code:

(in-package :stumpwm)

(defvar *wicd-current-network-color* 2 "Index of *colors* to use in menu for the currently connected network")
(defvar *wicd-wired-network-name* "wired" "What to call the wired network in the menu")
(defvar *wicd-cli-program-path* "/usr/bin/wicd-cli" "Path to the wicd-cli executable script")
(defvar *wicd-connection-status-timer* nil "Monitors output when attempting to connect to a network")

(defcommand wicd-disconnect () ()
  "disconnect wicd"
  (run-shell-command "wicd-cli -x"))

(defcommand wicd-scan-and-connect () ()
  "Scans for networks and then presents menu of possibilities"
  (message "Scanning for list of networks...")
  (wicd-c (run-shell-command "wicd-cli --wireless -Sl" t)))

(defcommand wicd-connect () ()
  "Presents menu of possibilities based on most recent scan"
  (message "Retrieving list of networks...")
  (wicd-c (run-shell-command "wicd-cli --wireless -l" t)))

(defun wicd-c (wicd-cache-string)
  "Allow the user to select a network from a list
Expects WICD-CACHE-STRING formatted as from wicd-cli -l output"
  (let ((network (second (select-from-menu
                          (current-screen)
                          (build-wicd-network-list wicd-cache-string)))))
    (when network (connect-wicd network))))

(defun connect-wicd (network-number)
  "connect wicd to the specified network by wicd number
NETWORK-NUMBER expects a sequence"
  (let ((args (if (equalp network-number *wicd-wired-network-name*)
                  '("--wired" "-c")
                  `("--wireless" "-c" "-n" ,network-number))))
    (when (timer-p *wicd-connection-status-timer*) (cancel-timer *wicd-connection-status-timer*))
    (setf *wicd-connection-status-output* (make-string-output-stream))
    (let ((proc-var (run-prog *wicd-cli-program-path* :args args :output :stream :pty t :wait nil)))
      (setf *wicd-connection-status-timer*
            (run-with-timer 1 1 (lambda () (monitor-wicd-connection-status proc-var)))))))

(defun monitor-wicd-connection-status (proc-var)
  "expects PROC-VAR a process structure / object implementation
collects process output and displays it line by line in a message"
  (loop for line = (handler-case (read-line
                                  #+sbcl (sb-ext:process-pty proc-var)
                                  #+ccl  (ccl:process-output proc-var)
                                  nil nil)
                     (stream-error (e)))
        for status = (concatenate 'string "^[^" (write-to-string *wicd-current-network-color*)
                                  "*Status: (if dhcp or validation takes long, check your settings)^]"
                                  (string #\Newline) line)
          then (concatenate 'string status (string #\Newline) line)
        while line do
          (message "~A" status))
  #+sbcl
  (unless (sb-ext:process-alive-p proc-var)
    (setf (timer-repeat *wicd-connection-status-timer*) nil))
  #+ccl
  (when (ccl:process-exhausted-p proc-var)
    (setf (timer-repeat *wicd-connection-status-timer*) nil))
  #-(or ccl sbcl)
  (progn
    (setf (timer-repeat *wicd-connection-status-timer*) nil)
    (message "~A" "Connecting, status monitoring unimplemented for your lisp implementation ...")))

(defun build-wicd-network-list (wicd-cache-string)
  "builds a list from wicd-cli -l output suitable for use with select-from-menu"
  (let ((current-essid (get-wicd-current-essid)))
    (mapcar (lambda (v) (let ((essid (elt v 1))
                         (num   (elt v 0)))
                     (list (concat "^["
                                   (if (equalp essid current-essid)
                                       (concat "^" (write-to-string *wicd-current-network-color*) "*")
                                       "")
                                   essid "^] "
                                   (unless (equalp essid *wicd-wired-network-name*)
                                     (concat
                                      (get-wicd-network-property num "encryption_method") " "
                                      (get-wicd-network-property num "quality") "%")))
                           (elt v 0))))
            (append `((,*wicd-wired-network-name* ,*wicd-wired-network-name*))
                    (mapcar (lambda (s)
                              (nth-value 1 (cl-ppcre:scan-to-strings
                                            "(\\d\\d?)\\s+\\S\\S(?::\\S\\S){5}\\s+\\d\\d?\\s+(\\S.+)" s)))
                            (rest (split-string wicd-cache-string)))))))

(defun get-wicd-network-property (network-number property)
  (string-trim '(#\Space #\Newline #\Linefeed #\Tab)
               (run-shell-command
                (concat "wicd-cli --wireless -p " property " -n " network-number)
                t)))

(defun get-wicd-current-essid ()
  (if (= 0 (length (run-shell-command "wicd-cli --wired -d" t)))
      (let ((output (run-shell-command "wicd-cli --wireless -d" t)))
        (unless (cl-ppcre:scan "^Invalid|^IP: None" output)
          (elt (nth-value 1 (cl-ppcre:scan-to-strings
                             "Essid: (\\S+)"
                             (cl-ppcre:regex-replace-all "\\s+" output " ")))
               0)))
      *wicd-wired-network-name*))

;;; End of file
