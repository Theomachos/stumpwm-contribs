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
;; Add the following line to your .stumpwmrc file:
;;
;; (load-module "wicd")
;;
;; You can use "%Y" in your mode line format to display status.
;;
;; Requires lucashpandolfo/dbus
;; code at https://github.com/lucashpandolfo/dbus
;; One way to use it is quicklisp local projects.
;;
;; It is hoped that a pull request will make it into the canonical
;; version.

;;; Code:

(in-package :stumpwm)

(defvar *wicd-current-network-color* 2 "Index of *colors* to use in menu for the currently connected network")
(defvar *wicd-wired-network-name* "wired" "What to call the wired network in the menu")
(defvar *wicd-connection-status-timer* nil "Monitors output when attempting to connect to a network")

(defun wicd-command (path interface name &rest args)
  (dbus:with-open-bus (bus (dbus:system-server-addresses))
    (dbus:with-introspected-object (wicd-service bus path "org.wicd.daemon")
      (apply #'wicd-service interface name args))))

(defun wicd-wireless-command (name &rest args)
  (apply #'wicd-command "/org/wicd/daemon/wireless" "org.wicd.daemon.wireless" name args))

(defun wicd-wired-command (name &rest args)
  (apply #'wicd-command "/org/wicd/daemon/wired" "org.wicd.daemon.wired" name args))

(defun wicd-general-command (name &rest args)
  (apply #'wicd-command "/org/wicd/daemon" "org.wicd.daemon" name args))

(defun wicd-get-wireless-property (network-number property)
  (wicd-wireless-command "GetWirelessProperty" 
                         `((:int32) ,network-number) `((:string) ,property)))

(defcommand wicd-disconnect () ()
  "disconnect wicd"
  (wicd-general-command "SetForcedDisconnect" '((:boolean) t))
  (wicd-general-command "Disconnect"))

(defcommand wicd-scan-and-connect () ()
  "Scans for networks and then presents menu of possibilities"
  (message "Scanning for list of networks...")
  (wicd-wireless-command "Scan" '((:boolean) t))
  (wicd-c))

(defcommand wicd-connect () ()
  "Presents menu of possibilities based on most recent scan"
  (message "Retrieving list of networks...")
  (wicd-c))

(defun wicd-c ()
  "Allow the user to select a network from a list"
  (let* ((network (second (select-from-menu
                           (current-screen)
                           (wicd-build-network-list))))
         (connecting-wired (equalp network *wicd-wired-network-name*)))
    (when network
      (if connecting-wired
          (wicd-wired-command "ConnectWired")
          (wicd-wireless-command "ConnectWireless" `((:int32) ,(parse-integer network))))
      (when (timer-p *wicd-connection-status-timer*) (cancel-timer *wicd-connection-status-timer*))
      (setf *wicd-connection-status-timer*
            (run-with-timer 1 1 (lambda () (wicd-monitor-connection-status connecting-wired)))))))

(defun wicd-monitor-connection-status (connecting-wired)
  "displays basic connection status information"
  (cond
    ((wicd-wireless-command "CheckIfWirelessConnecting")
     (message "connecting wireless..."))
    ((wicd-wired-command "CheckIfWiredConnecting")
     (message "connecting wired..."))
    (t (setf (timer-repeat *wicd-connection-status-timer*) nil)
       (message "~A" (if connecting-wired
                         (wicd-wired-command "CheckWiredConnectingMessage")
                         (wicd-wireless-command "CheckWirelessConnectingMessage"))))))

(defun wicd-decorate-essid (essid is-current)
  (concat "^["
          (if is-current
              (concat "^" (write-to-string *wicd-current-network-color*) "*")
              "")
          essid "^]"))

(defun get-wicd-current-essid ()
  (if (wicd-wired-command "GetWiredIP" '((:int32) 0))
      *wicd-wired-network-name*
      (wicd-wireless-command "GetCurrentNetwork" '((:int32) 0))))

(defun wicd-build-network-list ()
  "builds a list from wicd-cli -l output suitable for use with select-from-menu"
  (let ((current-essid (get-wicd-current-essid)))
    (append `((,(wicd-decorate-essid *wicd-wired-network-name*
                                     (string= *wicd-wired-network-name* current-essid))
               ,*wicd-wired-network-name*))
            (loop for n to (1- (wicd-wireless-command "GetNumberOfNetworks")) 
                  collecting (list (concat
                                    (let ((essid (wicd-get-wireless-property n "essid")))
                                      (wicd-decorate-essid essid (string= essid current-essid))) 
                                    " "
                                    (or (wicd-get-wireless-property n "encryption_method") "None") " "
                                    (write-to-string (wicd-get-wireless-property n "quality")) "%") 
                                   (write-to-string n))))))

(defun fmt-wicd (ml)
  (declare (ignore ml))
  (concat "wicd: "
          (or
           (cond
             ((wicd-wireless-command "CheckIfWirelessConnecting")
              (wicd-wireless-command "CheckWirelessConnectingMessage"))
             ((wicd-wired-command "CheckIfWiredConnecting")
              (wicd-wired-command "CheckWiredConnectingMessage"))
             (t (let ((status-info (cadr (wicd-general-command "GetConnectionStatus"))))
                  (unless (string= "" (car status-info))
                    (concat (cadr status-info) " " (caddr status-info) "%")))))
           "None")))

;;; add mode-line formatter

(add-screen-mode-line-formatter #\Y 'fmt-wicd)

;;; End of file
