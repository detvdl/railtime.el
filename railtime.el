;;; railtime.el --- Request Belgian railtime information -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Detlev Vandaele
;; Version: 0.0.1
;; Author: Detlev Vandaele <detlev.vandaele@gmail.com>
;; Maintainer: Detlev Vandaele <detlev.vandaele@gmail.com>
;; Created: 23 april 2018

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'bui)
(require 'json)
(require 'mm-uu)
(require 'url)

;; Constants
(defconst rt--api-base-uri "https://api.irail.be")
(defconst rt--default-headers '(("Accept" . "application/json")))
(defconst rt--default-parameters '(("format" . "json")))

;;; Variables

(defvar rt--cached-stations nil
  "Cache to be filled with all available stations upon first request.")

;;; Customizable variables & groups

(defgroup railtime nil
  "Custom group for customizable variables."
  :prefix "rt--"
  :version "0.0.1")

(defcustom rt--default-language 'en
  "Default language for API requests."
  :type 'symbol
  :group 'railtime)

(defcustom rt--default-connection-from nil
  "Default value for the `from' parameter when requesting a connection."
  :type 'string
  :group 'railtime)

(defcustom rt--default-connection-to nil
  "Default value for the `to' parameter when requesting a connection."
  :type 'string
  :group 'railtime)

;; Faces
(defface rt--delay-face
  `((t :foreground ,(or (face-attribute 'font-lock-warning-face :foreground) "red")))
  "Face for train delays."
  :group 'railtime)

(defface rt--status-good-face
  '((t :foreground "green"))
  "Face for good statuses."
  :group 'railtime)

(defface rt--status-warning-face
  '((t :foreground "red"))
  "Face for warning statuses."
  :group 'railtime)


;;; Macros

(defmacro rt--message-with-timeout (timeout mesg)
  "For TIMEOUT seconds, display a message MESG."
  `(run-with-timer 0 nil
                   (lambda () (message ,mesg)
                     (run-with-timer ,timeout nil (lambda () (message nil))))))

;;; General functions

(defun rt--merge-defaults-alist (defaults &optional other)
  "Merge a DEFAULTS -alist with an OTHER alist.
Values are merged right-to-left, aka defaults are only
retained if there's no element with the same car in OTHER."
  (let ((result (or other '())))
    (dolist (el defaults)
      (when (not (assoc (car el) result))
        (push el result)))
    result))

;; Maybe, perhaps, slightly, blatantly stolen from https://github.com/tkf/emacs-request/blob/master/request.el
(defun rt--parse-http-header-at-point ()
  "Parse the http-header string, eg. HTTP/1.1 200 OK."
  (when (re-search-forward "\\=[ \t\n]*HTTP/\\([0-9\\.]+\\) +\\([0-9]+\\)" nil t)
    (list :version (match-string 1)
          :code (string-to-number (match-string 2)))))

(defun rt--get-response-charset ()
  "Parse the charset from the response to avoid wrong en-/decoding of buffer contents."
  (-when-let* ((mime-info (mm-dissect-buffer (current-buffer)))
               (handle (mm-handle-type mime-info))
               (charset (rfc2231-get-value handle 'charset)))
    (intern (downcase charset))))

(defun rt--format-request-parameters (&optional params)
  "Correctly format the request PARAMS, eg. key1=value1&key2=value2."
  (mapconcat (lambda (param)
               (format "%s=%s" (car param) (cdr param)))
             params
             "&"))

(defun rt--format-request-url (&optional endpoint params)
  "Properly format the request url for ENDPOINT with parameters PARAMS."
  (let ((parameters (rt--format-request-parameters params)))
    (url-generic-parse-url
     (format "%s/%s/?%s" rt--api-base-uri endpoint parameters))))

(defun rt--request-endpoint (endpoint &optional headers params)
  "Request ENDPOINT from the api, with optional extra HEADERS and PARAMS."
  (let* ((parameters (rt--merge-defaults-alist rt--default-parameters params))
         (url (rt--format-request-url endpoint parameters))
         (extra-headers (or headers rt--default-headers))
         (url-request-method "GET")
         (url-request-extra-headers extra-headers))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (let ((charset (rt--get-response-charset))
            (body (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))
            (json-key-type 'symbol)
            (json-array-type 'list)
            (json-object-type 'alist))
        (url-mark-buffer-as-dead (current-buffer))
        (json-read-from-string (decode-coding-string body charset))))))

;; Stations
(defun rt--request-stations (&optional from-cache)
  "Request the current stations list, optionally FROM-CACHE can be set.
Caching is recommended since the station list is unlikely to change during the same session (or ever)."
  (when (not (and from-cache (boundp 'rt--cached-stations)))
    (setq rt--cached-stations
          (alist-get 'station (rt--request-endpoint 'stations nil '(("lang" . rt--default-language))))))
  rt--cached-stations)

(defun rt--stations-station->entry (station)
  (let-alist station
    (let* ((lat (string-to-number .locationY))
           (long (string-to-number .locationX))
           (location (format "lat: %.08f\tlong: %.08f" lat long)))
      `((id . , .id)
        (name . , .name)
        (location . ,location)))))

(defun rt--stations-get-entries (&optional from-cache)
  (mapcar 'rt--stations-station->entry (rt--request-stations
                                        (when (boundp 'from-cache) from-cache))))

(bui-define-interface stations list
  :buffer-name "*<railtime-stations>*"
  :get-entries-function 'rt--stations-get-entries
  :format '((id nil 30 t)
            (name nil 50 t)
            (location nil 50 t))
  :sort-key '(name))

(defun rt-stations ()
  (interactive)
  (bui-get-display-entries 'stations 'list))

;;;  Connections

(defun rt--read-time-input (&optional identifier default-now)
  "Read a time string from the minibuffer using `completing-read'.
Optionally provide an IDENTIFIER that prefixes the prompt.
Optionally set DEFAULT-NOW for the default time-string to be the current time."
  (let ((time nil))
    (while (not time)
      (let* ((input (read-string (concat (when identifier (format "%s " identifier)) "time: ")
                                 (when default-now
                                   (format-time-string "%H:%M"))))
             (split (split-string input ":")))
        (if (>= (length split) 2)
            (let ((hours (string-to-number (nth 0 split)))
                  (minutes (string-to-number (nth 1 split))))
              (if (or (> hours 23) (< hours 0)
                      (> minutes 59) (< minutes 0))
                  (rt--message-with-timeout 5 "Wrong time string. 24-hour clock values only!")
                (setq time (format "%02d%02d" hours minutes))))
          (rt--message-with-timeout 5 "Wrong time string. Format is 24-hour %%H:%%M, eg. 12:30")
          )))
    time))

(defun rt--completing-read-alist (prompt collection &rest args)
  (let* ((input (apply 'completing-read prompt collection args))
         (val (assoc input collection)))
    (if val (cdr val) input)))

(defun rt--mapcar* (f &rest xs)
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
            (apply 'rt--mapcar* f (mapcar 'cdr xs)))))

(defun rt--prompt-date (&optional days)
  (let ((days '()))
    (dotimes (i (or days 31))
      (setf days (cons (time-add (current-time) (days-to-time i)) days)))
    (let* ((day-seq (nreverse (mapcar (lambda (day) (format-time-string "%x" day)) days)))
           (day-alist (rt--mapcar* #'cons day-seq (nreverse days)))
           (input (rt--completing-read-alist "Date: " day-alist nil t)))
      (format-time-string "%d%m%y" input))))

(defun rt--request-connections (&optional from to timesel type date time)
  (let* ((type (or type 'all))
         (station-names (mapcar (lambda (station) (alist-get 'name station)) (rt--request-stations t)))
         (from (or from (completing-read "From: " station-names nil t rt--default-connection-from)))
         (to (or to (completing-read "To: " station-names nil t rt--default-connection-to)))
         (timesel (or timesel (completing-read "Choose: " '(departure arrival) nil t)))
         (date (or date (rt--prompt-date)))
         (time (or time (rt--read-time-input timesel t)))
         (parameters `(("from" . ,from) ("to" . ,to) ("timesel" . ,timesel)
                       ("typeOfTransport" . ,type) ("date" . ,date) ("time" . ,time))))
    (alist-get 'connection (rt--request-endpoint 'connections nil parameters))))

(defun rt--format-delay-string (delay &optional face)
  (let ((delay (if (numberp delay) delay (string-to-number delay))))
    (when (> delay 0)
      (let ((delay-string (concat "+" (format-seconds "%yy%dd%hh%z%mm" delay))))
        (propertize delay-string 'font-lock-face (or face 'rt--delay-face))))))

(defun rt--format-deparr-time (time &optional delay)
  (concat (format-time-string "%H:%M" (if (numberp time)
                                          time
                                        (string-to-number time))
                              (when delay (rt--format-delay-string delay)))))

(defun rt--format-alerts (alerts entry)
  (let ((alert-count 0))
    (dolist (alert alerts)
      (let ((num (alist-get 'number alert)))
        (when num (setq alert-count (+ alert-count (string-to-number num))))))
    (if (> alert-count 0)
        (format "%s %s" (propertize "⚠" 'font-lock-face 'rt--status-warning-face) alert-count)
      (propertize "✓" 'font-lock-face 'rt--status-good-face))))

(defun rt--format-status (cancellations entry)
  "Format a status string according to a list of CANCELLATIONS.
This is just a list of ones and zeroes indication any cancellations at the
departure- or arrival-end of the connection."
  (if (seq-every-p #'zerop (mapcar 'string-to-number cancellations))
      "OK"
    (propertize "Cancelled" 'font-lock-face 'rt--status-warning-face)))

(defun rt--connections-connection->entry (connection)
  (let-alist connection
    (let* ((dep-delay (rt--format-delay-string .departure.delay))
           (arr-delay (rt--format-delay-string .arrival.delay))
           (departure (rt--format-deparr-time .departure.time dep-delay))
           (arrival (rt--format-deparr-time .arrival.time arr-delay)))
      `((id . , .id)
        (departure . ,departure)
        (arrival . ,arrival)
        (duration . ,(format-seconds "%Y %D %H %z%M" (string-to-number .duration)))
        (vias . , (if .vias .vias.number 0))
        (dep-platform . , .departure.platform)
        (arr-platform . , .arrival.platform)
        (status . (, .departure.canceled , .arrival.canceled))
        (alerts . , .alerts)))))

(defun rt--connections-get-entries ()
  "Get all connections formatted properly as BUI-buffer entries."
  (mapcar 'rt--connections-connection->entry (rt--request-connections)))

(bui-define-interface connections list
  :buffer-name "*<railtime-connections>*"
  :get-entries-function 'rt--connections-get-entries
  :titles '((dep-platform . "Platform")
            (arr-platform . "Platform"))
  :format '((id nil 10 t)
            (departure nil 10 t)
            (dep-platform nil 10 t)
            (arrival nil 10 t)
            (arr-platform nil 10 t)
            (duration nil 30 t)
            (vias nil 5 t)
            (status rt--format-status 15 t)
            (alerts rt--format-alerts 10 t))
  :sort-key '(id))

(defun rt-connections ()
  (interactive)
  (bui-get-display-entries 'connections 'list))

(provide 'railtime)
;;; railtime.el ends here
