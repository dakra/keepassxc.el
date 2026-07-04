;;; keepassxc-mock-server.el --- Mock KeePassXC server for tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Kraus <daniel@kraus.my>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A minimal in-Emacs implementation of the KeePassXC browser
;; protocol, used by the ERT test suite.  It listens on a unix domain
;; socket and uses the real sodium crypto, so the client code under
;; test runs unmodified.
;;
;; Fault injection:
;; - `chunk-mode' `split': responses are sent in two chunks with a
;;   small delay, exercising the client's partial-read buffering.
;; - `chunk-mode' `coalesce': every response is concatenated with an
;;   unsolicited "database-unlocked" signal in a single write,
;;   exercising back-to-back message parsing.
;; - `fail-with': reply to the next request with the given errorCode.
;; - `corrupt-nonce': reply with a wrong nonce.

;;; Code:

(require 'cl-lib)
(require 'sodium)

(cl-defstruct (keepassxc-mock (:constructor keepassxc-mock--create)
                              (:copier nil))
  server-proc socket-path keypair
  client-key client-proc
  associations   ; alist of (ID . ID-KEY)
  entries        ; alist of (URL . list of entry plists)
  db-hash locked
  chunk-mode     ; nil, `split' or `coalesce'
  fail-with      ; errorCode integer for the next request
  corrupt-nonce  ; when non-nil, reply with a wrong nonce
  plaintext      ; when non-nil, send replies unencrypted
  hold           ; when non-nil, queue replies instead of sending them
  held           ; queued (PROC . STRING) replies, newest first
  requests)      ; list of (ACTION . MESSAGE-HASH-TABLE), newest first

(defun keepassxc-mock-start (socket-path &rest options)
  "Start a mock KeePassXC server listening on SOCKET-PATH.
OPTIONS is a plist supporting :entries, :db-hash, :locked and
:associations.  Return the mock object."
  (let ((mock (keepassxc-mock--create
               :socket-path socket-path
               :keypair (sodium-box-keypair)
               :db-hash (or (plist-get options :db-hash) "MOCKHASH")
               :entries (plist-get options :entries)
               :locked (plist-get options :locked)
               :associations (plist-get options :associations))))
    (setf (keepassxc-mock-server-proc mock)
          (make-network-process
           :name "keepassxc-mock"
           :server t
           :family 'local
           :service socket-path
           :coding 'utf-8-unix
           :filter (lambda (proc chunk)
                     (keepassxc-mock--filter mock proc chunk))
           :sentinel (lambda (proc event)
                       (when (string-prefix-p "open" event)
                         (setf (keepassxc-mock-client-proc mock) proc)))
           :noquery t))
    mock))

(defun keepassxc-mock-stop (mock)
  "Stop the mock server MOCK and close all its connections."
  (when-let* ((proc (keepassxc-mock-client-proc mock)))
    (when (process-live-p proc)
      (delete-process proc)))
  (when-let* ((proc (keepassxc-mock-server-proc mock)))
    (when (process-live-p proc)
      (delete-process proc))))

(defun keepassxc-mock-requests-for (mock action)
  "Return all messages MOCK received for ACTION, oldest first."
  (nreverse
   (mapcar #'cdr
           (seq-filter (lambda (req) (equal (car req) action))
                       (keepassxc-mock-requests mock)))))

(defun keepassxc-mock-send-signal (mock action)
  "Send the unsolicited signal ACTION to MOCK's connected client."
  (when-let* ((proc (keepassxc-mock-client-proc mock)))
    (when (process-live-p proc)
      (process-send-string proc (json-serialize `(:action ,action))))))

(defun keepassxc-mock--filter (mock proc chunk)
  "Parse CHUNK arriving on PROC and handle complete messages for MOCK."
  (let ((pending (concat (or (process-get proc 'keepassxc-mock-pending) "")
                         chunk))
        (rest "")
        messages)
    (with-temp-buffer
      (insert pending)
      (goto-char (point-min))
      (catch 'done
        (while t
          (skip-chars-forward " \t\n\r")
          (when (eobp) (throw 'done nil))
          (let ((start (point)))
            (condition-case nil
                (push (json-parse-buffer) messages)
              (json-end-of-file
               (setq rest (buffer-substring start (point-max)))
               (throw 'done nil)))))))
    (process-put proc 'keepassxc-mock-pending rest)
    (dolist (msg (nreverse messages))
      (keepassxc-mock--handle mock proc msg))))

(defun keepassxc-mock-flush (mock)
  "Send all replies held back by MOCK's `hold' mode."
  (dolist (item (nreverse (keepassxc-mock-held mock)))
    (when (process-live-p (car item))
      (process-send-string (car item) (cdr item))))
  (setf (keepassxc-mock-held mock) nil))

(defun keepassxc-mock--send-raw (mock proc string)
  "Send STRING to PROC honoring MOCK's `chunk-mode' and `hold'."
  (if (keepassxc-mock-hold mock)
      (push (cons proc string) (keepassxc-mock-held mock))
    (pcase (keepassxc-mock-chunk-mode mock)
      ('split
       (let ((mid (max 1 (/ (length string) 2))))
         (process-send-string proc (substring string 0 mid))
         (run-at-time 0.05 nil
                      (lambda ()
                        (when (process-live-p proc)
                          (process-send-string proc
                                               (substring string mid)))))))
      ('coalesce
       (process-send-string
        proc (concat string (json-serialize '(:action "database-unlocked")))))
      (_ (process-send-string proc string)))))

(defun keepassxc-mock--send (mock proc plist)
  "Serialize PLIST and send it to PROC for MOCK."
  (keepassxc-mock--send-raw mock proc (json-serialize plist)))

(defun keepassxc-mock--error-reply (mock proc action code)
  "Send an unencrypted error reply for ACTION with error CODE."
  (keepassxc-mock--send mock proc
                        `(:action ,action
                          :errorCode ,(number-to-string code)
                          :error "mock error")))

(defun keepassxc-mock--reply-nonce (mock nonce)
  "Return the reply nonce for request NONCE, honoring `corrupt-nonce'."
  (let ((incremented (sodium-increment nonce)))
    (if (keepassxc-mock-corrupt-nonce mock)
        (sodium-increment incremented)
      incremented)))

(defun keepassxc-mock--respond (mock proc action nonce fields)
  "Send an encrypted reply for ACTION with FIELDS plist.
NONCE is the request nonce; the reply uses its increment.  With
`plaintext' set on MOCK, send the reply unencrypted instead (a
protocol violation the client must reject)."
  (if (keepassxc-mock-plaintext mock)
      (keepassxc-mock--send mock proc
                            (append `(:action ,action
                                      :version "2.8.0"
                                      :success "true"
                                      :nonce ,(keepassxc-mock--reply-nonce
                                               mock nonce))
                                    fields))
    (keepassxc-mock--respond-encrypted mock proc action nonce fields)))

(defun keepassxc-mock--respond-encrypted (mock proc action nonce fields)
  "Send an encrypted reply for ACTION with FIELDS plist and NONCE."
  (let* ((reply-nonce (keepassxc-mock--reply-nonce mock nonce))
         (inner (json-serialize (append `(:action ,action
                                          :version "2.8.0"
                                          :success "true"
                                          :nonce ,reply-nonce)
                                        fields)))
         (cipher (sodium-box inner reply-nonce
                             (keepassxc-mock-client-key mock)
                             (alist-get 'sk (keepassxc-mock-keypair mock)))))
    (keepassxc-mock--send-raw
     mock proc
     (json-serialize `(:action ,action
                       :message ,cipher
                       :nonce ,reply-nonce)))))

(defun keepassxc-mock--handle (mock proc msg)
  "Handle one parsed client message MSG arriving on PROC for MOCK."
  (let ((action (gethash "action" msg))
        (nonce (gethash "nonce" msg)))
    (push (cons action msg) (keepassxc-mock-requests mock))
    (cond
     ((keepassxc-mock-fail-with mock)
      (let ((code (keepassxc-mock-fail-with mock)))
        (setf (keepassxc-mock-fail-with mock) nil)
        (keepassxc-mock--error-reply mock proc action code)))
     ((equal action "change-public-keys")
      (setf (keepassxc-mock-client-key mock) (gethash "publicKey" msg)
            (keepassxc-mock-client-proc mock) proc)
      (keepassxc-mock--send mock proc
                            `(:action ,action
                              :version "2.8.0"
                              :publicKey ,(alist-get
                                           'pk (keepassxc-mock-keypair mock))
                              :nonce ,(keepassxc-mock--reply-nonce mock nonce)
                              :success "true")))
     (t (keepassxc-mock--handle-encrypted mock proc msg action nonce)))))

(defun keepassxc-mock--handle-encrypted (mock proc msg action nonce)
  "Decrypt and handle the encrypted request MSG (ACTION, NONCE) on PROC."
  (let* ((inner (json-parse-string
                 (sodium-box-open (gethash "message" msg) nonce
                                  (keepassxc-mock-client-key mock)
                                  (alist-get 'sk (keepassxc-mock-keypair mock)))))
         (respond (lambda (fields)
                    (keepassxc-mock--respond mock proc action nonce fields)))
         (fail (lambda (code)
                 (keepassxc-mock--error-reply mock proc action code))))
    (push (cons (concat "inner:" action) inner) (keepassxc-mock-requests mock))
    (if (and (keepassxc-mock-locked mock)
             (not (member action '("lock-database" "request-autotype"))))
        (funcall fail 1)
      (pcase action
        ("get-databasehash"
         (funcall respond `(:hash ,(keepassxc-mock-db-hash mock))))
        ("associate"
         (let ((id (format "mock-assoc-%d"
                           (length (keepassxc-mock-associations mock))))
               (id-key (gethash "idKey" inner)))
           (push (cons id id-key) (keepassxc-mock-associations mock))
           (funcall respond `(:id ,id :hash ,(keepassxc-mock-db-hash mock)))))
        ("test-associate"
         (let* ((id (gethash "id" inner))
                (key (gethash "key" inner))
                (stored (cdr (assoc id (keepassxc-mock-associations mock)))))
           (if (and stored (equal stored key))
               (funcall respond `(:id ,id :hash ,(keepassxc-mock-db-hash mock)))
             (funcall fail 8))))
        ("get-logins"
         (let* ((url (gethash "url" inner))
                (entries (cdr (assoc url (keepassxc-mock-entries mock)))))
           (if entries
               (funcall respond `(:entries ,(vconcat entries)
                                  :count ,(length entries)))
             (funcall fail 15))))
        ("set-login" (funcall respond nil))
        ("get-totp" (funcall respond '(:totp "123456")))
        ("generate-password"
         (funcall respond '(:password "mock-generated-password")))
        ("get-database-groups"
         (funcall respond `(:groups (:groups ,(vector '(:name "Root"
                                                        :uuid "root-uuid"))))))
        ("create-new-group"
         (funcall respond `(:name ,(gethash "groupName" inner)
                            :uuid "mock-group-uuid")))
        ("get-database-entries"
         ;; Like the real server: only title, uuid and url per entry.
         (funcall respond
                  `(:entries
                    ,(vconcat
                      (mapcan (lambda (site)
                                (mapcar (lambda (entry)
                                          (list :title (plist-get entry :name)
                                                :uuid (plist-get entry :uuid)
                                                :url (car site)))
                                        (cdr site)))
                              (keepassxc-mock-entries mock))))))
        ("delete-entry" (funcall respond nil))
        ("request-autotype" (funcall respond nil))
        ("lock-database"
         (setf (keepassxc-mock-locked mock) t)
         (funcall respond nil))
        (_ (funcall fail 12))))))

(provide 'keepassxc-mock-server)
;;; keepassxc-mock-server.el ends here
