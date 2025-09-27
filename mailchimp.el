;;; mailchimp.el --- Mailchimp API integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Sacha Chua <sacha@sachachua.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A simple unofficial Emacs Lisp library to interact with the Mailchimp API.
;;
;; To use this library, set your API key. The API key
;; format is "<key>-<server-prefix>", e.g., "1234567890abcdef1234567890abcdef-us18".
;; You can configure the `mailchimp-api-key` variable in your Emacs configuration:
;;
;; (setq mailchimp-api-key "YOUR_API_KEY_GOES_HERE")
;;
;; Functions provided:
;; - `mailchimp-upload-file`: Uploads a file to your Mailchimp account.
;; - `mailchimp-get-recent-files`: Retrieves a list of recently uploaded files.

;;; Code:

(require 'url)
(require 'json)
(require 'base64)

(defgroup mailchimp nil
  "Customization options for the Mailchimp library."
  :group 'tools)

(defcustom mailchimp-api-key ""
  "Mailchimp API key.
Generate an API key on the Mailchimp website with Account > Extras > API keys."
  :type 'string
  :group 'mailchimp)

(defun mailchimp-server-prefix ()
  "Get the server prefix from `mailchimp-api-key'."
  (let ((parts (split-string mailchimp-api-key "-")))
    (if (and (listp parts) (= (length parts) 2))
        (nth 1 parts)
      (error "Invalid Mailchimp API key format"))))

(cl-defun mailchimp--request-json (path &key (method "GET") (body nil))
  "Make an HTTP request to the Mailchimp API.
PATH is the API endpoint, e.g., 'files'.
METHOD is the HTTP method, e.g., \"GET\" or \"POST\".
BODY is an optional request body (string or JSON).
Return a JSON object."
  (unless mailchimp-api-key
    (error "Mailchimp API key is not set. Please set `mailchimp-api-key`."))

  (let* ((server-prefix (mailchimp-server-prefix))
         (api-key-only (nth 0 (split-string mailchimp-api-key "-")))
         (url (if (string-match "^https:" path)
									path (format "https://%s.api.mailchimp.com/3.0/%s" server-prefix path)))
         (auth-header (concat "anystring:" mailchimp-api-key))
         (url-request-extra-headers
					`(("Authorization" .
						 ,(concat "Basic "
											(base64-encode-string auth-header)))
						("Content-Type" . "application/json")))
         (url-request-data (cond
														((stringp body) body)
														((null body) body)
														(t (encode-coding-string (json-encode body) 'utf-8))))
         (url-request-method method))
    (with-current-buffer (url-retrieve-synchronously
													url)
			(goto-char url-http-end-of-headers)
			(json-read))))

;;;###autoload
(defun mailchimp-upload-file (filename)
  "Upload a file to Mailchimp's Content Manager.
FILENAME is the path to the file to be uploaded.
The file content is base64-encoded and sent in the request body."
  (interactive "fFile to upload: ")
  (let ((file-name (file-name-nondirectory filename)))
    (unless (file-exists-p filename)
      (error "File does not exist: %s" filename))
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (let ((body (json-encode `((name . ,(file-name-nondirectory file-name))
                                 (file_data . ,(base64-encode-string (buffer-string)))))))
				(mailchimp--request-json "file-manager/files"
																 :method "POST"
																 :body body)))))

;;;###autoload
(defun mailchimp-recent-files (&optional n offset)
  "Get a list of recently uploaded files from Mailchimp.
N is an optional number to limit the count of recent files returned (default: 100).
OFFSET is the starting offset."
	(setq n (or n 100))
	(setq offset (or offset 0))
	(mailchimp--request-json
	 (format "file-manager/files?count=%d&sort_field=added_date&sort_dir=DESC&offset=%d" n offset)))

(defun mailchimp-campaigns (&optional n offset)
	"Get a list of recent campaigns from Mailchimp.
N is an optional number to limit the count of recent files returned (default: 10).
OFFSET is the starting offset."
	(setq n (or n 10))
	(setq offset (or offset 0))
	(mailchimp--request-json
	 (format "campaigns?count=%d&sort_field=create_time&sort_dir=DESC&offset=%d" n offset)))

(provide 'mailchimp)

;;; mailchimp.el ends here
