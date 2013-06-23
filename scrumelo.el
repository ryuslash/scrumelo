;;; scrumelo.el --- Scrum with elnode and org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: tools, hypermedia, outlines, comm

;;; Commentary:

;; A scrum web app.

(require 'cl-lib)
(require 'elnode)
(require 'esxml)
(require 'org)

(eval-when-compile
  (require 'cl-macs))

;;; Code:

(defvar scrumelo-project-file "~/projects/scrumelo/aeos.org"
  "The file containing the scrum backlog.")

(defvar scrumelo--base-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory)
  "The current directory.")

(defvar scrumelo-bootstrap-css-location
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css"
  "The location of the twitter bootstrap CSS file.")

(defvar scrumelo-bootstrap-js-location
  "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/js/bootstrap.min.js"
  "The location of the twitter bootstrap JS file.")

(defvar scrumelo-font-awesome-css-location
  "http://netdna.bootstrapcdn.com/font-awesome/3.1.1/css/font-awesome.min.css"
  "The location of the font awesome CSS file.")

(defvar scrumelo-jquery-js-location
  "http://code.jquery.com/jquery-2.0.0.min.js"
  "The location of the jQuery JS file.")

(defvar scrumelo-react-js-location
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/react.min.js"
  "The location of the React JS file.")

(defvar scrumelo-jsxtransformer-js-location
  "http://cdnjs.cloudflare.com/ajax/libs/react/0.3.2/JSXTransformer.js"
  "The location of the JSX Transformer JS file.")

(defmacro editing-scrumelo-story (id after &rest body)
  "Edit the story with ID.

Goto the story with ID and execute AFTER after executing BODY and
saving the buffer."
  (declare (indent 2))
  (let ((entry-var (cl-gensym)))
   `(with-scrumelo-buffer
      (let ((,entry-var (cdr (org-id-find ,id))))
        (goto-char ,entry-var)
        ,@body
        (save-buffer)
        ,after))))

(defmacro with-scrumelo-http-params (params httpcon &rest body)
  "Bind parameters PARAMS from HTTPCON and execute BODY."
  (declare (indent 2))
  `(let (,@(mapcar (lambda (p)
                     `(,p (elnode-http-param ,httpcon ,(symbol-name p))))
                   params))
     ,@body))

(defmacro with-scrumelo-buffer (&rest body)
  "Set the current buffer to `scrumelo-project-file' and run BODY."
  (declare (indent 0))
  `(with-current-buffer (find-file-noselect scrumelo-project-file)
     ,@body))

(defun scrumelo--css (href)
  "Return a link pointing to HREF."
  `(link (@ (href ,href) (rel "stylesheet") (type "text/css"))))

(defun scrumelo--css-list ()
  "Return a list of all required CSS files."
  (list (scrumelo--css scrumelo-bootstrap-css-location)
        (scrumelo--css scrumelo-font-awesome-css-location)))

(defun scrumelo--js (src)
  "Return a script sourcing SRC."
  `(script (@ (src ,src) (language "JavaScript")
              (type "text/javascript")) ""))

(defun scrumelo--js-list ()
  "Return a list of all required JS files."
  (list (scrumelo--js scrumelo-bootstrap-js-location)
        (scrumelo--js scrumelo-jquery-js-location)
        (scrumelo--js scrumelo-react-js-location)
        (scrumelo--js scrumelo-jsxtransformer-js-location)
        (scrumelo--js "js/scrumelo.js")))

(defun scrumelo-backlog-page (httpcon)
  "Send the backlog overview over HTTPCON."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
  (elnode-http-return
   httpcon
   (concat
    "<!DOCTYPE html>\n"
    (sxml-to-xml
     `(html (head (title "Scrumelo")
                  ,@(scrumelo--css-list)
                  ,@(scrumelo--js-list))
            (body
             (div (@ (class "container"))
                  (h1 "Backlog")
                  (div (@ (id "content")) "")
                  (script (@ (type "text/jsx") (src "js/main.js")) ""))))))))

(defun scrumelo-new-story (httpcon)
  "Parse data from HTTPCON and write a new scrum story using it."
  (elnode-method httpcon
    (POST
     (with-scrumelo-http-params (role necessity headline) httpcon
       (with-scrumelo-buffer
         (goto-char (point-max))
         (insert "\n* TODO " headline)
         (org-set-property "Role" role)
         (org-set-property "Necessity" necessity)
         (org-set-property "ID" (org-id-new "scrumelo"))
         (save-buffer)))
     (elnode-send-redirect httpcon "/"))))

(defun scrumelo-change-state (httpcon)
  "Parse data from HTTPCON and change the given task's state."
  (elnode-method httpcon
    (POST
     (with-scrumelo-http-params (id) httpcon
       (editing-scrumelo-story id
           (scrumelo--send-json
            httpcon `((:state . ,(org-entry-get (point) "TODO"))))
         (org-todo))))))

(defun scrumelo-move-story (dir)
  "Create a function to move a story in direction DIR."
  (let ((func (intern (concat "org-move-subtree-" dir))))
    (lambda (httpcon)
      (elnode-method httpcon
        (POST
         (with-scrumelo-http-params (id) httpcon
           (editing-scrumelo-story id
               (scrumelo--send-json httpcon '((:status . "OK")))
             (funcall func))))))))

(defun scrumelo--send-json (httpcon obj)
  "Respond to HTTPCON with OBJ converted to a json structure."
  (elnode-http-start httpcon 200 '("Content-Type" . "text/json"))
  (elnode-http-return httpcon (json-encode obj)))

(defun scrumelo-story-json (httpcon)
  "Repsond to HTTPCON with some json info about a story."
  (let* ((story (match-string 1 (elnode-http-mapping httpcon)))
         (entry (cdr (org-id-find story))))
    (with-scrumelo-buffer
      (goto-char entry)
      (scrumelo--send-json
       httpcon (list (cons 'Assignee (org-entry-get (point) "Assignee"))
                     (cons 'content (buffer-substring-no-properties
                                     (org-end-of-meta-data-and-drawers)
                                     (org-entry-end-position))))))))

(defun scrumelo--org-entry-to-list ()
  "Turn an org-entry to json."
  (let ((components (org-heading-components)))
   (when (= (car components) 1)
     `((:id . ,(org-id-get))
       (:state . ,(org-entry-get (point) "TODO"))
       (:role . ,(org-entry-get (point) "Role"))
       (:necessity . ,(org-entry-get (point) "Necessity"))
       (:title . ,(nth 4 components))))))

(defun scrumelo-main-json (request)
  "Respond to REQUEST with the json info for the main page."
  (with-scrumelo-buffer
    (scrumelo--send-json
     request  (cl-map 'vector #'identity
                      (delq nil
                            (org-map-entries
                             #'scrumelo--org-entry-to-list
                             nil nil 'comment))))))

(defun scrumelo--verify-credentials (audience assertion)
  "Make sure AUDIENCE and ASSERTION are correct."
  (let ((url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (json-encode `(("assertion" . ,assertion)
                                         ("audience" . ,audience))))
        (url-request-method "POST")
        result)
    (with-current-buffer
        (url-retrieve-synchronously "https://verifier.login.persona.org/verify")
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq result (json-read))
      (kill-buffer))
    result))

(defun scrumelo-login-page (httpcon)
  "Show a login link for persona for HTTPCON."
  (elnode-method httpcon
    (GET
     (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
     (elnode-http-return
      httpcon
      (concat
       "<!DOCTYPE html>\n"
       (sxml-to-xml
        '(html (@ (lang "en"))
               (head (meta (@ (charset "utf-8")))
                     (title "Login")
                     (script (@ (src "https://login.persona.org/include.js")) "")
                     (script (@ (src "/js/login.js")) ""))
               (body
                (form (@ (id "login-form")
                         (method "POST")
                         (action ""))
                      (input (@ (id "assertion-field")
                                (type "hidden")
                                (name "assertion")
                                (value ""))))
                (p (a (@ (href "javascript:login()")) "Login"))))))))
    (POST
     (let* ((audience "http://localhost:8028")
            (assertion (elnode-http-param httpcon "assertion"))
            (result (scrumelo--verify-credentials audience assertion)))
       (if (equal (cdr (assoc 'status result)) "okay")
           (progn
             (elnode-http-start httpcon 302
                                '("Content-Type" . "text/html")
                                '("Location" . "/"))
             (elnode-send-redirect httpcon "/"))
         (elnode-send-status httpcon 403 "Not allowed"))))))

(defun scrumelo-handler (httpcon)
  "Send the right requests in HTTPCON to the right functions."
  (elnode-dispatcher
   httpcon
   `(("^/$" . scrumelo-backlog-page)
     ("^/js/main.js" . ,(elnode-make-send-file
                         (concat scrumelo--base-dir "js/main.js")))
     ("^/js/login.js" . ,(elnode-make-send-file
                          (concat scrumelo--base-dir "js/login.js")))
     ("^/login/$" . scrumelo-login-page)
     ("^/stories/$" . scrumelo-main-json)
     ("^/stories/new/$" . scrumelo-new-story)
     ("^/stories/state/$" . scrumelo-change-state)
     ("^/stories/up/$" . ,(scrumelo-move-story "up"))
     ("^/stories/down/$" . ,(scrumelo-move-story "down"))
     ("^/stories/\\([a-z0-9:-]+\\)/$" . scrumelo-story-json))))

(elnode-start 'scrumelo-handler :port 8028 :host "0.0.0.0")

(provide 'scrumelo)
;;; scrumelo.el ends here
