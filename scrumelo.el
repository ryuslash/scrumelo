;;; scrumelo.el --- Scrum with elnode and org-mode

;; Copyright (C) 2013  Tom Willemse

;; Author: Tom Willemse <tom@ryuslash.org>
;; Keywords: tools, hypermedia, outlines, comm

;;; Commentary:

;; A scrum web app.

(require 'elnode)
(require 'esxml)
(require 'org)

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

(defmacro with-scrumelo-http-params (params httpcon &rest body)
  "Bind parameters PARAMS from HTTPCON and execute BODY."
  `(let (,@(mapcar (lambda (p)
                     `(,p (elnode-http-param ,httpcon ,(symbol-name p))))
                   params))
     ,@body))
(put 'with-scrumelo-http-params 'lisp-indent-function 2)

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
        (scrumelo--js "js/scrumelo.js")))

(defun scrumelo--story ()
  "Return a description of the current org heading as a scrum story."
  (format "As a %s, I %s to %s" (org-entry-get (point) "Role")
          (org-entry-get (point) "Necessity")
          (nth 4 (org-heading-components))))

(defun scrumelo--task-state-class (state)
  "Return the correct icon class for STATE."
  (cdr (assoc state '(("TODO" . "icon-check-empty")
                      ("DOING" . "icon-sign-blank")
                      ("DONE" . "icon-check")))))

(defun scrumelo--story-row ()
  "Return a table row for the current org headline."
  (let ((state (org-entry-get (point) "TODO")))
    `(tr (td (i (@ (class ,(scrumelo--task-state-class state))) "")
             " " ,state)
         (td (a (@ (id ,(org-id-get))
                   (onclick "return get_story_info(this)"))
                ,(scrumelo--story))))))

(defun scrumelo--maybe-story-row ()
  "If looking at a top level heading, return a table row for it."
  (when (= (car (org-heading-components)) 1)
    (scrumelo--story-row)))

(defun scrumelo--inner-story-table (buffer)
  "Return the inner part of the story table for BUFFER."
  (with-current-buffer buffer
    (delq nil (org-map-entries
               'scrumelo--maybe-story-row nil nil 'comment))))

(defun scrumelo--story-table (buffer)
  "Return the story table for BUFFER."
  `(table (@ (class "table table-striped"))
          ,@(scrumelo--inner-story-table buffer)))

(defun scrumelo--new-story-form ()
  "Create a form for adding new stories."
  `(form (@ (method "POST")
            (action "/stories/new/"))
         (fieldset
          (legend (@ (class "toggle")
                     (data-show "new-story")) "New story")
          (div (@ (id "new-story")
                  (class "hide")
                  (style "text-align: center;"))
               (div (@ (class "input-prepend input-append"))
                    (span (@ (class "add-on")) "As a ")
                    (input (@ (class "input-medium") (type "text")
                              (name "role")))
                    (span (@ (class "add-on")) " I ")
                    (input (@ (class "input-mini") (type "text")
                              (name "necessity")))
                    (span (@ (class "add-on")) " to ")
                    (input (@ (class "input-xxlarge") (type "text")
                              (name "headline")))
                    (button (@ (class "btn") (type "submit")) "!"))))))

(defun scrumelo-backlog-page (httpcon)
  "Send the backlog overview over HTTPCON."
  (let ((buffer (find-file-noselect scrumelo-project-file)))
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
                    ,(scrumelo--story-table buffer)
                    ,(scrumelo--new-story-form)))))))))

(defun scrumelo-new-story (httpcon)
  "Parse data from HTTPCON and write a new scrum story using it."
  (elnode-method httpcon
    (POST
     (let ((buffer (find-file-noselect scrumelo-project-file)))
       (with-scrumelo-http-params (role necessity headline) httpcon
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert "\n* TODO " headline)
           (org-set-property "Role" role)
           (org-set-property "Necessity" necessity)
           (save-buffer))))
     (elnode-send-redirect httpcon "/"))))

(defun scrumelo--send-json (httpcon obj)
  "Respond to HTTPCON with OBJ converted to a json structure."
  (elnode-http-start httpcon 200 '("Contaent-Type" . "text/json"))
  (elnode-http-return httpcon (json-encode obj)))

(defun scrumelo-story-json (httpcon)
  "Repsond to HTTPCON with some json info about a story."
  (let* ((story (match-string 1 (elnode-http-mapping httpcon)))
         (buffer (find-file-noselect scrumelo-project-file))
         (entry (cdr (org-id-find story))))
    (message "HI: %s" story)
    (with-current-buffer buffer
      (goto-char entry)
      (scrumelo--send-json
       httpcon (list (cons 'Assignee (org-entry-get (point) "Assignee"))
                     (cons 'content (buffer-substring-no-properties
                                     (org-end-of-meta-data-and-drawers)
                                     (org-entry-end-position))))))))

(defun scrumelo-handler (httpcon)
  "Send the right requests in HTTPCON to the right functions."
  (elnode-dispatcher
   httpcon
   `(("^/$" . scrumelo-backlog-page)
     ("^/js/scrumelo.js" . ,(elnode-make-send-file
                             (concat scrumelo--base-dir "js/scrumelo.js")))
     ("^/stories/new/$" . scrumelo-new-story)
     ("^/stories/\\([a-z0-9:-]+\\)/" . scrumelo-story-json))))

(elnode-start 'scrumelo-handler :port 8028 :host "localhost")

(provide 'scrumelo)
;;; scrumelo.el ends here
