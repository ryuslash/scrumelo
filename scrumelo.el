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
        (scrumelo--js scrumelo-jquery-js-location)))

(defun scrumelo--story ()
  "Return a description of the current org heading as a scrum story."
  (format "As a %s, I %s to %s" (org-entry-get (point) "Role")
          (org-entry-get (point) "Necessity")
          (nth 4 (org-heading-components))))

(defun scrumelo--story-row ()
  "Return a table row for the current org headline."
  `(tr (td ,(org-entry-get (point) "TODO"))
       (td ,(scrumelo--story))))

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
                    ,(scrumelo--story-table buffer)))))))))

(defun scrumelo-handler (httpcon)
  "Send the right requests in HTTPCON to the right functions."
  (elnode-dispatcher
   httpcon
   '(("^/$" . scrumelo-backlog-page))))

(elnode-start 'scrumelo-handler :port 8028 :host "localhost")

(provide 'scrumelo)
;;; scrumelo.el ends here
