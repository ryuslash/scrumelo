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

(defvar scrum-project-file "~/projects/scrumelo/aeos.org"
  "The file containing the scrumm backlog.")

(defun scrumelo-backlog-page (httpcon)
  "Send the backlog overview over HTTPCON."
  (let ((buffer (find-file-noselect scrum-project-file)))
    (elnode-http-start httpcon 200 '("Content-Type" . "text/html"))
    (elnode-http-return
     httpcon
     (concat
      "<!DOCTYPE html>\n"
      (sxml-to-xml
       `(html (head (title "Scrumelo")
                    (link (@ (href "http://ryuslash.org/bootstrap2/css/bootstrap.min.css")
                     (type "text/css") (rel "stylesheet"))))
              (body
               (div (@ (class "container"))
                (table (@ (class "table"))
                       ,@(with-current-buffer buffer
                           (delq nil
                                 (org-map-entries
                                  (lambda ()
                                    (when (= (car (org-heading-components)) 1)
                                      `(tr (td ,(org-entry-get (point) "TODO"))
                                           (td ,(format "As a %s, I %s to %s"
                                                        (org-entry-get (point) "Role")
                                                        (org-entry-get (point) "Necessity")
                                                        (nth 4 (org-heading-components))))))) nil nil 'comment))))))))))))

(defun scrumelo-handler (httpcon)
  "Send the right requests in HTTPCON to the right functions."
  (elnode-dispatcher
   httpcon
   '(("^/$" . scrumelo-backlog-page))))

(elnode-start 'scrumelo-handler :port 8028 :host "localhost")

(provide 'scrumelo)
;;; scrumelo.el ends here
