;;; git-share.el --- Stores a web link to git repository at current point  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: vc, tools
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://git.sr.ht/~mgmarlow/git-share

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;; V2 Goals:
;; (a) remove cl-lib
;; (b) simplify API
;; (c) add support for more forges

;;; Code:

(require 'vc-git)

(defgroup git-share ()
  "Generate links to git repositories directly from source."
  :group 'tools)

(defcustom git-share-open-links-in-browser nil
  "If non-nil, opens links in your default browser when copied."
  :group 'git-share
  :type 'boolean)

(defun git-share--forge (remote-url)
  "Return source forge from REMOTE-URL."
  (cond
   ((string-match-p "github.com" remote-url) 'github)
   ((string-match-p "git.sr.ht" remote-url) 'sourcehut)
   ((string-match-p "gitlab.com" remote-url) 'gitlab)
   ((string-match-p "codeberg.org" remote-url) 'codeberg)
   ((string-match-p "bitbucket.org" remote-url) 'bitbucket)
   ((string-match-p "savannah.gnu.org" remote-url) 'savannah)
   ((string-match-p "sv.gnu.org" remote-url) 'savannah)
   (t (error "Unsupported git remote %s" remote-url))))

;;;; Formatter function creators

(defun git-share--build-line-func (forge)
  (intern (concat "git-share--" (symbol-name forge) "-format-line")))

(defmacro git-share--create-line-formatter (forge formatter)
  `(defun ,(git-share--build-line-func forge) (base-url branch filename line)
     (format ,formatter base-url branch filename line)))

(defun git-share--build-region-func (forge)
  (intern (concat "git-share--" (symbol-name forge) "-format-region")))

(defmacro git-share--create-region-formatter (forge formatter)
  `(defun ,(git-share--build-region-func forge) (base-url branch filename start end)
     (format ,formatter base-url branch filename start end)))

;;; Line formatters
(git-share--create-line-formatter github "%s/blob/%s/%s#L%s")
(git-share--create-line-formatter sourcehut "%s/tree/%s/item/%s#L%s")
(git-share--create-line-formatter gitlab "%s/-/blob/%s/%s#L%s")
(git-share--create-line-formatter codeberg "%s/src/branch/%s/%s#L%s")
(git-share--create-line-formatter bitbucket "%s/src/%s/%s#lines-%s")

;; Savannah swaps the position of the branch/filename arguments, since
;; branch is a query parameter.
(defun git-share--savannah-format-line (base-url branch filename line)
  (format "%s/tree/%s?h=%s#n%s" base-url filename branch line))

;;; Region formatters
(git-share--create-region-formatter github "%s/blob/%s/%s#L%s-L%s")
(git-share--create-region-formatter sourcehut "%s/tree/%s/item/%s#L%s-%s")
(git-share--create-region-formatter gitlab "%s/-/blob/%s/%s#L%s-%s")
(git-share--create-region-formatter codeberg "%s/src/branch/%s/%s#L%s-L%s")
(git-share--create-region-formatter bitbucket "%s/src/%s/%s#lines-%s:%s")

(defun git-share--savannah-format-region (base-url branch filename start end)
  (error "GNU Savannah does not support region links"))

;;; Commit formatters

;; todo

(defun git-share--format-line (remote-url branch filename loc)
  "Assumes a formatter function that matches `git-share--build-line-func'."
  (let* ((base-url (git-share--base-url remote-url))
         (forge (git-share--forge base-url))
         (func (git-share--build-line-func forge)))
    (url-encode-url (funcall func base-url branch filename loc))))

(defun git-share--format-region (remote-url branch filename start end)
  "Assumes a formatter function that matches `git-share--build-region-func'."
  (let* ((base-url (git-share--base-url remote-url))
         (forge (git-share--forge base-url))
         (func (git-share--build-region-func forge)))
    (url-encode-url (funcall func base-url branch filename start end))))

(defun git-share--maybe-ssh-uri-to-https (remote-url)
  "Convert an SSH REMOTE-URL (git@) to HTTPS, if needed."
  (if (string-prefix-p "git@" remote-url)
      (string-replace "git@" "https://"
                      ;; Assuming no colons in usernames or repos.
                      (string-replace ":" "/" remote-url))
    remote-url))

(defun git-share--post-process-filename (forge filename)
  "Prepare FILENAME for its link format based on FORGE.

Almost all git forges use the same format for their
remote-url/base-path combinations, exempting Savannah which has a
number of edge-cases and differing formats.

* Savannah requires /cgit/ to be appended to FILENAME.

* Savannah HTTPS URIs contain an extraneous /git/ prefix.

* Savannah retains the *.git filename extension, where all other
  providers strip it if present."
  (cond
   ((eq forge 'savannah)
    (concat "/cgit" (if (string-prefix-p "/git/" filename)
                        (substring filename 4 (length filename))
                      filename)))
   ((string-suffix-p ".git" filename) (substring filename 0 -4))
   (t filename)))

(defun git-share--base-url (remote-url)
  "Extract the base-path for a link from REMOTE-URL."
  (let ((forge (git-share--forge remote-url))
        (url (url-generic-parse-url (git-share--maybe-ssh-uri-to-https remote-url))))
    (concat
     "https://"
     (url-host url)
     (git-share--post-process-filename forge (url-filename url)))))

(defun git-share--branch-prompt ()
  (let ((branches (vc-git-branches)))
    (if (= (length branches) 1)
        (car branches)
      (completing-read "Select branch: " branches nil t))))

(defun git-share--copy-link (link)
  "Copy LINK to clipboard.

Opens LINK via `browse-url' if `git-share-open-links-in-browser' is non-nil."
  (kill-new link)
  (when git-share-open-links-in-browser
    (browse-url link))
  (message (concat "Copied " link " to clipboard.")))

;;;###autoload
(defun git-share ()
  "Copy a web link to the LOC at point."
  (interactive)
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (remote-url (vc-git-repository-url filename)))
      (git-share--copy-link
       (if (use-region-p)
           (git-share--format-region
            remote-url
            (git-share--branch-prompt)
            (file-relative-name filename (vc-root-dir))
            (number-to-string (line-number-at-pos (region-beginning)))
            (number-to-string (line-number-at-pos (region-end))))
         (git-share--format-line
          remote-url
          (git-share--branch-prompt)
          (file-relative-name filename (vc-root-dir))
          (number-to-string (line-number-at-pos)))))
    (error "Must be in a git repository")))

;; (cl-defstruct git-share-remote
;;   "Wrapper around `vc-git-repository-url'."
;;   base-url filename rel-filename forge)

;; (defun git-share-remote-from-filename (filename &optional remote-url)
;;   "Create `git-share-remote' from FILENAME.

;; If REMOTE-URL is nil, determines remote URL via
;; `vc-git-repository-url'."
;;   (let* ((remote-url (or remote-url (vc-git-repository-url filename))))
;;     (make-git-share-remote
;;      :base-url (git-share--link-base-url remote-url)
;;      :filename filename
;;      :rel-filename (file-relative-name filename (vc-root-dir))
;;      :forge (git-share--forge-kind remote-url))))

;; (cl-defstruct git-share-forge loc-format-string commit-format-string)

;; (defun git-share--link-base-url (remote-url)
;;   (git-share--maybe-remove-extension
;;    (if (string-prefix-p "https" remote-url)
;;        remote-url
;;      (git-share--ssh-to-https remote-url))))

;; (defun git-share--ssh-to-https (remote-url)
;;   (concat "https://" (replace-regexp-in-string ":" "/" (substring remote-url (length "git@")))))

;; (defun git-share--copy-link (link)
;;   "Copy LINK to clipboard.

;; Opens LINK via `browse-url' if `git-share-open-links-in-browser' is non-nil."
;;   (kill-new link)
;;   (when git-share-open-links-in-browser
;;     (browse-url link))
;;   (message (concat "Copied " link " to clipboard.")))

;; (defun git-share--vc-git-blame (files &optional buffer &rest args)
;;   "Run git blame on FILES.

;; If BUFFER is nil, output is written to the *vc-blame* buffer. ARGS
;; are forwarded into the git blame command."
;;   (apply #'vc-git-command (or buffer "*vc-blame*") 1 files
;;          "blame" args))

;; (defun git-share--blame-line (filename loc)
;;   "Return git blame for FILENAME at LOC as a string."
;;   (let* ((loc (number-to-string loc))
;;          (loc-arg (concat "-L " loc "," loc)))
;;     (with-temp-buffer
;;       (git-share--vc-git-blame filename (current-buffer) loc-arg)
;;       (substring (buffer-string) 0 -1))))

;; (defun git-share--extract-commit (blame-string)
;;   "Extract commit hash from BLAME-STRING, stripping leading ^ if present."
;;   (let ((hash (car (split-string blame-string " "))))
;;     (if (string-prefix-p "^" hash)
;;         (substring hash 1)
;;       hash)))

;; (defun git-share--commit-at-point (remote)
;;   "Extract a short commit hash for REMOTE from LOC at point."
;;   (git-share--extract-commit
;;    (git-share--blame-line (git-share-remote-filename remote) (line-number-at-pos))))

;; (defun git-share--line-number (forge)
;;   "Return LOC line number for `git-share' as a string.

;; When region is active, returns a range, e.g. 10-20.  Otherwise,
;; returns `line-number-at-pos'."
;;   (if (use-region-p)
;;       (concat
;;        (number-to-string (line-number-at-pos (region-beginning)))
;;        "-"
;;        ;; Github requires a trailing -L in the second half of the range.
;;        (if (eq forge 'github) "L" "")
;;        (number-to-string (line-number-at-pos (region-end))))
;;     (number-to-string (line-number-at-pos))))

;; (defun git-share--loc-url (remote &optional default-branch)
;;   (let* ((branch (or default-branch (git-share--branch-prompt)))
;;          (loc (git-share--line-number (git-share-remote-forge remote)))
;;          (forge (alist-get (git-share-remote-forge remote) git-share-forge-alist)))
;;     (format
;;      (git-share-forge-loc-format-string forge)
;;      (git-share-remote-base-url remote)
;;      branch
;;      (concat (git-share-remote-rel-filename remote) "#L" loc))))

;; (defun git-share--commit-url (remote &optional commit)
;;   (let* ((commit (or commit (git-share--commit-at-point remote)))
;;          (forge (alist-get (git-share-remote-forge remote) git-share-forge-alist)))
;;     (format
;;      (git-share-forge-commit-format-string forge)
;;      (git-share-remote-base-url remote)
;;      commit)))

;; ;;;###autoload
;; (defun git-share ()
;;   "Copy a web link to the LOC at point."
;;   (interactive)
;;   (if-let* ((filename (buffer-file-name (current-buffer)))
;;             (remote (git-share-remote-from-filename filename)))
;;       (git-share--copy-link (git-share--loc-url remote))
;;     (error "Must be in a git repository")))

;; ;;;###autoload
;; (defun git-share-commit ()
;;   "Copy a web link to the commit at point."
;;   (interactive)
;;   (if-let* ((filename (buffer-file-name (current-buffer)))
;;             (remote (git-share-remote-from-filename filename)))
;;       (git-share--copy-link (git-share--commit-url remote))
;;     (error "Must be in a git repository")))

(provide 'git-share)
;;; git-share.el ends here
