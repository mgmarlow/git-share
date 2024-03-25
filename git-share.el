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

(defvar git-share-formatter-alist
  '((github . ((line . "%s/blob/%s/%s#L%s")
               (region . "%s/blob/%s/%s#L%s-L%s")
               (commit . "%s/commit/%s")))
    (sourcehut . ((line . "%s/tree/%s/item/%s#L%s")
                  (region . "%s/tree/%s/item/%s#L%s-%s")
                  (commit . "%s/commit/%s")))
    (gitlab . ((line . "%s/-/blob/%s/%s#L%s")
               (region . "%s/-/blob/%s/%s#L%s-%s")
               (commit . "%s/-/commit/%s"))))
  "An alist of link formats for different source forges.")

(defun git-share--forge (remote-url)
  "Return source forge from REMOTE-URL."
  (cond
   ((string-match-p "github.com" remote-url) 'github)
   ((string-match-p "git.sr.ht" remote-url) 'sourcehut)
   ((string-match-p "gitlab.com" remote-url) 'gitlab)
   (t (error "Unsupported git remote %s" remote-url))))

(defun git-share--maybe-remove-extension (uri)
  "Remove trailing '.git' from URI, if present."
  (if (string-suffix-p ".git" uri)
      (substring uri 0 (* (length ".git") -1))
    uri))

;; TODO: Call vc-git-repository-url elsewhere

(defun git-share--base-url (filename remote-url)
  "Create a base HTTP URL for FILENAME from REMOTE-URL. A base
URL includes the source forge, username, and repository name.

Remotes may be either SSH or HTTPS."
  (git-share--maybe-remove-extension
   (if (string-prefix-p "https" remote-url)
       remote-url
     (string-replace "git@" "https://"
                     ;; Assuming no colons in usernames or repos.
                     (string-replace ":" "/" remote-url)))))

(defun git-share--copy-link (link)
  "Copy LINK to clipboard.

Opens LINK via `browse-url' if `git-share-open-links-in-browser' is non-nil."
  (kill-new link)
  (when git-share-open-links-in-browser
    (browse-url link))
  (message (concat "Copied " link " to clipboard.")))

(defun git-share--format-string (forge kind)
  "Retrieve a format-string from `git-share-formatter-alist'."
  (alist-get kind (alist-get forge git-share-formatter-alist)))

(defun git-share--format-line (base-url branch filename)
  "Return a string URL for the git repository at line."
  (let ((forge (git-share--forge base-url)))
    (format
     (git-share--format-string forge 'line)
     base-url
     branch
     filename
     (number-to-string (line-number-at-pos)))))

(defun git-share--format-region (base-url branch filename)
  "Return a string URL for git repository for lines within current region."
  (let ((forge (git-share--forge base-url)))
    (format
     (git-share--format-string forge 'region)
     base-url
     branch
     filename
     (number-to-string (line-number-at-pos (region-beginning)))
     (number-to-string (line-number-at-pos (region-end))))))

;;;###autoload
(defun git-share ()
  "Copy a web link to the LOC at point."
  (interactive)
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (remote-url (vc-git-repository-url filename)))
      (git-share--copy-link
       (if (use-region-p)
           (git-share--format-region
            (git-share--base-url filename remote-url)
            (git-share--branch-prompt)
            (file-relative-name filename (vc-root-dir)))
         (git-share--format-line
          (git-share--base-url filename remote-url)
          (git-share--branch-prompt)
          (file-relative-name filename (vc-root-dir)))))
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



;; (defun git-share--branch-prompt ()
;;   (let ((branches (vc-git-branches)))
;;     (if (= (length branches) 1)
;;         (car branches)
;;       (completing-read "Select branch: " branches nil t))))

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
