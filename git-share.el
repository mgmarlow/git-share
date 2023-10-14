;;; git-share.el --- Stores a web link to git repository at current point  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: vc, tools
;; Version: 1.0.0
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

;; A tiny Emacs package for sharing code links with colleagues.
;; Currently supports Github and Sourcehut.
;;
;; - `git-share': Copies a link to the current line of code.
;; - `git-share-commit': Copies a link to the commit hash associated
;;   responsible for the current line of code, determined via git blame.

;;; Code:

(require 'vc-git)

(defgroup git-share ()
  "Generate links to git repositories directly from source."
  :group 'tools)

(defcustom git-share-open-links-in-browser nil
  "If non-nil, opens links in your default browser when copied."
  :group 'git-share
  :type 'boolean)

(defun git-share--format (basename branch rel-filename loc)
  "Format BASENAME, BRANCH, REL-FILENAME, and LOC into a URI."
  (let ((format-string
         (cond
          ((string-prefix-p "github.com" basename) "https://%s/blob/%s/%s" )
          ((string-prefix-p "git.sr.ht" basename) "https://%s/tree/%s/item/%s")
          (t (error "Unsupported git remote %s" basename))))
        (filename-at-loc (concat rel-filename "#L" (number-to-string loc))))
    (format format-string basename branch filename-at-loc)))

(defun git-share--maybe-remove-extension (uri)
  "Remove '.git' from a repo URI, if it exists."
  (if (string-suffix-p ".git" uri)
      (substring uri 0 (* (length ".git") -1))
    uri))

(defun git-share--repo-basename (repo-uri)
  "Extract basename from REPO-URI.

Examples:
https://github.com/user/repo.git -> github.com/user/repo
git@github.com:user/repo.git -> github.com/user/repo
git@git.sr.ht:~user/repo     -> git.sr.ht/~user/repo"
  (git-share--maybe-remove-extension
   (if (string-prefix-p "https" repo-uri)
       (substring repo-uri (length "https://"))
     (replace-regexp-in-string ":" "/" (substring repo-uri (length "git@"))))))

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

(defun git-share--vc-git-blame (files &optional buffer &rest args)
  "Run git blame on FILES.

If BUFFER is nil, output is written to the *vc-blame* buffer. ARGS
are forwarded into the git blame command."
  (apply #'vc-git-command (or buffer "*vc-blame*") 1 files
         "blame" args))

(defun git-share--blame-line (filename loc)
  "Return git blame for FILENAME at LOC as a string."
  (let* ((loc (number-to-string loc))
         (loc-arg (concat "-L " loc "," loc)))
    (with-temp-buffer
      (git-share--vc-git-blame filename (current-buffer) loc-arg)
      (substring (buffer-string) 0 -1))))

(defun git-share--extract-commit (blame-string)
  "Extract commit hash from BLAME-STRING, stripping leading ^ if present."
  (let ((hash (car (split-string blame-string " "))))
    (if (string-prefix-p "^" hash)
        (substring hash 1)
      hash)))

(defun git-share--loc-url (filename)
  (let* ((url (vc-git-repository-url filename))
         (basename (git-share--repo-basename url))
         (branch (git-share--branch-prompt))
         (rel-filename (file-relative-name filename (vc-root-dir))))
    (git-share--format basename branch rel-filename (line-number-at-pos))))

(defun git-share--commit-url (filename)
  (let* ((url (vc-git-repository-url filename))
         (basename (git-share--repo-basename url))
         (commit (git-share--extract-commit
                  (git-share--blame-line filename (line-number-at-pos)))))
    (format "https://%s/commit/%s" basename commit)))

;;;###autoload
(defun git-share ()
  "Copy a web link to the LOC at point."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "Must be in a git repository"))
    (git-share--copy-link (git-share--loc-url filename))))

;;;###autoload
(defun git-share-commit ()
  "Copy a web link to the commit at point."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "Must be in a git repository"))
    (git-share--copy-link (git-share--commit-url filename))))

(provide 'git-share)
;;; git-share.el ends here
