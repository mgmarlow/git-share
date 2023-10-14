;;; store-git-link.el --- Stores a web link to git repository at current point  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Graham Marlow

;; Author: Graham Marlow <info@mgmarlow.com>
;; Keywords: vc, tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; URL: https://git.sr.ht/~mgmarlow/store-git-link

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
;; - `store-git-link': Copies a link to the current line of code.
;; - `store-git-link-commit': Copies a link to the commit hash associated
;;   responsible for the current line of code, determined via git blame.

;;; Code:

(require 'vc)
(require 'vc-git)

(defgroup store-git-link ()
  "Generate links to git repositories directly from source."
  :group 'tools)

(defcustom sgl-open-links-in-browser nil
  "If non-nil, opens links in your default browser when copied."
  :group 'store-git-link
  :type 'boolean)

;; URI formatters for different code hosts
(defun sgl--format-sourcehut (basename branch rel-filename loc)
  (format "https://%s/tree/%s/item/%s"
          basename
          branch
          (concat rel-filename "#L" (number-to-string loc))))

(defun sgl--format-github (basename branch rel-filename loc)
  (format "https://%s/blob/%s/%s"
          basename
          branch
          (concat rel-filename "#L" (number-to-string loc))))

(defun sgl--format (basename branch rel-filename loc)
  "Format BASENAME, BRANCH, REL-FILENAME, and LOC into a URI."
  (cond ((string-match-p "github.com" basename)
         (sgl--format-github basename branch rel-filename loc))
        ((string-match-p "git.sr.ht" basename)
         (sgl--format-sourcehut basename branch rel-filename loc))
        (t (error "Unsupported git remote"))))

(defun sgl--maybe-remove-extension (uri)
  "Remove '.git' from a repo URI, if it exists."
  (if (string-suffix-p ".git" uri)
      (substring uri 0 (* (length ".git") -1))
    uri))

(defun sgl--repo-basename (repo-uri)
  "Extract basename from REPO-URI.

Examples:
https://github.com/user/repo.git -> github.com/user/repo
git@github.com:user/repo.git -> github.com/user/repo
git@git.sr.ht:~user/repo     -> git.sr.ht/~user/repo"
  (sgl--maybe-remove-extension
   (if (string-prefix-p "https" repo-uri)
       (substring repo-uri (length "https://"))
     (replace-regexp-in-string ":" "/" (substring repo-uri (length "git@"))))))

(defun sgl--branch-prompt ()
  (let ((branches (vc-git-branches)))
    (if (= (length branches) 1)
        (car branches)
      (completing-read "Select branch: " branches nil t))))

(defun sgl--generate-link (filename)
  "Generate a link to the repository of current buffer at current line number."
  (let ((basename (sgl--repo-basename (vc-git-repository-url filename)))
        (branch (sgl--branch-prompt))
        (rel-filename (file-relative-name filename (vc-root-dir))))
    (sgl--format basename branch rel-filename (line-number-at-pos))))

(defun sgl--copy-link (link)
  "Copy LINK to clipboard.

Opens LINK via `browse-url' if `sgl-open-links-in-browser' is non-nil."
  (kill-new link)
  (when sgl-open-links-in-browser
    (browse-url link))
  (message (concat "Copied " link " to clipboard.")))

;;;###autoload
(defun store-git-link ()
  "Store a web link to git repository at current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "Must be in a git repository"))
    (sgl--copy-link (sgl--generate-link filename))))

(defun sgl--format-commit (basename commit)
  (format "https://%s/commit/%s" basename commit))

(defun sgl--vc-git-blame (files &optional buffer &rest args)
  "Run git blame on FILES.

If BUFFER is nil, output is written to the *vc-blame* buffer. ARGS
are forwarded into the git blame command."
  (apply #'vc-git-command (or buffer "*vc-blame*") 1 files
                  "blame" args))

(defun sgl--blame-line (filename loc)
  "Return git blame for FILENAME at LOC as a string."
  (let* ((loc (number-to-string loc))
         (loc-arg (concat "-L " loc "," loc)))
    (with-temp-buffer
      (sgl--vc-git-blame filename (current-buffer) loc-arg)
      (substring (buffer-string) 0 -1))))

(defun sgl--extract-commit (blame-string)
  "Extract commit hash from BLAME-STRING, stripping leading ^ if present."
  (let ((hash (car (split-string blame-string " "))))
    (if (string-prefix-p "^" hash)
        (substring hash 1)
      hash)))

;;;###autoload
(defun store-git-link-commit ()
  "Store a link to the commit associated with current point in the kill ring."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (unless (and filename (vc-git-registered filename))
      (error "Must be in a git repository"))
    (let ((basename (sgl--repo-basename (vc-git-repository-url filename)))
          (commit (sgl--extract-commit
                   (sgl--blame-line filename (line-number-at-pos)))))
      (sgl--copy-link (sgl--format-commit basename commit)))))

(provide 'store-git-link)
;;; store-git-link.el ends here
