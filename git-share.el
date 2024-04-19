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

;; A tiny Emacs package that provides a couple of commands for copying
;; web links to git repositories.  Links are copied to the kill-ring
;; with a configuration option available to additionally open them in
;; your default browser.  The link URI is determined based on the
;; remote URI of the git repository in which the buffer file resides.
;; If there are multiple branches, the user is prompted to select one.

;; `git-share' copies a link to the current line or region at point.

;; `git-share-commit' copies a link to the current commit based on a
;; blame of the line at point.

;; The following forges are supported:
;;
;; - GitHub
;; - SourceHut
;; - GitLab
;; - Codeberg
;; - Bitbucket
;; - GNU Savannah

;;; Code:

(require 'vc-git)
(require 'url-parse)

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

;;; Dynamic formatter functions

;; The following macros create defuns called by `git-share' when
;; pairing a forge-type (e.g. github) to a format string.  Most forges
;; use a very similar format for their URLs, so the majority of macro
;; invocations only differ by format string.  Others, like GNU
;; Savannah, require a custom format function due to a different order
;; of arguments.
;;
;; The defun created by these macros follows a consistent structure
;; that is expected by the respective callsite,
;; e.g. `git-share--format-line'.  That structure looks like
;; "git-share--<FORGE-NAME>-format-<KIND>".

;; These functions are used during the application runtime when called
;; by the top-level `git-share-format-<KIND>', hence the
;; `eval-and-compile'.
(eval-and-compile
  (defun git-share--build-line-func (forge)
    (intern (concat "git-share--" (symbol-name forge) "-format-line")))

  (defun git-share--build-region-func (forge)
    (intern (concat "git-share--" (symbol-name forge) "-format-region")))

  (defun git-share--build-commit-func (forge)
    (intern (concat "git-share--" (symbol-name forge) "-format-commit"))))

(eval-when-compile
  (defmacro git-share--create-line-formatter (forge formatter)
    `(defun ,(git-share--build-line-func forge) (base-url branch filename line)
       (format ,formatter base-url branch filename line)))

  (defmacro git-share--create-region-formatter (forge formatter)
    `(defun ,(git-share--build-region-func forge) (base-url branch filename start end)
       (format ,formatter base-url branch filename start end)))

  (defmacro git-share--create-commit-formatter (forge formatter)
    `(defun ,(git-share--build-commit-func forge) (base-url commit)
       (format ,formatter base-url commit))))

;;; Line formatters
(git-share--create-line-formatter github "%s/blob/%s/%s#L%s")
(git-share--create-line-formatter sourcehut "%s/tree/%s/item/%s#L%s")
(git-share--create-line-formatter gitlab "%s/-/blob/%s/%s#L%s")
(git-share--create-line-formatter codeberg "%s/src/branch/%s/%s#L%s")
(git-share--create-line-formatter bitbucket "%s/src/%s/%s#lines-%s")
;; Compared to the other format-line functions, GNU Savannah swaps the
;; position of the branch/filename arguments since branch is a query
;; parameter.
(defun git-share--savannah-format-line (base-url branch filename line)
  (format "%s/tree/%s?h=%s#n%s" base-url filename branch line))

;;; Region formatters
(git-share--create-region-formatter github "%s/blob/%s/%s#L%s-L%s")
(git-share--create-region-formatter sourcehut "%s/tree/%s/item/%s#L%s-%s")
(git-share--create-region-formatter gitlab "%s/-/blob/%s/%s#L%s-%s")
(git-share--create-region-formatter codeberg "%s/src/branch/%s/%s#L%s-L%s")
(git-share--create-region-formatter bitbucket "%s/src/%s/%s#lines-%s:%s")

(defun git-share--savannah-format-region (_base-url _branch _filename _start _end)
  (error "GNU Savannah does not support region links"))

;;; Commit formatters
(git-share--create-commit-formatter github "%s/commit/%s")
(git-share--create-commit-formatter sourcehut "%s/commit/%s")
(git-share--create-commit-formatter gitlab "%s/-/commit/%s")
(git-share--create-commit-formatter codeberg "%s/commit/%s")
(git-share--create-commit-formatter bitbucket "%s/commits/%s")
(git-share--create-commit-formatter savannah "%s/commit/?id=%s")

;; TODO: Could probably clean this up a bit, all three of these format
;; function fetchers are very similar.
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

(defun git-share--format-commit (remote-url commit)
  "Assumes a formatter function that matches `git-share--build-commit-func'."
  (let* ((base-url (git-share--base-url remote-url))
         (forge (git-share--forge base-url))
         (func (git-share--build-commit-func forge)))
    (url-encode-url (funcall func base-url commit))))

;;; Helpers for forming the base link path
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

;;; Commit extraction
(defun git-share--vc-git-blame (files &optional buffer &rest args)
  "Run git blame on FILES.

If BUFFER is nil, output is written to the *vc-blame* buffer.
ARGS are forwarded into the git blame command."
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

(defun git-share--commit-at-point (filename)
  "Extract a short commit hash for REMOTE from LOC at point."
  (git-share--extract-commit
   (git-share--blame-line filename (line-number-at-pos))))

;;;###autoload
(defun git-share-commit ()
  "Copy a web link to the commit at point."
  (interactive)
  (if-let* ((filename (buffer-file-name (current-buffer)))
            (remote-url (vc-git-repository-url filename)))
      (git-share--copy-link
       (git-share--format-commit
        remote-url
        (git-share--commit-at-point (file-relative-name filename (vc-root-dir)))))
    (error "Must be in a git repository")))

(provide 'git-share)
;;; git-share.el ends here
