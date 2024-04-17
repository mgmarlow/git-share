;;; git-share-test.el --- unit tests  -*- lexical-binding: t; -*-

(require 'git-share)
(require 'ert)

(ert-deftest test-git-share-line ()
  (let ((branch "main")
        (line 15)
        (filename "foo.txt") ;; relative to vc-root-dir
        (testcases
         '(("https://github.com/user/repo/blob/main/foo.txt#L15" . "git@github.com:user/repo.git")
           ("https://github.com/user/repo/blob/main/foo.txt#L15" . "https://github.com/user/repo.git")
           ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L15" . "https://git.sr.ht/~user/repo.git")
           ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L15" . "git@git.sr.ht:~user/repo")
           ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L15" . "https://gitlab.com/user/repo.git")
           ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L15" . "git@gitlab.com:user/repo.git")
           ("https://codeberg.org/user/repo/src/branch/main/foo.txt#L15" . "https://codeberg.org/user/repo.git")
           ("https://bitbucket.org/user/repo/src/main/foo.txt#lines-15" . "https://bitbucket.org/user/repo.git")
           ("https://bitbucket.org/user/repo/src/main/foo.txt#lines-15" . "git@bitbucket.org:user/repo.git")
           ;; todo: url-encode branch name
           ;; ("https://git.savannah.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n24" . "git://git.savannah.gnu.org/repo.git")
           ;; ("https://git.savannah.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n24" . "https://git.savannah.gnu.org/git/repo.git")
           ;; ("https://git.savannah.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n24" . "ssh://git.savannah.gnu.org/srv/git/repo.git")))
        )))
    (dolist (testcase testcases)
      (let ((expected (car testcase))
            (remote-url (cdr testcase)))
        (should (equal (git-share--format-line remote-url branch filename line)
                       expected))))))

(ert-deftest test-git-share-line-unsupported-remote ()
  'stub)

(ert-deftest test-git-share-line-url-encodes-savannah ()
  'stub)

(ert-deftest test-git-share-line-rel-filename ()
  (should (equal (git-share--format-line "git@github.com:user/repo.git" "main" "docs/bar/foo.txt" 15)
                 "https://github.com/user/repo/blob/main/docs/bar/foo.txt#L15")))

(ert-deftest test-git-share-region ()
  'stub)

;; ehhhhhh
;; (require 'el-mock)
;; (defun mock-save (funcsym)
;;   (when (fboundp funcsym)
;;     (put funcsym 'mock-original-func (symbol-function funcsym))))

;; (defun mock-restore (funcsym)
;;   (when-let ((func (get funcsym 'mock-original-func)))
;;     (fset funcsym func)))

;; (defun run-test-loc (expected url loc branch)
;;   (unwind-protect
;;       (mock-save 'buffer-file-name)
;;     (mock-save 'vc-git-repository-url)
;;     (mock-save 'vc-root-dir)

;;     (fset 'buffer-file-name `(lambda (_) "~/repo/foo.txt"))
;;     (fset 'vc-git-repository-url `(lambda () ,url))
;;     (fset 'vc-root-dir `(lambda () "~/repo/"))

;;     (should (equal (git-share) expected))

;;     (mock-restore 'buffer-file-name)
;;     (mock-restore 'vc-git-repository-url)
;;     (mock-restore 'vc-root-dir)))

;; (ert-deftest test-git-share ()
;;   (run-test-loc "https://github.com/user/repo/blob/main/foo.txt#L1"
;;                 "https://github.com/user/repo"
;;                 155
;;                 "main"))

;; (ert-deftest test-forge ()
;;   (let ((testcases '((github . "https://github.com/user/repo")
;;                      (sourcehut . "https://git.sr.ht/~user/repo")
;;                      (gitlab . "https://gitlab.com/user/repo")
;;                      (savannah . "git://git.savannah.gnu.org/emacs.git")
;;                      (savannah . "git://git.sv.gnu.org/emacs.git"))))
;;     (dolist (testcase testcases)
;;       (let ((expected (car testcase))
;;             (input (cdr testcase)))
;;         (should (equal (git-share--forge input) expected))))))

;; (ert-deftest test-unsupported-forge ()
;;   (should-error (git-share--forge "https://foolab.com/user/repo")))

;; (ert-deftest test-base-url ()
;;   (let ((testcases '(("https://github.com/user/repo" . ("https://github.com/user/repo.git"))
;;                      ("https://github.com/user/repo" . ("git@github.com:user/repo.git"))
;;                      ("https://git.sr.ht/~user/repo" . ("https://git.sr.ht/~user/repo.git"))
;;                      ("https://git.sr.ht/~user/repo" . ("git@git.sr.ht:~user/repo"))
;;                      ("https://gitlab.com/user/repo" . ("https://gitlab.com/user/repo.git"))
;;                      ("https://gitlab.com/user/repo" . ("git@gitlab.com:user/repo.git"))
;;                      ("https://git.savannah.gnu.org/cgit/emacs.git" . ("git://git.sv.gnu.org/emacs.git"))
;;                      ("https://git.savannah.gnu.org/cgit/emacs.git" . ("git://git.savannah.gnu.org/emacs.git"))
;;                      ("https://git.savannah.gnu.org/cgit/emacs.git" . ("ssh://git.savannah.gnu.org/emacs.git"))
;;                      ("https://git.savannah.gnu.org/cgit/emacs.git" . ("https://git.savannah.gnu.org/emacs.git"))
;;                      ("https://codeberg.org/user/repo" . ("https://codeberg.org/user/repo.git"))
;;                      ("https://bitbucket.org/user/repo" . ("https://bitbucket.org/user/repo.git"))
;;                      ("https://bitbucket.org/user/repo" . ("git@bitbucket.org:user/repo.git")))))
;;     (dolist (testcase testcases)
;;       (let ((expected (car testcase))
;;             (input (cdr testcase)))
;;         (should (equal (apply #'git-share--base-url input) expected))))))

;; (ert-deftest test-format-line ()
;;   (let ((testcases '(("https://github.com/user/repo/blob/main/foo.txt#L1" .
;;                       ("https://github.com/user/repo" "main" "foo.txt" "1"))
;;                      ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1" .
;;                       ("https://git.sr.ht/~user/repo" "main" "foo.txt" "1"))
;;                      ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L1" .
;;                       ("https://gitlab.com/user/repo" "main" "foo.txt" "1")))))
;;     (dolist (testcase testcases)
;;       (let ((expected (car testcase))
;;             (input (cdr testcase)))
;;         (should (equal (apply #'git-share--format-line input) expected))))))

;; (ert-deftest test-format-region ()
;;   (let ((testcases '(("https://github.com/user/repo/blob/main/foo.txt#L1-L30" .
;;                       ("https://github.com/user/repo" "main" "foo.txt" "1" "30"))
;;                      ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1-30" .
;;                       ("https://git.sr.ht/~user/repo" "main" "foo.txt" "1" "30"))
;;                      ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L1-30" .
;;                       ("https://gitlab.com/user/repo" "main" "foo.txt" "1" "30")))))
;;     (dolist (testcase testcases)
;;       (let ((expected (car testcase))
;;             (input (cdr testcase)))
;;         (should (equal (apply #'git-share--format-region input) expected))))))

;;; -- OLD

;; (defun mock-remote (forge)
;;   (pcase forge
;;     ('github-https
;;      (git-share-remote-from-filename "foo.txt" "https://github.com/user/repo.git"))
;;     ('github-ssh
;;      (git-share-remote-from-filename "foo.txt" "git@github.com:user/repo.git"))
;;     ('sourcehut-https
;;      (git-share-remote-from-filename "foo.txt" "https://git.sr.ht/~user/repo.git"))
;;     ('sourcehut-ssh
;;      (git-share-remote-from-filename "foo.txt" "git@git.sr.ht:~user/repo"))
;;     ('gitlab-https
;;      (git-share-remote-from-filename "foo.txt" "https://gitlab.com/user/repo.git"))
;;     ('gitlab-ssh
;;      (git-share-remote-from-filename "foo.txt" "git@gitlab.com:user/repo.git"))))

;; (ert-deftest test-github-https-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'github-https) "main")
;;                  "https://github.com/user/repo/blob/main/foo.txt#L1")))

;; (ert-deftest test-github-ssh-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'github-ssh) "main")
;;                  "https://github.com/user/repo/blob/main/foo.txt#L1")))

;; (ert-deftest test-sourcehut-https-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'sourcehut-https) "main")
;;                  "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

;; (ert-deftest test-sourcehut-ssh-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'sourcehut-ssh) "main")
;;                  "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

;; (ert-deftest test-gitlab-https-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'gitlab-https) "main")
;;                  "https://gitlab.com/user/repo/-/blob/main/foo.txt#L1")))

;; (ert-deftest test-gitlab-ssh-loc ()
;;   (should (equal (git-share--loc-url (mock-remote 'gitlab-ssh) "main")
;;                  "https://gitlab.com/user/repo/-/blob/main/foo.txt#L1")))

;; (ert-deftest test-loc-unsupported-provider ()
;;   (should-error (git-share--loc-url (git-share-remote-from-filename "foo.txt" "git@foolab.com:inkscape/inkscape") "main")))

;; (ert-deftest test-github-commit ()
;;   (should (equal (git-share--commit-url (mock-remote 'github-ssh) "002c05b6")
;;                  "https://github.com/user/repo/commit/002c05b6")))

;; (ert-deftest test-sourcehut-commit ()
;;   (should (equal (git-share--commit-url (mock-remote 'sourcehut-ssh) "002c05b6")
;;                  "https://git.sr.ht/~user/repo/commit/002c05b6")))

;; (ert-deftest test-gitlab-commit ()
;;   (should (equal (git-share--commit-url (mock-remote 'gitlab-ssh) "002c05b6")
;;                  "https://gitlab.com/user/repo/-/commit/002c05b6")))

;; (ert-deftest test-extract-commit ()
;;   "Test commit extraction from git blame."
;;   (should (equal (git-share--extract-commit "002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
;;                  "002c05b6"))
;;   (should (equal (git-share--extract-commit "^002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
;;                  "002c05b6")))
