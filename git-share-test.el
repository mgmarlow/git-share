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
           ("https://git.savannah.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n15" . "git://git.savannah.gnu.org/repo.git")
           ("https://git.sv.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n15" . "git://git.sv.gnu.org/repo.git")
           ("https://git.savannah.gnu.org/cgit/repo.git/tree/foo.txt?h=main#n15" . "https://git.savannah.gnu.org/git/repo.git"))))
    (dolist (testcase testcases)
      (let ((expected (car testcase))
            (remote-url (cdr testcase)))
        (should (equal (git-share--format-line remote-url branch filename line)
                       expected))))))

(ert-deftest test-git-share-line-unsupported-remote ()
  (should-error (git-share--format-line "git@foo.bar:user/repo.git" "main" "foo.txt" 15)))

(ert-deftest test-git-share-line-rel-filename ()
  (should (equal (git-share--format-line "git@github.com:user/repo.git" "main" "docs/bar/foo.txt" 15)
                 "https://github.com/user/repo/blob/main/docs/bar/foo.txt#L15")))

(ert-deftest test-git-share-region ()
  (let ((branch "main")
        (start 15)
        (end 30)
        (filename "foo.txt") ;; relative to vc-root-dir
        (testcases
         '(("https://github.com/user/repo/blob/main/foo.txt#L15-L30" . "git@github.com:user/repo.git")
           ("https://github.com/user/repo/blob/main/foo.txt#L15-L30" . "https://github.com/user/repo.git")
           ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L15-30" . "https://git.sr.ht/~user/repo.git")
           ("https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L15-30" . "git@git.sr.ht:~user/repo")
           ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L15-30" . "https://gitlab.com/user/repo.git")
           ("https://gitlab.com/user/repo/-/blob/main/foo.txt#L15-30" . "git@gitlab.com:user/repo.git")
           ("https://codeberg.org/user/repo/src/branch/main/foo.txt#L15-L30" . "https://codeberg.org/user/repo.git")
           ("https://bitbucket.org/user/repo/src/main/foo.txt#lines-15:30" . "https://bitbucket.org/user/repo.git")
           ("https://bitbucket.org/user/repo/src/main/foo.txt#lines-15:30" . "git@bitbucket.org:user/repo.git"))))
    (dolist (testcase testcases)
      (let ((expected (car testcase))
            (remote-url (cdr testcase)))
        (should (equal (git-share--format-region remote-url branch filename start end)
                       expected))))))

(ert-deftest test-git-share-region-unsupported-remote ()
  (should-error (git-share--format-region "git@foo.bar:user/repo.git" "main" "foo.txt" 15 30)))

(ert-deftest test-git-share-region-errors-on-savannah ()
  (should-error (git-share--format-region "git://git.savannah.gnu.org/repo.git" "main" "foo.txt" 15 30)))

(ert-deftest test-git-share-commit ()
  (let ((commit "002c05b6")
        (testcases
         '(("https://github.com/user/repo/commit/002c05b6" . "git@github.com:user/repo.git")
           ("https://github.com/user/repo/commit/002c05b6" . "https://github.com/user/repo.git")
           ("https://git.sr.ht/~user/repo/commit/002c05b6" . "https://git.sr.ht/~user/repo.git")
           ("https://git.sr.ht/~user/repo/commit/002c05b6" . "git@git.sr.ht:~user/repo")
           ("https://gitlab.com/user/repo/-/commit/002c05b6" . "https://gitlab.com/user/repo.git")
           ("https://gitlab.com/user/repo/-/commit/002c05b6" . "git@gitlab.com:user/repo.git")
           ("https://codeberg.org/user/repo/commit/002c05b6" . "https://codeberg.org/user/repo.git")
           ("https://bitbucket.org/user/repo/commits/002c05b6" . "https://bitbucket.org/user/repo.git")
           ("https://bitbucket.org/user/repo/commits/002c05b6" . "git@bitbucket.org:user/repo.git")
           ("https://git.savannah.gnu.org/cgit/repo.git/commit/?id=002c05b6" . "git://git.savannah.gnu.org/repo.git")
           ("https://git.sv.gnu.org/cgit/repo.git/commit/?id=002c05b6" . "git://git.sv.gnu.org/repo.git")
           ("https://git.savannah.gnu.org/cgit/repo.git/commit/?id=002c05b6" . "https://git.savannah.gnu.org/git/repo.git"))))
    (dolist (testcase testcases)
      (let ((expected (car testcase))
            (remote-url (cdr testcase)))
        (should (equal (git-share--format-commit remote-url commit) expected))))))

(ert-deftest test-git-share-commit-unsupported-remote ()
  (should-error (git-share--format-commit "git@foo.bar:user/repo.git" "002c05b6")))
