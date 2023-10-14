;;; git-share-test.el --- unit tests  -*- lexical-binding: t; -*-

(require 'git-share)
(require 'ert)

(ert-deftest test-github-https-loc ()
  (should (equal (git-share--loc-url "foo.txt" "https://github.com/user/repo.git" "main" ".")
                 "https://github.com/user/repo/blob/main/foo.txt#L1")))

(ert-deftest test-github-ssh-loc ()
  (should (equal (git-share--loc-url "foo.txt" "git@github.com:user/repo.git" "main" ".")
                 "https://github.com/user/repo/blob/main/foo.txt#L1")))

(ert-deftest test-sourcehut-https-loc ()
  (should (equal (git-share--loc-url "foo.txt" "https://git.sr.ht/~user/repo.git" "main" ".")
                 "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

(ert-deftest test-sourcehut-ssh-loc ()
  (should (equal (git-share--loc-url "foo.txt" "git@git.sr.ht:~user/repo" "main" ".")
                 "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

;; Unfortunately can't easily test `file-relative-name' path using an
;; actual root directory, so fudge it in the filename argument
;; instead.
(ert-deftest test-loc-different-root ()
  (should (equal (git-share--loc-url "foo/bar/main.rs" "git@git.sr.ht:~user/repo" "main" ".")
                 "https://git.sr.ht/~user/repo/tree/main/item/foo/bar/main.rs#L1")))

(ert-deftest test-github-commit ()
  (should (equal (git-share--commit-url "foo.txt" "git@github.com:user/repo.git" "002c05b6")
                 "https://github.com/user/repo/commit/002c05b6")))

(ert-deftest test-sourcehut-commit ()
  (should (equal (git-share--commit-url "foo.txt" "git@git.sr.ht:~user/repo" "002c05b6")
                 "https://git.sr.ht/~user/repo/commit/002c05b6")))

(ert-deftest test-extract-commit ()
  "Test commit extraction from git blame."
  (should (equal (git-share--extract-commit "002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6"))
  (should (equal (git-share--extract-commit "^002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6")))
