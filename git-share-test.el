;;; git-share-test.el --- unit tests  -*- lexical-binding: t; -*-

(require 'git-share)
(require 'ert)

(defun mock-remote (forge)
  (pcase forge
    ('github-https
     (git-share-remote-from-filename "foo.txt" "https://github.com/user/repo.git"))
    ('github-ssh
     (git-share-remote-from-filename "foo.txt" "git@github.com:user/repo.git"))
    ('sourcehut-https
     (git-share-remote-from-filename "foo.txt" "https://git.sr.ht/~user/repo.git"))
    ('sourcehut-ssh
     (git-share-remote-from-filename "foo.txt" "git@git.sr.ht:~user/repo"))
    ('gitlab-https
     (git-share-remote-from-filename "foo.txt" "https://gitlab.com/user/repo.git"))
    ('gitlab-ssh
     (git-share-remote-from-filename "foo.txt" "git@gitlab.com:user/repo.git"))))

(ert-deftest test-github-https-loc ()
  (should (equal (git-share--loc-url (mock-remote 'github-https) "main")
                 "https://github.com/user/repo/blob/main/foo.txt#L1")))

(ert-deftest test-github-ssh-loc ()
  (should (equal (git-share--loc-url (mock-remote 'github-ssh) "main")
                 "https://github.com/user/repo/blob/main/foo.txt#L1")))

(ert-deftest test-sourcehut-https-loc ()
  (should (equal (git-share--loc-url (mock-remote 'sourcehut-https) "main")
                 "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

(ert-deftest test-sourcehut-ssh-loc ()
  (should (equal (git-share--loc-url (mock-remote 'sourcehut-ssh) "main")
                 "https://git.sr.ht/~user/repo/tree/main/item/foo.txt#L1")))

(ert-deftest test-gitlab-https-loc ()
  (should (equal (git-share--loc-url (mock-remote 'gitlab-https) "main")
                 "https://gitlab.com/user/repo/-/blob/main/foo.txt#L1")))

(ert-deftest test-gitlab-ssh-loc ()
  (should (equal (git-share--loc-url (mock-remote 'gitlab-ssh) "main")
                 "https://gitlab.com/user/repo/-/blob/main/foo.txt#L1")))

(ert-deftest test-loc-unsupported-provider ()
  (should-error (git-share--loc-url (git-share-remote-from-filename "foo.txt" "git@foolab.com:inkscape/inkscape") "main")))

(ert-deftest test-github-commit ()
  (should (equal (git-share--commit-url (mock-remote 'github-ssh) "002c05b6")
                 "https://github.com/user/repo/commit/002c05b6")))

(ert-deftest test-sourcehut-commit ()
  (should (equal (git-share--commit-url (mock-remote 'sourcehut-ssh) "002c05b6")
                 "https://git.sr.ht/~user/repo/commit/002c05b6")))

(ert-deftest test-gitlab-commit ()
  (should (equal (git-share--commit-url (mock-remote 'gitlab-ssh) "002c05b6")
                 "https://gitlab.com/user/repo/-/commit/002c05b6")))

(ert-deftest test-extract-commit ()
  "Test commit extraction from git blame."
  (should (equal (git-share--extract-commit "002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6"))
  (should (equal (git-share--extract-commit "^002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6")))
