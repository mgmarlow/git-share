;;; git-share-test.el --- unit tests  -*- lexical-binding: t; -*-

(require 'git-share)
(require 'ert)

(ert-deftest test-repo-basename ()
  "Tests the extraction of basename from a repo URI."
  (should (equal (git-share--repo-basename "git@github.com:user/repo.git")
                 "github.com/user/repo"))
  (should (equal (git-share--repo-basename "git@git.sr.ht:~user/repo")
                 "git.sr.ht/~user/repo"))
  (should (equal (git-share--repo-basename "https://github.com/user/repo.git")
                 "github.com/user/repo")))

(ert-deftest test-maybe-remove-extension ()
  "Tests removal of .git from URIs"
  (should (equal (git-share--maybe-remove-extension "github.com/user/repo.git")
                 "github.com/user/repo"))
  (should (equal (git-share--maybe-remove-extension "github.com/user/repo")
                 "github.com/user/repo")))

(ert-deftest test-format ()
  "Tests formatting of URI placed on kill ring."
  (should (equal (git-share--format "github.com/user/repo" "main" "foo/bar.el" 323)
                 "https://github.com/user/repo/blob/main/foo/bar.el#L323"))
  (should (equal (git-share--format "git.sr.ht/~user/repo" "main" "foo/bar.el" 323)
                 "https://git.sr.ht/~user/repo/tree/main/item/foo/bar.el#L323")))

(ert-deftest test-extract-commit ()
  "Test commit extraction from git blame."
  (should (equal (git-share--extract-commit "002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6"))
  (should (equal (git-share--extract-commit "^002c05b6 (mgmarlow 2023-04-29 10:09:12 -0700 77)   \"Extracts basename from HTTPS repository URI.")
                 "002c05b6")))
