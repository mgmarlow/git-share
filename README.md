# git-share

An emacs package for sharing code links with colleagues.

Supports:

- GitHub
- SourceHut
- GitLab
- Codeberg
- Bitbucket
- GNU Savannah

## Installation

### Emacs 29

Use `package-vc-install`:

```
M-x package-vc-install https://github.com/mgmarlow/git-share
```

### Older versions

Clone the repo and update your load path:

```
git clone https://github.com/mgmarlow/git-share /path/to/git-share
```

```
(add-to-list 'load-path "/path/to/git-share")
(require 'git-share)
```

## Commands

- `M-x git-share`: Copies a URL to the current line of code.

- `M-x git-share-commit`: Copies a URL to the commit responsible for
  the current line of code, determined via `git blame`.

### Configuration

- `git-share-open-links-in-browser`: When `t`, opens links in your
  browser when copied.

## License

Licensed under [GPL-3.0](./LICENSE).
