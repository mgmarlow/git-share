# git-share

An emacs package for sharing code links with colleagues.

Supports:

- Github
- Sourcehut
- Gitlab

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

### Adding new git remotes

Check out this commit for an example: [Add Gitlab
support](https://github.com/mgmarlow/git-share/commit/7f04a4a284bbff3eb9bacba93064351af5c4ffb6).

## License

Licensed under [GPL-3.0](./LICENSE).
