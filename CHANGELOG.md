# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

## [0.2] - 2026-07-04

Complete modernization for KeePassXC ≥ 2.6 (tested against 2.8-dev).

### Added
- auth-source backend (`keepassxc-auth-source.el`):
  `keepassxc-auth-source-enable` makes KeePassXC answer
  `auth-source-search`, including entry creation with `:create t`.
- New protocol actions: `keepassxc-get-totp`, `keepassxc-copy-totp`,
  `keepassxc-get-database-groups`, `keepassxc-create-new-group`,
  `keepassxc-delete-entry`, `keepassxc-request-autotype`,
  `keepassxc-get-database-entries`.
- Copy commands with kill-ring auto-clear
  (`keepassxc-password-timeout`): `keepassxc-copy-password`,
  `keepassxc-copy-username`, `keepassxc-copy-totp`.
- `keepassxc-copy-url` and `keepassxc-browse-url`: pick an entry from
  all database entries (completion matches title and URL) and copy or
  browse its URL.
- `keepassxc` transient menu.
- Entry selection with completion annotations (username, group).
- Unsolicited server signals (`database-locked`/`database-unlocked`)
  are dispatched to the `keepassxc-signal-functions` hook.
- Socket discovery for current KeePassXC locations: nested XDG runtime
  path (2.7.5+), legacy symlink, snap, flatpak, macOS `$TMPDIR`;
  override with `keepassxc-socket-path`.
- Error hierarchy (`keepassxc-error`, `keepassxc-database-locked`,
  `keepassxc-not-associated`, ...) mapped from KeePassXC error codes.
- `keepassxc-lock-all-databases` and `keepassxc-open-database` work
  without D-Bus (macOS included) by invoking `keepassxc-command`,
  which reaches the running KeePassXC through its single-instance
  socket.
- Offline generators backed by `keepassxc-cli`
  (`keepassxc-cli-command`): `keepassxc-cli-generate-password`,
  `keepassxc-cli-diceware`, `keepassxc-cli-estimate-password`.
- ERT test suite with a full in-Emacs mock KeePassXC server;
  GitHub Actions CI (Linux + macOS × Emacs 28.2/29.4/30.1).

### Changed
- **Breaking**: requires Emacs 28.1+ and sodium.el 0.2.
- Associations are stored GPG-encrypted in a plstore
  (`keepassxc-association-file`), keyed by database hash — multiple
  databases are now supported.  The old plaintext `keepassxc-save-file`
  is not read anymore; run `keepassxc-associate` once and delete it.
- The transport keypair is ephemeral per connection instead of being
  reused from disk.
- `keepassxc-generate-password` supports the modern reply format
  (async `password` field with `requestID`).
- `keepassxc-set-login` signature is now
  `(url login password &optional uuid group group-uuid)`.

### Fixed
- `keepassxc-set-login` sent the association *key* instead of the
  association *id* and could never succeed.
- `triggerUnlock` was serialized as JSON boolean, which KeePassXC
  ignores; the unlock prompt now actually appears (customizable via
  `keepassxc-trigger-unlock`).
- Partial reads, back-to-back messages and stray bytes on the socket
  no longer break the connection ("rubbish after the json object");
  the process filter now buffers and re-syncs properly.
- Protocol errors (wrong nonce, decryption failure) are signaled to
  the caller instead of being swallowed inside the process filter and
  surfacing as bogus timeouts.
- `keepassxc-lock-database` and entry commands are properly
  `(interactive)`.

## [0.1] - 2019-05-14

Initial version: DBus commands, associate/get-logins/set-login/
generate-password over the browser socket protocol.
