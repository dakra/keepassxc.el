# KeePassXC integration for Emacs

[![CI](https://github.com/dakra/keepassxc.el/actions/workflows/test.yml/badge.svg)](https://github.com/dakra/keepassxc.el/actions/workflows/test.yml)

Talk to a running [KeePassXC](https://keepassxc.org/) from Emacs over
the KeePassXC-Browser protocol.
Plus an [auth-source](https://www.gnu.org/software/emacs/manual/html_mono/auth.html)
backend so Gnus, ERC, smtpmail, Forge and friends can read (and
create) credentials in your KeePassXC database.

All traffic is encrypted with libsodium `crypto_box`
(X25519-XSalsa20-Poly1305) via the
[sodium.el](https://github.com/dakra/sodium.el) dynamic module.

## Features

- **Entry access**: `keepassxc-copy-password`, `keepassxc-copy-username`,
  `keepassxc-copy-totp`, `keepassxc-get-login` with rich completion
  (annotations work with vertico/marginalia out of the box).
  Copied secrets are cleared from the kill-ring after
  `keepassxc-password-timeout` (45s default), like password-store.
  `keepassxc-copy-url` and `keepassxc-browse-url` pick an entry from
  the whole database — completion matches title *and* URL.
- **auth-source backend**: `(keepassxc-auth-source-enable)` and your
  KeePassXC database answers `auth-source-search`, including entry
  creation via `:create t`.
- **Entry management**: `keepassxc-create-login`,
  `keepassxc-delete-entry`, `keepassxc-create-new-group`,
  `keepassxc-generate-password` (uses the KeePassXC generator dialog),
  `keepassxc-request-autotype`.
- **Transient menu**: `M-x keepassxc` shows all commands.
- **Secure association storage**: the per-database association key is
  kept GPG-encrypted in a [plstore](https://www.gnu.org/software/emacs/manual/html_node/epa/Encrypting_002fdecrypting-gpg-files.html)
  (`keepassxc-associations.plist`); the transport keypair is ephemeral
  per connection.
- **Application control**: `keepassxc-lock-all-databases` and
  `keepassxc-open-database` work on every platform; closing databases,
  quitting and non-interactive unlock need D-Bus (Linux).
- **Offline generators**: `keepassxc-cli-generate-password`,
  `keepassxc-cli-diceware` and `keepassxc-cli-estimate-password` run
  `keepassxc-cli` — no unlocked database, no running KeePassXC needed.

## Requirements

- Emacs 28.1+ built with dynamic module support and native JSON
- KeePassXC ≥ 2.6 with **Browser Integration enabled**
  (Settings → Browser Integration → Enable browser integration; no
  browser needs to be configured, only the setting enabled)
- [sodium.el](https://github.com/dakra/sodium.el) and libsodium

## Installation

keepassxc.el depends on the [sodium.el](https://github.com/dakra/sodium.el) dynamic module,
which needs a one-time native build (install `libsodium` and
`pkg-config` first, e.g. `apt install libsodium-dev pkg-config` or
`brew install libsodium pkg-config`).

With use-package and `:vc` (Emacs 30+), letting `package-vc` run the
module build:

```elisp
;; Allow package-vc to run sodium.el's make (builds the native module).
(setq package-vc-allow-build-commands '(sodium))

(use-package sodium
  :vc (:url "https://github.com/dakra/sodium.el" :rev :newest :make "all"))

(use-package keepassxc
  :vc (:url "https://github.com/dakra/keepassxc.el" :rev :newest)
  :config
  (keepassxc-auth-source-enable))
```

Or from local checkouts (run `make` in the sodium.el checkout once):

```elisp
(use-package sodium
  :load-path "~/src/sodium.el")

(use-package keepassxc
  :load-path "~/src/keepassxc.el"
  :commands (keepassxc keepassxc-get-login keepassxc-copy-password)
  :config
  (keepassxc-auth-source-enable))
```

## Setup

1. Enable Browser Integration in KeePassXC (see above).
2. `M-x keepassxc-associate` — KeePassXC pops up a dialog; confirm and
   give the association a name (e.g. "emacs").
3. Set `plstore-encrypt-to` to your GPG key id (or email) so the
   association is stored GPG-encrypted:

   ```elisp
   (setq plstore-encrypt-to "you@example.com")
   ```

   Without it, plstore uses symmetric encryption and asks for a passphrase.

That's it.  The association is per database and stored in
`keepassxc-association-file`; new databases just trigger a new
association dialog on first use.

## Usage

| Command                           | Description                                      |
|-----------------------------------|--------------------------------------------------|
| `keepassxc`                       | Transient menu with all commands                 |
| `keepassxc-copy-password`         | Copy password (auto-cleared from kill-ring)      |
| `keepassxc-copy-username`         | Copy username                                    |
| `keepassxc-copy-totp`             | Copy current TOTP (auto-cleared)                 |
| `keepassxc-copy-url`              | Copy an entry's URL (searches title + URL)       |
| `keepassxc-browse-url`            | Open an entry's URL with `browse-url`            |
| `keepassxc-get-login`             | Select entry; copies username + password         |
| `keepassxc-create-login`          | Create a new entry (offers generated password)   |
| `keepassxc-generate-password`     | Generate a password with the KeePassXC generator |
| `keepassxc-delete-entry`          | Delete an entry (to the recycle bin)             |
| `keepassxc-create-new-group`      | Create a group (`"emacs/mail"` nests)            |
| `keepassxc-request-autotype`      | Trigger KeePassXC global auto-type               |
| `keepassxc-lock-database`         | Lock the current database                        |
| `keepassxc-lock-all-databases`    | Lock all open databases                          |
| `keepassxc-open-database`         | Open a database in KeePassXC                     |
| `keepassxc-cli-generate-password` | Generate a password offline (`keepassxc-cli`)    |
| `keepassxc-cli-diceware`          | Generate a diceware passphrase (`keepassxc-cli`) |
| `keepassxc-cli-estimate-password` | Estimate password entropy (`keepassxc-cli`)      |
| `keepassxc-associate`             | (Re-)associate Emacs with KeePassXC              |

`keepassxc-copy-url`, `keepassxc-browse-url` and
`keepassxc-get-database-entries` list all entries of the database,
which requires "Allow limited access to all entries" to be enabled in
the KeePassXC browser settings.

Programmatic API: `keepassxc-get-logins`, `keepassxc-set-login`,
`keepassxc-get-totp`, `keepassxc-get-database-groups`,
`keepassxc-get-database-entries`, `keepassxc-get-database-hash`.
Entries are hash-tables with keys like `"login"`, `"password"`,
`"name"`, `"uuid"`, `"group"`, `"totp"`.

## auth-source

```elisp
(keepassxc-auth-source-enable)   ; adds 'keepassxc to auth-sources
```

Hosts are looked up as URLs in KeePassXC.  The `:port` (number or
service name) picks the URL scheme via
`keepassxc-auth-source-port-scheme-alist`, falling back to
`keepassxc-default-url-schema`:

```elisp
;; ERC — matches a KeePassXC entry with URL ircs://irc.libera.chat
(auth-source-search :host "irc.libera.chat" :port "6697")

;; smtpmail — entry URL smtp://mail.example.com
(setq smtpmail-smtp-server "mail.example.com"
      smtpmail-smtp-service 587)

;; Forge/ghub — entry URL https://api.github.com, username "you^forge"
(auth-source-search :host "api.github.com" :user "you^forge")
```

The entry's *URL* must match the looked-up URL and the entry's
*username* is matched against `:user`.

Packages that save credentials (`:create t`) work too: the new entry
is written to KeePassXC when the caller invokes the returned
`:save-function`.

When the database is locked, searches return nil (packages then
usually prompt); unlocking KeePassXC automatically flushes
auth-source's negative cache via the `database-unlocked` signal.

## Security notes

- The transport keypair is generated fresh for every connection; only
  the association *id* (plaintext) and the association *public key*
  (GPG-encrypted via plstore) are stored on disk.
- Passwords are never written to disk by this package; copied secrets
  are removed from the kill-ring after `keepassxc-password-timeout`.
- KeePassXC shows its own confirmation dialogs for association and
  (depending on your KeePassXC settings) for entry access.
- On Linux you may prefer Emacs's built-in Secret Service integration
  (`secrets.el`) — KeePassXC implements `org.freedesktop.secrets`.
  This package's socket protocol works on macOS too and offers more
  (TOTP, generator, groups, auto-type).

## Application control

`keepassxc-lock-all-databases` and `keepassxc-open-database` work on
every platform: with a D-Bus session they use the KeePassXC D-Bus
interface, otherwise they invoke `keepassxc-command` (`--lock` /
database path), which reaches the running KeePassXC through its
single-instance socket.
Opening a database shows the unlock prompt there.

D-Bus only (Linux): `keepassxc-open-database-password` (also with key
file) for non-interactive unlock, `keepassxc-close-all-databases`,
`keepassxc-exit`, `keepassxc-refresh-hardware-keys`.  These are hidden
from the transient menu without a D-Bus session.

## keepassxc-cli

`keepassxc-cli-generate-password`, `keepassxc-cli-diceware` and
`keepassxc-cli-estimate-password` run the `keepassxc-cli` program
(`keepassxc-cli-command`) — they need no running KeePassXC and no
database.  Passwords travel over stdin only, never on the command
line.  On macOS the binaries are picked up from the application
bundle automatically when not on `PATH`.

## Development

```sh
make            # compile + checkdoc + test
make test       # ERT suite against an in-Emacs mock KeePassXC
make lint       # package-lint
```

The test suite spins up a mock KeePassXC server (real sodium crypto,
fault injection for chunked/coalesced messages, error codes, corrupt
nonces) on a temporary unix socket — no KeePassXC needed.

## Future work

- Passkeys (`passkeys-get`/`passkeys-register`)
- Async (callback/promise) request API
- Entry browser buffer (tabulated-list)

## License

GPLv3+, see [LICENSE](LICENSE).
