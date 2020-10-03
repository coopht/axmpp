AXMPP
=====

> A XMPP client library in Ada.

With the AXMPP library you can connect to a Jabber server to send and
receive messages.

## Install
### Using alire
Run

    alr get --build axmpp

### Build from sources
Download sources and run

    make

### Dependencies

The AXMPP library depends on the following software:
 * [gnutls library](http://www.gnu.org/software/gnutls)
 * [GNAT complier](http://www.adacore.com)
 * [Matreshka](https://forge.ada-ru.org/matreshka)

## Usage
Add `with "axmpp";` to your project file.

## Maintainers

 * [@Alexander Basov](https://github.com/coopht)
 * [@Max Reznik](https://github.com/reznikmm)

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/coopht/axmpp/issues/new) or submit PRs.

## License

[BSD-3-Clause](templates/bsd_header) © Alexander Basov
