# haskell-stomp

Beginning work on a client for the [STOMP messaging protocol](https://stomp.github.io/) written in Haskell. Currently a minimal demo which sends a CONNECT and SUBSCRIBE frame and receives one frame. Parses using the [attoparsec](https://hackage.haskell.org/package/attoparsec-0.14.4) parser combinator library to handle Partial parsing for eventual stream parsing support.
