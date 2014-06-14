hskkserv
===
skkserv implemented in haskell.

Installation(mac)
---

```.bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
$ ./dist/build/register-skk-dict/register-skk-dict ~/.dict.sqlite3 SKK-JISYO.L
$ eval `./dist/build/hskkserv/hskkserv --plist`
```
