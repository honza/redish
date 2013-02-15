Redish
======

A multi-threaded, TCP, key-value store inspired by Redis implemented in
Haskell.

**Note**: This is a learning exercise.

Building
--------

    $ git clone git://github.com/honza/redish.git
    $ cd redish
    $ cabal configure
    $ cabal build
    $ ./dist/build/redish/redish 7777

Protocol
--------

    get key
    set key name

`key` may not contain spaces

Example
-------

    $ telnet localhost 7777
    set name honza
    OK
    get name
    honza
    set name mrhonza
    OK
    get name
    mrhonza
    get age
    null

License
-------

BSD, short and sweet.
