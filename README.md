Redish
======

A multi-threaded, TCP, key-value store inspired by Redis implemented in
Haskell.

**Note**: This is a learning exercise.

You can [read the introductory blog post][1].

Building
--------

    $ git clone git://github.com/honza/redish.git
    $ cd redish
    $ stack build
    $ stack exec Redish

Protocol
--------

Redish implements the Redis protocol.  It currently only supports the get and
set operations.

Example
-------

```
$ redis-cli -p 7777
127.0.0.1:7777> set name honza
OK
127.0.0.1:7777> get name
"honza"
```

License
-------

BSD, short and sweet.

[1]: https://honza.ca/2015/09/building-a-redis-clone-in-haskell
