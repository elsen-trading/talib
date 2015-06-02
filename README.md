talib
-----

Elsen bindings to Talib.

Function names match their ta-lib counterparts, but are lower case.

The talib package requires ta-lib. See [ubuntuBuildDeps.sh](ubuntuBuildDeps.sh) for an example of how to install on Ubuntu.

```bash
$ wget http://prdownloads.sourceforge.net/ta-lib/ta-lib-0.4.0-src.tar.gz && tar xzvf ta-lib-0.4.0-src.tar.gz
$ cd ta-lib
$ ./configure
$ make
$ sudo make install
$ sudo ln -s /usr/local/lib/libta_lib.so.0 /usr/lib/libta_lib.so.0
$ sudo ldconfig
```

[![Build Status](https://magnum.travis-ci.com/elsen-trading/talib.svg?token=BpJfxk8kj7YxSxz44Sq9)](https://magnum.travis-ci.com/elsen-trading/talib)
