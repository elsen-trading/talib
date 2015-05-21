#!/bin/bash
sudo apt-get install build-essential

TMPDIR=ta_tmp
mkdir $TMPDIR
cd $TMPDIR
wget http://prdownloads.sourceforge.net/ta-lib/ta-lib-0.4.0-src.tar.gz
tar xzvf ta-lib-0.4.0-src.tar.gz
cd ta-lib && ./configure && make && sudo make install
sudo ln -s /usr/local/lib/libta_lib.so.0 /usr/lib/libta_lib.so.0
sudo ldconfig
cd ../..

if [ -d "$TMPDIR" ]; then
   rm -rf "$TMPDIR"
fi
