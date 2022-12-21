#!/bin/bash

cd ~
git clone git://git.savannah.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation --with-tree-sitter --with-json
make -j$(nproc)
