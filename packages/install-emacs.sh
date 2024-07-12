#!/bin/bash

cd ~
git clone --depth=1 git://git.savannah.gnu.org/emacs.git
cd emacs
./autogen.sh
./configure --with-native-compilation --with-tree-sitter --with-json
make -j$(nproc)
