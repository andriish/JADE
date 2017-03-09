#!/bin/bash
set -x
rm -rf ./JADE-2017.1
svn export ./ ./JADE-2017.1
rm -rf JADE-2017.1/DPHEP2017
rm -rf JADE-2017.1/Note
tar czf JADE-2017.1.tar.gz ./JADE-2017.1