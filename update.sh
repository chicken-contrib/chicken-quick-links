#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
set -x
curl --fail --silent --show-error -o index5.html https://eggs.call-cc.org/5/
curl --fail --silent --show-error -o index4.html https://eggs.call-cc.org/4/
csi -script update.scm
