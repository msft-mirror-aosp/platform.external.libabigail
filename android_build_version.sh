#!/bin/sh

set -x
set -e

dir="$1"
out="$2"

version=$(git -C "$dir" describe --abbrev=8 --always)-soong

echo "#define ANDROID_BUILD_VERSION \"${version}\"" > "$out"
