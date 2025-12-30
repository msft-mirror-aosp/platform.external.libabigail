#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestdiffpkg > build/tests/runtestdiffpkg-output.txt; python tests/update-test-output.py build/tests/runtestdiffpkg-output.txt > build/tests/runtestdiffpkg-update.sh; sh build/tests/runtestdiffpkg-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestdiffpkg | tee build/tests/runtestdiffpkg-output.txt; echo "DONE TESTING!"
