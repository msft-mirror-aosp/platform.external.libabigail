#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestabidiff > build/tests/runtestabidiff-output.txt; python tests/update-test-output.py build/tests/runtestabidiff-output.txt > build/tests/runtestabidiff-update.sh; sh build/tests/runtestabidiff-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestabidiff | tee build/tests/runtestabidiff-output.txt; echo "DONE TESTING!"
