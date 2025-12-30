#!/bin/sh -x

date; time ./build/tests/runtestreadwrite > build/tests/runtestreadwrite-output.txt; tests/update-test-output.py build/tests/runtestreadwrite-output.txt > build/tests/runtestreadwrite-update.sh; sh build/tests/runtestreadwrite-update.sh; echo "TESTING the update ..."; date; time ./build/tests/runtestreadwrite | tee build/tests/runtestreadwrite-output.txt; echo "DONE TESTING!";
