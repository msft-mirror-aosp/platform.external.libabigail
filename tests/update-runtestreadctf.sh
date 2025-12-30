#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestreadctf > build/tests/runtestreadctf-output.txt; python tests/update-test-output.py build/tests/runtestreadctf-output.txt > build/tests/runtestreadctf-update.sh; sh build/tests/runtestreadctf-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestreadctf | tee build/tests/runtestreadctf-output.txt; echo "DONE TESTING!"
