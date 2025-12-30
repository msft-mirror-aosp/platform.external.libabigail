#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestreadbtf > build/tests/runtestreadbtf-output.txt; python tests/update-test-output.py build/tests/runtestreadbtf-output.txt > build/tests/runtestreadbtf-update.sh; sh build/tests/runtestreadbtf-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestreadbtf | tee build/tests/runtestreadbtf-output.txt; echo "DONE TESTING!"
