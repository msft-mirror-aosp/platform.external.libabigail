#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestabidiffexit > build/tests/runtestabidiffexit-output.txt; python tests/update-test-output.py build/tests/runtestabidiffexit-output.txt > build/tests/runtestabidiffexit-update.sh; sh build/tests/runtestabidiffexit-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestabidiffexit | tee build/tests/runtestabidiffexit-output.txt; echo "DONE TESTING!"
