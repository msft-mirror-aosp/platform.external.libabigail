#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestdifffilter > build/tests/runtestdifffilter-output.txt; python tests/update-test-output.py build/tests/runtestdifffilter-output.txt > build/tests/runtestdifffilter-update.sh; sh build/tests/runtestdifffilter-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestdifffilter | tee build/tests/runtestdifffilter-output.txt; echo "DONE TESTING!"
