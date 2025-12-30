#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestannotate > build/tests/runtestannotate-output.txt; python tests/update-test-output.py build/tests/runtestannotate-output.txt > build/tests/runtestannotate-update.sh; sh build/tests/runtestannotate-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestannotate | tee build/tests/runtestannotate-output.txt; echo "DONE TESTING!"
