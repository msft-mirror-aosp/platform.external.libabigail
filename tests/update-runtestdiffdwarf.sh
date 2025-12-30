#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestdiffdwarf > build/tests/runtestdiffdwarf-output.txt; python tests/update-test-output.py build/tests/runtestdiffdwarf-output.txt > build/tests/runtestdiffdwarf-update.sh; sh build/tests/runtestdiffdwarf-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestdiffdwarf | tee build/tests/runtestdiffdwarf-output.txt; echo "DONE TESTING!"
