#!/bin/sh -x

date; time build/tests/trun.sh build/tests/runtestreaddwarf > build/tests/runtestreaddwarf-output.txt; python tests/update-test-output.py build/tests/runtestreaddwarf-output.txt > build/tests/runtestreaddwarf-update.sh; sh build/tests/runtestreaddwarf-update.sh; echo "TESTING the update ..."; date; time build/tests/trun.sh build/tests/runtestreaddwarf | tee build/tests/runtestreaddwarf-output.txt; echo "DONE TESTING!"
