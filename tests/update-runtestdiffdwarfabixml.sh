#!/bin/sh -x

date; echo "Updating the ABIXML files ..."; time build/tests/trun.sh build/tools/abidw tests/data/test-diff-dwarf-abixml/test0-pr19026-libvtkIOSQL-6.1.so.1 > tests/data/test-diff-dwarf-abixml/test0-pr19026-libvtkIOSQL-6.1.so.1.abi; date; time build/tests/trun.sh build/tools/abidw tests/data/test-diff-dwarf-abixml/PR25409-librte_bus_dpaa.so.20.0 > tests/data/test-diff-dwarf-abixml/PR25409-librte_bus_dpaa.so.20.0.abi; echo "... DONE";

echo "Running the test now: "
date; time build/tests/runtestdiffdwarfabixml
echo "UPDATED TEST RUNNING DONE"
