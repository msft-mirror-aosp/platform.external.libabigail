#!/bin/sh -x

## This is a shell script that runs abidw (on a binary given in
## argument) several times and detects if the several runs yield
## different outputs.
##
## The argument of the script is an abidw command like:
##          $ abidw --options binary
## 

# the last element of the array is the input binary
bin="${@: -1}"
cmd=("$@")
# all the elements of the $@ array but the first one represent the
# arguments of the command line
args=${cmd[@]:1}
f=$(basename $bin)
d=$(dirname $0)
b=$d/../build

if test "x$args" = x;then
    args=$bin
fi

abidw_opts="--annotate --no-corpus-path --no-architecture --no-load-undefined-interfaces --no-parameter-names"
#abidw_opts="--annotate --emit-native-offsets --no-corpus-path --no-architecture --no-load-undefined-interfaces --no-parameter-names"
#abidw_opts="--annotate --emit-native-offsets --no-corpus-path --no-architecture --no-parameter-names"
abilint_opts="--noout"

abifile1=$f.abi
abifile2=$f-2.abi
abifile3=$f-3.abi
abifile4=$f-4.abi
difffile1=diff-$abifile1-$abifile2.txt
difffile2=diff-$abifile1-$abifile3.txt
difffile3=diff-$abifile1-$abifile4.txt

time $b/tests/trun.sh $b/tools/abidw $abidw_opts $args > $abifile1
time $b/tests/trun.sh $b/tools/abilint $abilint_opts $abifile1

time $b/tests/trun.sh $b/tools/abidw $abidw_opts $args > $abifile2
time $b/tests/trun.sh $b/tools/abilint $abilint_opts $abifile2

time $b/tests/trun.sh $b/tools/abidw $abidw_opts $args > $abifile3
time $b/tests/trun.sh $b/tools/abilint $abilint_opts $abifile3

time $b/tests/trun.sh $b/tools/abidw $abidw_opts $args > $abifile4
time $b/tests/trun.sh $b/tools/abilint $abilint_opts $abifile4

diff -u $abifile1 $abifile2 | tee $difffile1
diff -u $abifile1 $abifile3 | tee $difffile2
diff -u $abifile1 $abifile4 | tee $difffile3


