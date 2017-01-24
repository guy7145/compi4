#!/bin/bash

# bright colors:
DarkGray='\033[1;30m'
LightRed='\033[1;31m'
LightGreen='\033[1;32m'
Yellow='\033[1;33m'
LightBlue='\033[1;34m'
LightPurple='\033[1;35m'
LightCyan='\033[1;36m'
White='\033[1;37m'

# dark colors:
Black='\033[0;30m'
Red='\033[0;31m'
Green='\033[0;32m'
Brown='\033[0;33m' # /Orange?
Blue='\033[0;34m'
Purple='\033[0;35m'
Cyan='\033[0;36m'
LightGray='\033[0;37m'

NC='\033[0m' # No Color


inputLocation=compi4
toolsLocation=testing-tools
testLocation=do-not-touch


rm -rf $testLocation


echo -e $Blue
if [ ! -d $testLocation ]; then 
	mkdir $testLocation
fi
cp $toolsLocation/* $testLocation
cp $inputLocation/* $testLocation
cd $testLocation
echo -e $NC

echo "_____________________________________________________________________________________________________________________________"

# --HW1-------------------------------------------------------

echo HW1 compare tests:
#echo commented out...
scheme --script compare-tests-hw1.scm | grep TESTS


# --HW2-------------------------------------------------------

#<<COMMENT

# .::part1::.
echo HW2 part1 compare tests:
scheme --script compare-tests-hw2.scm | grep TESTS

echo HW2 part1 my compare tests:
scheme --script my-compare-tests-hw2.scm | grep TESTS

# .::part2::.
echo HW2 part2 compare tests:
scheme --script compare-tests-hw2-CSE.scm | grep TESTS

echo HW2 part2 my compare tests:
scheme --script my-compare-tests-hw2-CSE.scm | grep TESTS

#COMMENT

# --HW3-------------------------------------------------------

echo HW3 tdd:
scheme --script test-hw3.scm 

echo HW3 compare tests:
scheme --script compare-tests-hw3.scm | grep TESTS

# i have no custom compare tests... :(
#echo HW3 my compare tests:
#echo commented out...
#scheme --script my-compare-tests-hw3.scm





