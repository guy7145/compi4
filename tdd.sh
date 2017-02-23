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
example_scm_files=scm_examples
testLocation=do-not-touch

compare_script_file="automated-compare-tests.scm" # should be same as in test-hw4.scm!

rm -rf $testLocation

echo -e $Blue
if [ ! -d $testLocation ]; then 
	mkdir $testLocation
fi
cp --recursive $toolsLocation/* $testLocation
cp --recursive $inputLocation/* $testLocation
cp --recursive $example_scm_files/* $testLocation
cd $testLocation
echo -e $NC

echo "HW4: scheme --> c..."
scheme --script compile-files.scm
echo ""

echo "HW4: c --> executable..."
for file in *.c
do
	echo -e $Green"$file -> ${file%.c}.o"$NC
	
	# compile
 	gcc -o ${file%.c}.o $file
	
	# run both languages
	scheme --quiet < ${file%.c}.scm > ${file%.c}.out1
	sh -c "./${file%.c}.o >${file%.c}.out2 2>${file%.c}.out2"
	# format lists
	sed -i "s/(/'(/" ${file%.c}.out1
	sed -i "s/(/'(/" ${file%.c}.out2
done
echo ""

echo running automatic tests...
scheme --script $compare_script_file











