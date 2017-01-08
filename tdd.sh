inputLocation=compi4
toolsLocation=testing-tools
testLocation=do-not-touch


if [ ! -d $testLocation ]; then 
	mkdir $testLocation
fi
cp $toolsLocation/* $testLocation
cp $inputLocation/* $testLocation
cd $testLocation

echo HW4 tdd:
scheme --script test-hw4.scm
