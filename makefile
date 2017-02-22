all:
	gcc -o my-cisc.o my-cisc.c
	echo "\033[1;36mrunning 'my-cisc':\033[0m"
	./my-cisc.o
	rm ./my-cisc.o