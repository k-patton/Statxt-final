Language: Statxt

Members:
Darpan Chaudhary, System Architect
dc3292@columbia.edu 
Xiao Lim, Project Manager
xl2669@columbia.edu 
Zion Lin, Language Guru
zel2109@columbia.edu 
Katherine Patton, Tester
kyp2106@columbia.edu 

Welcome to Statxt. This is the result of a long semester's work on our langage
Statxt and its compiler. 

Statxt is a language which specializes in analyzing textual statistics. 


To compile the compiler:
type 
$ make clean
$ make 

To execute the compiler: 
To run all our test script on files test-*.stxt and fail-*.stxt in tests/
that do NOT require stdlib functions simply type:
$ ./testall.sh

To run all our test script on files test-*.stxt and fail-*.stxt in stdlibstests/
that do require stdlib functions simply type:
$ ./stdlibtestall.sh 

To run a specific program.stxt (this will copy in the stdlib functions)
contained in stdlib.stxt) type:
$ ./statxt.sh program.stxt 

Manual compilation and execution of program.stxt can be achieve via the
following steps that do NOT include copying in stdlib functions:
$ ./statxt.native mystatxtcode.stxt > mystatxtcode.ll
$ llc mystatxtcode.ll > mystatxtcode.s
$ cc -o mystatxtcode.exe mystatxtcode.s cfunctions.o
$ ./mystatxtcode.exe



This folder contains a lot of code which consists of three groups: 
1. The compiler code and stdlib.stxt (toplevel)
2. The compiler feature tests (QA for our features; /tests) 
3. The demo code (/examples)

Section 1:
Files included: 
scanner.mll
parser.mly
ast.ml
semant.ml
sast.ml
codegen.ml
cfunctions.c
statxt.ml
stdlib.stxt
Makefile 

Section 2: 
Includes two shell scripts on the top level for automation of testing:
testall.sh (tests all (omits stdlib functions))
stdlibtestall.sh (tests all (copies in stdlib functions))
statxt.sh  (copies stdlib functions into given file and runs it)
usage:
	./testall.sh
	./stdlibtestall.sh
	./statxt.sh ./tests/test-structs.stxt

testall.sh runs all tests in tests/
stdlibtestall.sh runs all tests in stdlibtests/


Section 3: 
The demo code consists of a shell script on the top level that 
automates copying of the standard library functions and the contents of the
file passed in as an argument into a third temp file for compilation. the script
assumes that stdlib.stxt in the same directory:
statxt.sh
usage:
	./statxt.sh ./examples/syllables.stxt

The rest of the demo code can be found in /examples

The three tests are:
syllables.stxt 
word_frequency.stxt
book_record.stxt 


More about the demo code: 
syllables.stxt
This file print an estimate of the number of syllables of each word in the file,
the average number of syllables, and computes the Flesch Reading Ease Score as
well as the Flesch Kincaid grade level based on the sample text in
./examples/sample_test.txt

word_frequency.stxt
It prints the word frequency of each word in the sample text at
./examples/sample_text.txt

book_record.stxt
It stores grouped information about each book in a struct
and prints this information. 
