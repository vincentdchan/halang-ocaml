#!/bin/bash

TESTS=$(ls ./tests/parser)

# echo $TESTS

for i in $TESTS
do
    ./parser < ./tests/parser/$i/actual.ha > ./tests/parser/$i/result.txt
    RE=$(diff -c ./tests/parser/$i/ast.txt ./tests/parser/$i/result.txt)
    if [ "$RE" = "" ]; then
        echo "PASS"
    else
        diff -c ./tests/parser/$i/ast.txt ./tests/parser/$i/result.txt
        echo "./tests/parser/$i/result.txt"
        cat ./tests/parser/$i/result.txt
        rm ./tests/parser/$i/result.txt
        exit 1
    fi
    rm ./tests/parser/$i/result.txt
done
