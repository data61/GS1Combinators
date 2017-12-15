#!/usr/bin/env sh

if [ $# -eq 1 ] && [ "$1" = "-c" ]; then
   stack clean
fi

test_report=`stack build --coverage; stack test --coverage 2>&1 | egrep -i "The coverage report for GS1Combinators's test-suite \"GS1Combinators-test\" is available at " | sed "s/The coverage report for GS1Combinators's test-suite \"GS1Combinators-test\" is available at //g"`;


(google-chrome $test_report &)
echo
