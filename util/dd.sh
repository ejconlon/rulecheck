#!/usr/bin/env sh

# A script thats useful for delta-debugging

timeout 4 stack run -- --searchterm "$1" "(,) Int Int"
RESULT=$?
if [ $RESULT -ne 0 ]; then
    echo "Error $RESULT"
    exit $RESULT
else
    echo "OK"
fi
