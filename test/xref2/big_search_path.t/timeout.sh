#!/bin/sh

cmd=$2

timeout=$1

($cmd ; echo 'done') & sleep $timeout ; kill $! 2> /dev/null || :
