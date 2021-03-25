#!/usr/bin/bash

#
# Use this file to run a test. For example:
#
#     ./test.sh CsvTest
#
# NOTE: do not include the .hs extension!
#

MODULE=$1

runhaskell -i../ "$MODULE.hs"
