#!/usr/bin/bash

# Use this script to prepare submission files.

folder=dist-submission

if [ -d $folder ]; then rm -rf $folder; fi
mkdir $folder

cp group.txt report.pdf $folder

for n in {6..10}; do
    cp "problems/pr$n/pr$n.cql" "$folder/pr$n.cql"
done

cd $folder
zip -r "$folder.zip" *
