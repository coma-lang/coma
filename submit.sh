#!/usr/bin/bash

# Use this script to prepare submission files.

folder=dist-submission

./install.sh

if [ -d $folder ]; then rm -rf $folder; fi
mkdir $folder

cp -r app src test coma.cabal README.md LICENSE ChangeLog.md hie.yaml $folder
for n in {1..5}; do
    cp "problems/pr$n/pr$n.cql" "$folder/pr$n.cql"
done

cd $folder
zip -r "$folder.zip" *
