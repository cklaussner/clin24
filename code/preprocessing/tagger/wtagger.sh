#!/bin/bash



FILES=/home/carmen/git/source-language-recognition/dataK/*

for dir in $FILES; do

    for file in "$dir"/*; do
    

	echo "Processing $file file..."
	  /home/carmen/git/source-language-recognition/tools/tagger/cmd/tree-tagger-english $file /home/carmen/git/source-language-recognition/tools/tagger/test/$file
    
    
    
    
    
    done
done
