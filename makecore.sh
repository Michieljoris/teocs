#!/bin/bash
if [ -n "$1" ]
then 
  file=$1
else
  echo 'no file name given..'
  exit 0
fi

filename=${file%.*p} 
sbcl --eval "(load (compile-file $file)" --eval "(sb-ext:save-lisp-and-die $filename :executable t)" --eval "(quit)"
