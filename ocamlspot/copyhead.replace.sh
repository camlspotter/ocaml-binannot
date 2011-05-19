#!/bin/sh

ML="(*   Copyright 2008-2011 Jun Furuse. All rights reserved.              *)"
MAKE="#     Copyright 2008-2011 Jun Furuse. All rights reserved.              #"

for file in `find . -type f | xargs grep -l 'Copyright.*Jun Furuse'`
do
  case "$file" in
  *.ml*)
    sed -e "s/.*Copyright.*Jun Furuse.*/$ML/" $file > $file.tmp
     /bin/mv $file.tmp $file
    ;;
  *.txt*)
    sed -e "s/.*Copyright.*Jun Furuse.*/$ML/" $file > $file.tmp
     /bin/mv $file.tmp $file
    ;;
  *Makefile*)
    sed -e "s/.*Copyright.*Jun Furuse.*/$MAKE/" $file > $file.tmp
    /bin/mv $file.tmp $file
    ;;
  esac
done

