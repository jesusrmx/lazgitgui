#!/bin/bash

REPO=$HOME/temp/repo.git

makerepo () {
  if [ ! -d repo.git ]; then
    echo "Initializing a bare repository"
    git init --bare repo.git
  else
    echo "The repository repo.git already exists"
  fi
}

clonerepos () {
  if [ ! -d gita ]; then
    echo "Cloning sample repositories"
    git clone file://$REPO gita
    #git clone file://$REPO gitb
    #git clone file://$REPO gitc
  else
    echo "The reposositories are already created"
  fi
}

docommits () {

  echo "Creating commits in $1"

  FILE=notes_$1.txt
  cd $1

  if [ ! -f $FILE ]; then
    echo "Initial text" > $FILE
    git add $FILE
    git commit -m "$1: Added file $FILE"
  else
    FILESIZE=$(stat -c%s "$FILE")
    MSG=" - now with $FILESIZE bytes"
  fi

  for i in {1..5}; do
    echo "Line $i$MSG" >> $FILE
    git add $FILE
    git commit -m "$1: added Line $i$MSG"
    sleep 1
  done

  cd ..

}

if [ "$1" == "clean" ]; then
    echo "cleaning..."
    rm -rf repo.git
    rm -rf gita
    rm -rf gitb
    rm -rf gitc
fi

makerepo
clonerepos

docommits gita
#docommits gitb
#docommits gitc


