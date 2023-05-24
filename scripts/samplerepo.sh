#!/bin/bash

#REPO=$HOME/temporal/repo.git
REPO=$(pwd)/repo.git
MAXCOMMITS=2
DRY=

GIT () {
  if [ "$DRY" == "" ]; then
    case "$#" in
      1)
      git "$1"
      ;;
      2)
      git "$1" "$2"
      ;;
      3)
      git "$1" "$2" "$3"
      ;;
    esac
  else
    echo "git $1 $2 $3 $4 $5 $6 $7 $8"
  fi
}

CD () {
  if [ "$DRY" == "" ]; then
    cd $1
  else
    echo "cd $1"
  fi
}

makerepo () {
  if [ ! -d repo.git ]; then
    echo "Initializing a bare repository"
    GIT init --bare repo.git
  else
    echo "The repository repo.git already exists"
  fi
}

clonerepos () {
  if [ ! -d gita ]; then
    echo "Cloning sample repositories"
    GIT clone file://$REPO gita
    #git clone file://$REPO gitb
    #git clone file://$REPO gitc
  else
    echo "The reposositories are already created"
  fi
}

docommits () {

  echo "Creating commits in $1"

  CD $1

  FILE=notes_$1.txt
  if [ "$DRY" == "" ]; then
    BRANCH=$(git symbolic-ref --short HEAD)
  else
    BRANCH=XYZ
  fi

  if [ ! -f $FILE ]; then
    echo "Initial text" > $FILE
    GIT add $FILE
    GIT commit -m "$1-$BRANCH: Added file $FILE"
  else
    if [[ "$OSTYPE" == "darwin"* ]]; then
      FILESIZE=$(stat -f %z "$FILE")
    else
      FILESIZE=$(stat -c%s "$FILE")
    fi
    MSG=" - now with $FILESIZE bytes"
  fi

  for i in $(seq 1 $MAXCOMMITS); do
    echo "Line $i$MSG" >> $FILE
    GIT add $FILE
    GIT commit -m "$1-$BRANCH: added Line $i$MSG"
    if [ "$DRY" == "" ]; then
      sleep 1
    else
      echo "sleep 1"
    fi
  done


  CD ..

}

createbranch () {
  CD $1
  GIT branch $2 master
  GIT switch $2
  CD ..
}

switchbranch () {
  CD $1
  GIT switch $2
  CD ..
}

mergebranch () {
  CD $1
  GIT merge $2
  CD ..
}

recreatemergetoright () {
  docommits $1
  docommits $1
  docommits $1
  createbranch $1 algo
  docommits $1
  switchbranch $1 master
  docommits $1
  mergebranch $1 algo
  switchbranch $1 algo
  docommits $1
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

#recreatemergetoright gita

