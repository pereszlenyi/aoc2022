#!/bin/bash

# Builds and runs the solution with input.txt as the input.

ECHO=/usr/bin/echo
DIRNAME=/usr/bin/dirname
MKDIR=/usr/bin/mkdir
LISP=/usr/bin/clisp

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$DIRNAME" "$MKDIR" "$LISP" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or not executable."
done

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."

PROGRAM=rucksack

$ECHO "=== Building ===" && \
$MKDIR -p ./build && \
$LISP -c ./${PROGRAM}.lisp -o ./build/ && \
$ECHO "=== Running ${PROGRAM} ===" && \
$LISP ./build/${PROGRAM}.fas ./input.txt && \
$ECHO "=== All OK ===" || \
die "Build or run failed."
