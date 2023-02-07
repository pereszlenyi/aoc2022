#!/bin/bash

# Builds and runs the solution with input.txt as the input.

ECHO=/usr/bin/echo
DIRNAME=/usr/bin/dirname
MKDIR=/usr/bin/mkdir
COBOL=/usr/bin/cobc

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$DIRNAME" "$MKDIR" "$COBOL" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or not executable."
done

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."

BINARY=rock_paper_scissors

$ECHO "=== Building ===" && \
$MKDIR -p ./build && \
$COBOL -W -O3 -x ./${BINARY}.cbl -o ./build/${BINARY} && \
$ECHO "=== Running ${BINARY} ===" && \
./build/${BINARY} && \
$ECHO "=== All OK ===" || \
die "Build or run failed."
