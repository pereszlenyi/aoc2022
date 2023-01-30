#!/bin/bash

# Builds and runs the solution with input.txt as the input.

ECHO=/usr/bin/echo
DIRNAME=/usr/bin/dirname
CMAKE=/usr/bin/cmake

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$DIRNAME" "$CMAKE" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or not executable."
done

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."

BINARY=most_calories

$ECHO "=== Building ===" && \
$CMAKE -S ./ -B ./build && \
$CMAKE --build ./build && \
$ECHO "=== Running ${BINARY} ===" && \
./build/${BINARY} ./input.txt && \
$ECHO "=== All OK ===" || \
die "Build or run failed."
