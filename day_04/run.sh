#!/bin/bash

# Runs the solution in the Commodore 64 emulator with input.txt as the input.

ECHO=/usr/bin/echo
DIRNAME=/usr/bin/dirname
PETCAT=/usr/bin/petcat
C1541=/usr/bin/c1541
X64=/usr/bin/x64sc

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$DIRNAME" "$PETCAT" "$C1541" "$X64" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or not executable."
done

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."

$ECHO "=== Converting cleanup.bas to PRG file cleanup.prg ===" && \
$PETCAT -w2 -o ./build/disk/cleanup.prg -- ./cleanup.bas && \
$ECHO "=== Converting input.txt to Petscii text SEQ file input.seq ===" && \
$PETCAT -text -w2 -o ./build/disk/input.seq -- ./input.txt && \
$ECHO "=== Creating a disk image in d64 format ===" && \
$C1541 -format "aoc,1" d64 ./build/disk/disk.d64 && \
$ECHO "=== Writing cleanup.prg to the disk image ===" && \
$C1541 -attach ./build/disk/disk.d64 -write ./build/disk/cleanup.prg cleanup && \
$ECHO "=== Writing input.seq to the disk image ===" && \
$C1541 -attach ./build/disk/disk.d64 -write ./build/disk/input.seq "input,s" && \
$ECHO "=== Showing contents of the disk image ===" && \
$C1541 -attach ./build/disk/disk.d64 -list && \
$ECHO "=== Starting the Commodore 64 emulator ===" && \
$X64 -directory ./build/vice/system/ ./build/disk/disk.d64 && \
$ECHO "=== All OK ===" || \
die "Run failed."
