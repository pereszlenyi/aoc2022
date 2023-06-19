#!/bin/bash

# This script starts DOSBox and, inside DOSBox,
# it builds the solution with Turbo Pascal 5.5.
# Then it runs the program with input.txt as the input.

ECHO=/usr/bin/echo
DIRNAME=/usr/bin/dirname
MKDIR=/usr/bin/mkdir
RM=/usr/bin/rm
SHASUM=/usr/bin/sha256sum
GREP=/usr/bin/grep
UNZIP=/usr/bin/unzip
CP=/usr/bin/cp
TODOS=/usr/bin/todos
DOSBOX=/usr/bin/dosbox

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$DIRNAME" "$MKDIR" "$RM" "$SHASUM" "$GREP" "$UNZIP" "$CP" "$TODOS" "$DOSBOX" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or it's not executable."
done

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."

C_DRIVE=./build/c_drive

function delete_shared_folder {
	$RM -rf "$C_DRIVE"
}

function die_with_delete {
	delete_shared_folder
	die "$1"
}

$ECHO "=== Creating shared folder for the DOS C: drive ===" && \
delete_shared_folder && \
$MKDIR -p "$C_DRIVE" && \
$ECHO "=== Verifying Turbo Pascal 5.5 ===" && \
$SHASUM ./build/tp55.zip | \
$GREP -F "a04ff65555f88cd7714a11362c6f625f254932679ad5623b207015893ef638b6" && \
$ECHO "=== Extracting Turbo Pascal 5.5 from archive ===" && \
$UNZIP ./build/tp55.zip -d "$C_DRIVE" && \
$ECHO "=== Copying files to the shared folder ===" && \
$CP ./stacks.pas ./input.txt "$C_DRIVE" && \
$ECHO "=== Converting text files to DOS format ===" && \
$TODOS "${C_DRIVE}/stacks.pas" "${C_DRIVE}/input.txt" && \
$ECHO "=== Starting DOSBox ===" && \
$DOSBOX -c "mount c $C_DRIVE" \
	-c "z:\config.com -securemode" \
	-c "c:" \
	-c "Disk1\TPC.EXE /\$I+ /\$R+ /\$S+ /\$V+ stacks.pas" \
	-c "STACKS.EXE input.txt" && \
delete_shared_folder && \
$ECHO "=== All OK ===" || \
die_with_delete "Build or run failed."
