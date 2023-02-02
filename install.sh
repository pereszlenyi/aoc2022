#!/bin/bash

# Run this script to install all the compilers and interpreters
# required to build and run the solutions.
# It is only meant to work with Ubuntu.

ECHO=/usr/bin/echo
SUDO=/usr/bin/sudo
DIRNAME=/usr/bin/dirname
LS=/usr/bin/ls
APTGET=/usr/bin/apt-get
ANSIBLE=/usr/bin/ansible-playbook

function die {
	$ECHO "Error: $1"
	exit 1
}

for FILE in "$ECHO" "$SUDO" "$DIRNAME" "$LS" "$APTGET" ; do
	[ -x "$FILE" ] || die "'$FILE' doesn't exist or not executable."
done

function check_ansible {
	[ -x "$ANSIBLE" ]
}

DIR=$($DIRNAME "$0")
cd $DIR || die "Can't enter $DIR."
$LS ./install.ansible.yml >/dev/null || die "Ansible playbook is missing."

check_ansible || ($ECHO "Installing ansible:" && $SUDO $APTGET install ansible && $ECHO "")
check_ansible || die "Ansible is not installed."

$ANSIBLE ./install.ansible.yml --ask-become-pass && \
$ECHO "Install was successful." || \
die "Install failed."
