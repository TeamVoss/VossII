#!/usr/bin/env python3

""" Runs a few tests to check the signature functions. """

# NOTE: This script is ugly, not robust, etc. It is not intended to be more
# than a quick and dirty test script.

from subprocess import check_output
import sys
import os

TESTS_DIR = "./sigtests"

had_error = False


def sigOf(path):
    return check_output(['./sig.bin', path])


def testSame(path):
    global had_error
    for sub in os.scandir(path):
        if not sub.is_dir():
            continue

        commonSig = None
        for f in os.scandir(sub.path):
            sig = sigOf(f.path)
            if commonSig is None:
                commonSig = sig
            else:
                if sig != commonSig:
                    print('ERROR: {}'.format(sub.name), file=sys.stderr)
                    had_error = True
                    break


def testDiff(path):
    global had_error
    for sub in os.scandir(path):
        if not sub.is_dir():
            continue

        sigs = set()
        for f in os.scandir(sub.path):
            sig = sigOf(f.path)
            if sig in sigs:
                print('ERROR: {}'.format(sub.name), file=sys.stderr)
                had_error = True
                break
            else:
                sigs.add(sig)


print(">> TESTING SAME")
testSame(os.path.join(TESTS_DIR, 'same'))
print(">> TESTING DIFF")
testDiff(os.path.join(TESTS_DIR, 'diff'))

if had_error:
    print("Errors occurred :c")
sys.exit(had_error)
