//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include <errno.h>

extern "C" int fl_main(int argc, char *argv[]);

int
main(int argc, char *argv[])
{
    fl_main(argc, argv);
}
