//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#pragma once
#include <cstdio>

#define LOG_WARNING(fmt, ...) fprintf(stderr, "[WARN] " fmt "\n", __VA_ARGS__)
