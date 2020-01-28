#pragma once
#include <cstdio>

#define LOG_WARNING(fmt, ...) fprintf(stderr, "[WARN] " fmt "\n", __VA_ARGS__)
