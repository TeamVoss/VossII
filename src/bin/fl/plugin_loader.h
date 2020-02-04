//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

/* Initialisation and loading of functions residing in plugins */
#ifndef __PLUGIN_LOADER_H
#define __PLUGIN_LOADER_H
#include "plugin_errlog.h"

typedef struct fl_plugin_api_rec* fl_plugin_api;
typedef struct fl_plugin_rec* fl_plugin;
typedef struct fl_plugin_fun_rec* fl_plugin_funs;
typedef struct fl_plugin_type_rec* fl_plugin_types;

extern fl_plugin_api plugin_api;

int load_plugin(char* plugin_name, char** out);
void check_plugin(fl_plugin plugin, plugin_errlog log);
void check_plugin_types(int num_types, fl_plugin_types types, plugin_errlog log);
void check_plugin_funs(int num_funs, fl_plugin_funs funs, plugin_errlog log);
int register_types(int num_types, fl_plugin_types types);
int register_funs(int num_funs, fl_plugin_funs funs);

#endif
