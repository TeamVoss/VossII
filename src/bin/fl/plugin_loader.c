//-------------------------------------------------------------------
// Copyright 2020 Carl-Johan Seger
// SPDX-License-Identifier: Apache-2.0
//-------------------------------------------------------------------

#include <stdlib.h>
#include <dlfcn.h>
#include <limits.h>
#include <dirent.h>
#include "plugin_loader.h"
#include "plugin_errlog.h"
#include "plugin.h"

#define MAX_ERROR_SIZE 5120

/* Error logger to use when fl_init calls back into register_types/register_funs. */
static plugin_errlog current_errlog = NULL;

/* Current # of funs registered by current plugin using register_funs. */
static int current_funs_registered = 0;

/* Current plugin, if registered. */
static fl_plugin current_plugin = NULL;

/* Does the current plugin metadata pass all sanity checks? */
static bool current_plugin_metadata_ok = 0;

/* Do the types exported by the current plugin pass all sanity checks?
   Defaults to 1 since plugins don't need to export types.
*/
static bool current_plugin_types_ok = 1;

typedef struct _plugin_list {
    fl_plugin plugin;
    struct _plugin_list* next;
} _plugin_list;

static _plugin_list* loaded_plugins = NULL;

int
plugin_already_loaded(fl_plugin plugin)
{
    for(_plugin_list* l = loaded_plugins; l != NULL; l = l->next) {
        if(!strcmp(plugin->name, l->plugin->name)) {
            return 1;
        }
    }
    return 0;
}

void
mark_plugin_as_loaded(fl_plugin plugin)
{
    _plugin_list* head = malloc(sizeof(_plugin_list));
    head->plugin = plugin;
    head->next = loaded_plugins;
    loaded_plugins = head;
}

/* Sets the fixity for the plugin. */
static void
insert_fixity(fl_plugin_fun f)
{
    switch(f->fixity) {
    case NO_FIXITY:
        /* Nothing to do here. */
        break;
    case PREFIX:
        Insert_prefix(f->name, f->precedence);
        break;
    case INFIXL:
        Insert_infix(f->name, f->precedence);
        break;
    case INFIXR:
        Insert_infixr(f->name, f->precedence);
        break;
    case POSTFIX:
        Insert_postfix(f->name);
        break;
    }
}

/* Check metadata for the given plugin, and register it if the metadata is OK. */
static fl_plugin_api
register_plugin(fl_plugin plugin)
{
    if(plugin_already_loaded(plugin)) {
        log_plugin_error(current_errlog, "plugin is already loaded.");
        return NULL;
    }

    current_plugin = plugin;
    check_plugin(plugin, current_errlog);
    if(num_plugin_errors(current_errlog) == 0) {
        current_plugin_metadata_ok = 1;
        return plugin_api;
    }
    return NULL;
}

/* Check and load all the given types. */
/* No-op if plugin metadata hasn't been successfully checked. */
int
register_types(int num_types, fl_plugin_types types)
{
    if(!current_plugin_metadata_ok) {
        return -1;
    }

    check_plugin_types(num_types, types, current_errlog);
    if(num_plugin_errors(current_errlog) > 0) {
        current_plugin_types_ok = 0;
        return -1;
    }

    for(int i = 0; i < num_types; i++) {
        types[i].class =
            Add_ExtAPI_Object( types[i].name
                             , types[i].mark
                             , types[i].sweep
                             , types[i].save
                             , types[i].load
                             , types[i].obj2string
                             , types[i].equals
                             , types[i].gmap
                             , types[i].gmap2
			     , NULL		// No SHA256 computation
                             );
        types[i].type = Get_Type(types[i].name, NULL, TP_INSERT_FULL_TYPE);
    }
    return 0;
}

/* Load and check all the given functions. */
/* No-op if plugin metadata hasn't been successfully checked. */
int
register_funs(int num_funs, fl_plugin_funs funs)
{
    if(!current_plugin_metadata_ok) {
        return -1;
    }

    check_plugin_funs(num_funs, funs, current_errlog);
    if(num_plugin_errors(current_errlog) > 0) {
        return -1;
    }

    for(int i = 0; i < num_funs; i++) {
        Add_ExtAPI_Function( funs[i].name
                           , funs[i].strictness
                           , funs[i].non_lazy
                           , funs[i].type
                           , funs[i].fun_ptr
                           );
        insert_fixity(&funs[i]);
    }
    current_funs_registered += num_funs;
    return 0;
}

/* Load the first plugin found with the given name.
   For a plugin named 'foo', the function first attempts to load 'libfoo.so'
   from the current working directory. If no such file is found, the function
   instead tries to find 'VOSSLIB_DIR/libfoo.so'.
   If this file can't be found either, the function fails.

   Returns 1 on success, 0 if the plugin couldn't be loaded.
   On success, 'out' will point to a string containing the name of the plugin.
   On failure, 'out' will instead point to a string containing an error message.
   The output string is guaranteed to persist until the next call to load_plugin.
 */
int
load_plugin(char* plugin_name, char** out)
{
    char* path;
    char filename[PATH_MAX+1];
    static char error[MAX_ERROR_SIZE];
    FILE* fp;

    current_funs_registered = 0;
    current_plugin = NULL;
    current_plugin_metadata_ok = 0;
    current_plugin_types_ok = 1;
    *out = error;

    snprintf(filename, PATH_MAX+1, "lib%s.so", plugin_name);
    fp = Tryopen(filename, "r", &path);
    if(!fp) {
        snprintf(error, MAX_ERROR_SIZE, "Plugin file '%s' does not exist.", filename);
        return 0;
    }
    fclose(fp);

    void *plugin_so = dlopen(path, RTLD_NOW);
    if(!plugin_so) {
        snprintf(error, MAX_ERROR_SIZE, "%s", dlerror());
        return 0;
    }

    void (*fl_init)(fl_plugin_api (*)(fl_plugin)) = dlsym(plugin_so, "fl_init");
    if(!fl_init) {
        snprintf(error, MAX_ERROR_SIZE, "Plugin does not export 'fl_init'. Is this really an fl plugin?");
        return 0;
    }

    current_errlog = new_errlog();
    fl_init(register_plugin);

    if(num_plugin_errors(current_errlog) > 0) {
        int i;
        i = snprintf(error, MAX_ERROR_SIZE, "Unable to load plugin because:\n");
        get_plugin_errors(current_errlog, &error[i], MAX_ERROR_SIZE-i, "  * ");
        return 0;
    }
    free(current_errlog);
    current_errlog = NULL;
    mark_plugin_as_loaded(current_plugin);
    *out = current_plugin->name;
    return 1;
}
