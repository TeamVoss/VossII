#ifndef __PLUGIN_ERRLOG_H
#define __PLUGIN_ERRLOG_H
#include <stdio.h>

typedef struct plugin_errlog_node_rec {
  char *error;
  struct plugin_errlog_node_rec* next;
  struct plugin_errlog_node_rec* prev;
} plugin_errlog_node_rec;
typedef plugin_errlog_node_rec* plugin_errlog_node;

typedef struct plugin_errlog_rec {
  int num_errors;
  plugin_errlog_node first;
  plugin_errlog_node last;
} plugin_errlog_rec;
typedef plugin_errlog_rec* plugin_errlog;

/* Copies the given error into the list of plugin errors. */
void log_plugin_error(plugin_errlog log, const char* fmt, ...);

/* Get the current number of reported plugin errors. */
int num_plugin_errors(plugin_errlog log);

/* Write all plugin errors reported so far to the given char pointer. */
int get_plugin_errors(plugin_errlog log, char* out, int max_size, const char* prefix);

/* Creates a new error log. */
plugin_errlog new_errlog();

/* Frees a previously created error log. */
void free_errlog(plugin_errlog log);
#endif /* __PLUGIN_ERRLOG_H */
