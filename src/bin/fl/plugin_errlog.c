#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "plugin_errlog.h"
#define MAX_ERROR_LEN 2048

void
log_plugin_error(plugin_errlog log, const char* fmt, ...)
{
    plugin_errlog_node next = malloc(sizeof(plugin_errlog_node_rec));
    va_list args;

    if(log->num_errors > 0) {
      log->last->next = next;
    } else {
      log->first = next;
    }

    next->error = malloc(MAX_ERROR_LEN);
    va_start(args, fmt);
    vsnprintf(next->error, MAX_ERROR_LEN, fmt, args);
    va_end(args);

    next->next = NULL;
    next->prev = log->last;
    log->last = next;
    log->num_errors++;
}

int
num_plugin_errors(plugin_errlog log)
{
    return log->num_errors;
}

int
get_plugin_errors(plugin_errlog log, char* out, int max_size, const char* prefix)
{
    int i = 0;
    for(plugin_errlog_node p = log->first; p != NULL; p = p->next) {
        i += snprintf(&out[i], max_size-i, "%s%s\n", prefix, p->error);
    }
    return i;
}

plugin_errlog
new_errlog()
{
    plugin_errlog log = malloc(sizeof(plugin_errlog_rec));
    log->first = NULL;
    log->last = NULL;
    log->num_errors = 0;
    return log;
}

void
free_errlog(plugin_errlog log)
{
    for(plugin_errlog_node p = log->first; p != NULL; p = p->next) {
        free(p->error);
        if(p->prev) {
            free(p->prev);
        }
    }
    free(log->last);
    free(log);
}
