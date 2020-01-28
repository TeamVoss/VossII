/** Few debug features. These flags should all be turned off in release. */

#ifdef DEBUG
#define DEBUG_TMP
#endif

#ifdef DEBUG_ALL
#define DEBUG_EQUAL
#define DEBUG_FIND
#endif

/* Debug printing for sub-equality matches */
#ifdef DEBUG_EQUAL
#define EQ_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define EQ_DEBUG(...) do {} while(false)
#endif

/* Debug printing for subcircuit finding */
#ifdef DEBUG_FIND
#define FIND_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define FIND_DEBUG(...) do {} while(false)
#endif

/* Debug printing for temporary debug, not meant to last */
#ifdef DEBUG_TMP
#define TMP_DEBUG(...) fprintf(stderr, __VA_ARGS__)
#else
#define TMP_DEBUG(...) do {} while(false)
#endif
