#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#endif
#define STRLEN 256
 
#define SIUNIX 1
#define IBMAIX 2
#define ULTRIX 3
#define HPUX   4
#define SUNOS  5
#define LINUX  6
 
#ifndef NULL
#define NULL   ((void*)0)
#endif

#include <time.h>
#include "glob.h"

/* Find_file typedef for context structure linked list. */

typedef struct context_struct {
  int gl_next;       /* Next element of glob_t structure to return. */
  char *pattern;     /* Pattern string. */
  glob_t *pglob;     /* Pointer to glob structure (used by glob()). */
  struct context_struct *next;   /* Pointer to next context element. */
} context_t;
 
/* Function declarations for find_file.c */
 
char *find_file(char *ifile, char *rfile, context_t **context);
long find_file_(char *ifile, char *rfile, context_t **context,
  long len_ifile, long len_rfile);
void find_file_end(context_t **context);
long find_file_end_(context_t **context);
 
/* Function declarations for lib_find_file.c */
 
char *lib_find_file(char *ifile, char *rfile, context_t **context);
int lib$find_file_(char *ifile, char *rfile, int *indexp,
  int len_ifile, int len_rfile);
int lib$find_file_end_(int *indexp);
long lib$wait_( float *seconds);
 
/* Non-emulation declarations. */
 
char *cstring(char *fstr, long len_fstr, char *cstr, long len_cstr);
void d0_nodename_(char *node, long nodelen);
long d0_tz_offset_();
char *d0_loc_(char *var);
long utc_tz_offset_();
long daylight_time_offset(long *tim);
char *fstring(char *cstr, char *fstr, long len_fstr);
char *getword(char *word, int n);
long d0_readlink_(char *path, char *buf, long pathlen, long buflen);
char *getusernetrc(char *host);
char *getpassnetrc(char *host, char *user);
void flush_std_();
int stopme_install_();
int segv_install_(int *force_dump);

/* Other VAX emulation function declarations. */
 
void exit_(long *exit_status);
long lib$day_(long *day, long *user_time, long *day_time);
long lib$delete_file_(char *file, long len_file);
long lib$get_vm_(long *nbytes, long *baseadd);
long lib$rename_file_(char *from, char *to, long len_from, long len_to);
long sys$asctim_(short *timlen, char *timbuf, long *tim, long len_timbuf1,
  long len_timbuf2);
long sys$bintim_(char *timbuf, long *tim, long len_timbuf);
long sys$gettim_(long *time);
long sys$numtim_(short *timbuf, long *tim);

char *fstring(char *cstr, char *fstr, long len_fstr);
char *cstring(char *fstr, long len_fstr, char *cstr, long len_cstr);
void unix_to_vms_time_ (time_t*, long*);
void vms_to_unix_time_ (long*, time_t*);
int fatmen_find(char *file, int nout, char *outbuf);

