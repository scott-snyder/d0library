#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#endif
#define STRLEN 256

#define SIUNIX 1
#define IBMAIX 2
#define ULTRIX 3
#define HPUX   4
#define SUNOS  5

#ifndef NULL
#define NULL   ((void*)0)
#endif

/* Function declarations for find_file.c */

char *find_file(char *ifile, char *rfile, char **context);
long find_file_(char *ifile, char *rfile, char **context, 
  long len_ifile, long len_rfile);
void find_file_end(char **context);
long find_file_end_(char **context);

/* Function declarations for lib_find_file.c */

char *lib_find_file(char *ifile, char *rfile, char **context);
long lib$find_file_(char *ifile, char *rfile, char **context, 
  long len_ifile, long len_rfile);
long lib$find_file_end_(char **context);
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
char *getpassnetrc(char *host, char *user);

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
