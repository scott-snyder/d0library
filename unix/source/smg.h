/*
C----------------------------------------------------------------------
C-
C-   Name: smg.h
C-
C-   Purpose: smg header file
C-
C-   Created  20-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

#define SIUNIX 1
#define IBMAIX 2
#define ULTRIX 3
#define HPUX   4
#define SUNOS  5
#define LINUX  6

#if D0FLAVOR == ULTRIX
#include <cursesX.h>
#else
#if D0FLAVOR == LINUX
#include <ncurses/curses.h>
#else
#include <curses.h>
#endif
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#endif
#endif 

#include "smgdef.h"
#include "trmdef.h"

#ifndef NULL
#define NULL    0
#endif

/* Here we define macros on IBM for missing routines and macros. */

#if D0FLAVOR == IBMAIX

#define getmaxyx(win,y,x)	((y) = getmaxy(win), (x) = getmaxx(win))
#define getmaxy(win)		((win)->_maxy)
#define getmaxx(win)		((win)->_maxx)

#define	getbegyx(win,y,x)	((y) = getbegy(win), (x) = getbegx(win))
#define	getbegy(win)		((win)->_begy)
#define	getbegx(win)		((win)->_begx)

#define curs_set(flag)          (OK)
#define isendwin()              (0)

#endif /* IBMAIX */

/* Parameters */

#define SMG$_PASALREXI 0x00128031

/* Data structures */

typedef struct smg_display_data {  /* Display data structure */
  WINDOW *window;                  /* Text window pointer */
  WINDOW *brdr;                    /* Border window */
  char *title;                     /* Title string */
  int nrows;                       /* Number of rows in text window */
  int ncols;                       /* Number of columns in text window */
  int vrows;                       /* Number of rows in viewport */
  int vcols;                       /* Number of columns in viewport */
  int row_vport;                   /* Origin of viewport wrt text window */
  int col_vport;                   /* Origin of viewport wrt text window */
  long dattr;                      /* Display attributes (bordered?) */
  long vattr;                      /* Video attributes */
  int pasted;                      /* Pasted? (0=no, 1=yes) */
  int prow;                        /* Pasteboard row of text window */
  int pcol;                        /* Pasteboard column of text window */
  int num;                         /* Number in chain (for debugging) */
  int nscroll;                     /* Number of deferred scolls */
  struct smg_display_data *next;   /* Pointer to next display (NULL if last) */
  struct smg_display_data *prev;   /* Pointer to previous display (NULL if 
                                      first) */
} DISPLAY;

extern DISPLAY *smg_display_root;   /* First link of display chain (points to
                                      window stdscr).  Initialized to NULL in
                                      smg$create_pasteboard_. */

typedef struct {
  char *function_name;              /* Pointer to name of currently active
                                       routine. */
  FILE *cin, *cout;                 /* Pointers to input and output streams. */
  int batch;                        /* Batch update mode */
  int async;                        /* Asynchronous mode i/o mode. */
  int child;                        /* Child process (0 = none). */
} SMG_DATA;

extern SMG_DATA smg_static_data;

/* Function prototypes. */

long smg$begin_pasteboard_update_( DISPLAY **pbid);
long smg$end_pasteboard_update_( DISPLAY **pbid);
long smg$change_viewport_( DISPLAY **dpid, long *row_vport, long *col_vport,
  long *vrows, long *vcols);
long smg$change_virtual_display_(DISPLAY **dpid, long *nrows, long *ncols, 
  long *dattr, long *vattr, long *char_set);
long smg$copy_virtual_display_( DISPLAY **dpid, DISPLAY **dpid2);
long smg$create_pasteboard_( DISPLAY **pbid, char *outdev, long *nrows, 
  long *ncols, long *flags, long outdev_len);
long smg$create_viewport_(DISPLAY **dpid, long *row_start, long *col_start,
  long *nrows, long *ncols);
long smg$create_virtual_display_( long *nrows, long *ncols, DISPLAY **dpid, 
  long *dattr, long *vattr, long *char_set);
long smg$create_virtual_keyboard_( DISPLAY **kbid);
long smg$cursor_column_( DISPLAY **dpid);
long smg$cursor_row_( DISPLAY **dpid);
long smg$delete_pasteboard_( DISPLAY **pbid, long *flags);
long smg$delete_viewport_( DISPLAY **dpid);
long smg$delete_virtual_display_( DISPLAY **dpid);
long smg$delete_virtual_keyboard_( DISPLAY **kbid);
long smg$disable_unsolicited_input_( DISPLAY **pbid);
long smg$enable_unsolicited_input_( DISPLAY **pbid,
  void (*ast_routine)(DISPLAY**, void*), void *ast_argument);
long smg$erase_display_( DISPLAY **dpid, long *start_row, long *start_col,
  long *end_row, long *end_col);
long smg$erase_line_( DISPLAY **dpid, long *row, long *col);
long smg$erase_pasteboard_( DISPLAY **pbid);
long smg$get_display_attr_( DISPLAY **dpid, long *height, long *width, 
  long *dattr, long *vattr, long *char_set, long *flags);
long smg$get_pasting_info_( DISPLAY **dpid, DISPLAY **pbid, long *pflag,
  long *prow, long *pcol);
long smg$get_viewport_char_( DISPLAY **dpid, long *row_vport, long *col_vport,
  long *vrows, long *vcols);
long smg$move_text_( DISPLAY **dpid, long *ulrow, long *ulcol,
  long *brrow, long *brcol, DISPLAY **dpid2, long *ulrow2, long *ulcol2,
  long *flags);
long smg$move_virtual_display_( DISPLAY **dpid, DISPLAY **pbid, long *prow, 
  long *pcol, DISPLAY **top_dbid);
long smg_paste_display( DISPLAY **dpid, DISPLAY **pbid, long *prow,
  long *pcol, DISPLAY **top_dpid);
long smg$paste_virtual_display_( DISPLAY **dpid, DISPLAY **pbid, long *prow, 
  long *pcol, DISPLAY **top_dbid);
long smg$put_chars_( DISPLAY **dpid, char *text, long *row, long *col, 
  long *flags, long *vattr, long *vattrc, long *char_set, long len_text);
long smg$put_chars_wide_( DISPLAY **dpid, char *text, long *row, long *col, 
  long *vattr, long *vattrc, long *char_set, long len_text);
long smg$put_line_( DISPLAY **dpid, char *text, long *line_adv, long *vattr,
  long *vattrc, long *flags, long *char_set, long *direction, long len_text);
long smg$put_with_scroll_( DISPLAY **dpid, char *text, long *direction,
  long *vattr, long *vattrc, long *flags, long *char_set, long len_text);
long smg$read_from_display_( DISPLAY **dpid, char *text, char *term_string, 
  long *row, long len_text, long len_term_string);
long smg$read_keystroke_( DISPLAY **kbid, short *term_code, char *prompt, 
  long *time, DISPLAY **dpid, long *vattr, long *vattrc, long len_prompt);
long smg$read_string_( DISPLAY **kbid, char *text, char *prompt, 
  long *max_length, long *modifiers, long *time, long *term_set,
  short *text_len, short *term_code, DISPLAY **dpid, char *ini_string,
  long *vattr, long *vattrc, long len_text, long len_prompt, 
  long len_ini_string);
long smg$repaint_screen_( DISPLAY **pbid);
long smg$repaste_virtual_display_( DISPLAY **dpid, DISPLAY **pbid, long *prow, 
  long *pcol, DISPLAY **top_dbid);
long smg$return_cursor_pos_( DISPLAY **dpid, long *row, long *col);
long smg$set_cursor_abs_( DISPLAY **dpid, long *row, long *col);
long smg$set_cursor_mode_( DISPLAY **pbid, long *mode);
long smg$set_display_scroll_region_(DISPLAY **dpid, long *start, long *end);
long smg$unpaste_virtual_display_( DISPLAY **dpid, DISPLAY **pbid);


/* Private functions. */

void smg_bell(char *string);
long smg_error(char *message, long retcode);
int smg_getch(WINDOW *window, int isecho);
int smg_getstr(WINDOW *window, char *string, int max_chars, int isecho, 
	       long term_mask);
int smg_reset_term_();
int smg_scroll(DISPLAY *display);
int smg_rendition(DISPLAY *display, long *vattr, long *vattrc);
long smg_update(DISPLAY *display);
DISPLAY *smg_verify_display(DISPLAY *display);
