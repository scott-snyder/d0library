#include <string.h>
#include "smg.h"
#include "unix.h"
#define STRLEN 256

long smg$read_string_( DISPLAY **kbid, char *text, char *prompt, 
  long *max_length, long *modifiers, long *time, long *term_set,
  short *text_len, short *term_code, DISPLAY **dpid, char *ini_string,
  long *vattr, long *vattrc, long len_text, long len_prompt, 
  long len_ini_string)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$read_string_
C-
C-   Purpose: Read a string from the specified virtual keyboard.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   usage:
C-
C-      ok = smg$read_string(kbid, text, prompt, max_length, 
C-     &  modifiers, timeout, term_set, text_len, term_code, 
C-     &  dpid, ini_string, vattr, vattrc)
C-
C-   Arguments:
C-
C-     kbid (integer*4, read only)       - Keyboard identifier.
C-     text (character, write only)      - Buffer to receive input.
C-     prompt (character, read only)     - Prompt string.
C-     max_length (integer*4, read only)   - Maximum number of characters
C-                                         to read.
C-     modifiers (integer*4, read only)  - Input options.
C-     timeout (integer*4, ignored)      - Timeout in seconds.
C-     term_set (integer*4, read only)   - String terminators.
C-     text_len (integer*2, write only)  - Actual number of characters read.
C-     term_code (integer*2, write only) - Actual termination character.
C-     dpid (integer*4, read only)       - Display for output (prompt, echo).
C-     ini_string (character, ignored)
C-     vattr (integer*4, read only)      - Output video attributes.
C-     vattrc (integer*4, read only)     - Output video attributes.
C-
C-   Created   22-AUG-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  char cprompt[STRLEN];          /* Null-terminated copy of prompt string */
  char ctext[STRLEN];            /* Local input buffer */
  int max_chars;                 /* Maximum number of characters to read
                                    (not incl. terminator) */
  int term_char;                 /* Actual string terminator */
  int isecho;                    /* Echo mode flag */
  long term_mask;                /* Terminator set mask */
  long prow, pcol;
  int curx, cury, scrolly;       /* Cursor position */

  smg_static_data.function_name = "smg$read_string";

/* Verify keyboard and display. */

  if(*kbid != smg_display_root)
    return smg_error("invalid keyboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Get terminator set mask (default null, lf, cr) */

  if(term_set == NULL)
    term_mask = 1 << '\0' | 1 << '\n' | 1 << '\r';
  else
    term_mask = *term_set;

/* Set video attributes. */

  if(smg_rendition(display, vattr, vattrc) == ERR)
    return 0;

/* Set input options. */

  if(modifiers != NULL) {
    if(*modifiers & TRM$M_TM_PURGE)
      if(flushinp() == ERR)
	return smg_error("error returned by flushinp", 0);
    if(*modifiers & TRM$M_TM_NOECHO) {
      isecho = 0;
    }
    else {
      isecho = 1;
    }
  }

/* In echo mode, make sure that our display is pasted on top. */

  if(isecho) {
    prow = display->prow;
    pcol = display->pcol;
    if(!display->pasted)
      if(!smg$paste_virtual_display_(&display, &smg_display_root,
				     &prow, &pcol,
				     NULL))
	return 0;
    if(display->next != NULL && display->next->pasted)
      if(!smg$repaste_virtual_display_(&display, &smg_display_root,
				       &prow, &pcol,
				       NULL))
	return 0;
  }

/* Make a null-terminated copy of the prompt string and print it. */

  if(prompt != NULL) {
    if(len_prompt >= STRLEN)
      return smg_error("prompt string too long", 0);
    strncpy(cprompt, prompt, len_prompt);
    cprompt[len_prompt] = '\0';
    if(strlen(cprompt) > 0) {
      if(smg_scroll(display) == ERR)            /* Do deferred scrolls */
	return 0;
      smg_bell(cprompt);
      getyx(display->window, cury, curx);

    /* The following block is a kluge to keep the prompt visible in compack. */

      scrolly = cury + display->prow - smg_display_root->nrows + 3;
      if(scrolly > 0) {
        if(wscrl(display->window, scrolly) == ERR)
          return smg_error("error returned by wscrl", 0);
        if(wmove(display->window, cury-scrolly, curx) == ERR)
          return smg_error("error returned by wmove", 0);
      }
      if(waddstr(display->window, cprompt) == ERR)
	return smg_error("error returned by waddstr", 0);
      if(smg_update(display) == 0)
	return 0;
    }
  }

/* Determine the maximum number of characters to read. */

  if(max_length != NULL)
    max_chars = *max_length;
  if(max_chars >= STRLEN)
    max_chars = STRLEN-1;
  if(max_chars > len_text)
    max_chars = len_text;

/* Push the initialization string onto the input queue. */

  if(ini_string != NULL) {
    while(len_ini_string-- > 0) {
      if(ungetch(ini_string[len_ini_string]) == ERR)
	return smg_error("error returned by ungetch", 0);
    }
  }

/* Read the input string */

  term_char = smg_getstr(display->window, ctext, max_chars, isecho, 
			 term_mask);
  if(term_char == ERR)
    return smg_error("error returned by smg_getstr", 0);

/* Echo newline terminator. */

  if(isecho && (term_char == '\r' || term_char == '\n' || term_char == '\0'))
    pechochar(display->window, '\n');

/* Copy stuff back to calling program */

  if(text_len != NULL)
    *text_len = strlen(ctext);
  if(text != NULL) {
    if(len_text < strlen(ctext))
      ctext[len_text] = '\0';
    fstring(ctext, text, len_text);
  }
  if(term_code != NULL)
    *term_code = term_char;
  return 1;
}
