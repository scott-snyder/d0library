#include "smg.h"
#define STRLEN 256

long smg$read_keystroke_( DISPLAY **kbid, short *term_code, char *prompt, 
  long *time, DISPLAY **dpid, long *vattr, long *vattrc, long len_prompt)

/*
C----------------------------------------------------------------------
C-
C-   Name: smg$read_keystroke_
C-
C-   Purpose: Read a keystroke from the specified virtual keyboard.
C-
C-   Returned value: 0 (false) - Operation failed.
C-                   1 (true)  - Operation succeeded.
C-
C-   usage:
C-
C-      ok = smg$read_keystroke(kbid, term_code, prompt, timeout, 
C-     &  dpid, vattr, vattrc)
C-
C-   Arguments:
C-
C-     kbid (integer*4, read only)       - Keyboard identifier.
C-     term_code (integer*2, write only) - Key code.
C-     prompt (character, read only)     - Prompt string.
C-     timeout (integer*4, ignored)      - Timeout in seconds.
C-     dpid (integer*4, read only)       - Display for output (prompt, echo).
C-     vattr (integer*4, read only)      - Output video attributes.
C-     vattrc (integer*4, read only)     - Output video attributes.
C-
C-   Created   30-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
*/

{
  DISPLAY *display;
  char cprompt[STRLEN];          /* Null-terminated copy of prompt string */
  int term_char;                 /* Actual string terminator */

  smg_static_data.function_name = "smg$read_keystroke";

/* Verify keyboard and display. */

  if(*kbid != smg_display_root)
    return smg_error("invalid keyboard id", 0);
  display = smg_verify_display(*dpid);
  if(display == NULL)
    return smg_error("invalid display id", 0);

/* Set video attributes. */

  if(smg_rendition(display, vattr, vattrc) == ERR)
    return 0;

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
      if(waddstr(display->window, cprompt) == ERR)
	return smg_error("error returned by waddstr", 0);
      if(smg_update(display) == 0)
	return 0;
    }
  }

/* Read the input character */

  term_char = smg_getch(display->window, 0);
  if(term_char == ERR)
    return smg_error("error returned by smg_getch", 0);

/* Copy stuff back to calling program */

  if(term_code != NULL)
    *term_code = term_char;
  return 1;
}
