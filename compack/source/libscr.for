      INTEGER FUNCTION LIBSCR(LINE1,LINE2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set limited scroll region.
C-                         VAX-specific (Uses SMG)
C-
C-   Inputs  : LINE1: First line of scroll region
C-             LINE2: Last line of scroll region
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Modified    4-FEB-1991   Scott Snyder
C-    Added entry LIBSCR_AGAIN to set the scrolling region using the
C-    same parameters as the last call to LIBSCR. Used to kluge around
C-    the problem that we can't get scrolling region information from
C-    SMG in COPY_MAINID, which we of course need to kluge around a SMG
C-    `idiosyncrasy'...
C-   Updated  19-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LINE1,LINE2
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$SET_DISPLAY_SCROLL_REGION
C
      INTEGER LIBSCR_AGAIN
      INTEGER LINE1_SAVE, LINE2_SAVE
      SAVE    LINE1_SAVE, LINE2_SAVE
C----------------------------------------------------------------------
      LIBSCR=SMG$SET_DISPLAY_SCROLL_REGION(MAINID,LINE1,LINE2)
      LINE1_SAVE = LINE1
      LINE2_SAVE = LINE2
      RETURN
C
      ENTRY LIBSCR_AGAIN
      LIBSCR_AGAIN=SMG$SET_DISPLAY_SCROLL_REGION(MAINID,
     &                                           LINE1_SAVE, LINE2_SAVE)
      RETURN
      END
