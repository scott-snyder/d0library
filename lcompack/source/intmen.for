      SUBROUTINE INTMEN(TOPS,USENAM,DSPNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup interrupt menu and return to main task.
C-
C-   Inputs  : TOPS:   Title for top of display.
C-             USENAM: Name of menu level to use
C-             DSPNAM: Name of dispatch routine to use (EXTERNAL)
C-   Outputs : None
C-   Controls: Interrupt menu will take control of menu part of screen
C-
C-   Created  22-SEP-1988   Jan S. Hoftun
C-
C-      27-Oct-1989 Penelope Constanta-Fanourakis      
C-          Replaced closing of LUN 5 with an assignement
C-	    of INPLUN to be 5
C-
C-   Revised   2-APR-1991   Scott Snyder
C-    smg$enable_unsolicited_input never calls the AST routine if
C-    there is data in the typeahead buffer when it's called.  So
C-    call QIOAST after SETQIO.  Also move the call to MENDIS to before
C-    SETQIO - QIOAST can call MENDIS, and code generated by Digital's
C-    fortrash isn't exactly a paragon of reentrancy...
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TOPS,USENAM
      EXTERNAL DSPNAM
      RETURN
      END
