      SUBROUTINE EXIMEN(ISTAT,ILEV)
C-   Purpose and Methods : Exit from a MAINPACK program
C-   Inputs  : ISTAT: Status to be reported at exit
C-             ILEV:  Level of menu exiting from (not used).
      INTEGER ISTAT,ILEV
      CALL OUTMSG(' Exit from:')
      CALL OUTMSG(ISTAT)
      CALL OUTMSG(' ')
C  THE STOP CLEARS ALL VIEWPORTS
      STOP
      END
