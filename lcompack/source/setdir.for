      SUBROUTINE SETDIR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set default directory for reading command files
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-MAY-1989   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER PFNUM
      LOGICAL SETMOD
      CHARACTER*64 NEWDIR
C----------------------------------------------------------------------
      CALL GETPAR(1,' Enter directory specification >','U',NEWDIR)
      IF(.NOT.SETMOD().AND.PFNUM().EQ.0) THEN
        DEFDIR=NEWDIR
      ENDIF
  999 RETURN
      END
