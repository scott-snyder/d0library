      SUBROUTINE SETDMP(IUNI,FILNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to dump lines to a file
C-
C-   Inputs  : IUNI: Unit number to use, 6 means get it on screen after
C-                   the dump has ended.
C-             FILNAM: Name of file to dump to.
C-   Outputs : None
C-
C-   Created  29-JUN-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IUNI
      CHARACTER*(*) FILNAM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      LOGICAL OK
C----------------------------------------------------------------------
      IF(IUNI.EQ.6) THEN
         CALL OUTSAV
      ELSE
         CALL D0OPEN(IUNI, FILNAM, 'OFL', OK)
      ENDIF
  999 RETURN
      END
