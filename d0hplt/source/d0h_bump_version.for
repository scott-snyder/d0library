      SUBROUTINE D0H_BUMP_VERSION(FILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To check if FILE exists and if it does
C-                         to copy FILE into itself to increase version number
C-                         by 1. This is a kluge to fix a problem with HRPUT
C-                         whicj overwrites existing files.
C-
C-   Inputs  : FILE [C]    filename
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-JUL-1990   Chip Stewart
C-   Updated  26-Feb-1992   Herbert Greenlee
C-      Added machine block to turn this routine into a dummy on UNIX.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INTEGER I,J,K,L,II,JJ,KK,UNIT,IER
C&ELSE
C&ENDIF
      CHARACTER FILE*(*), COMMAND*80
C----------------------------------------------------------------------
C&IF VAXVMS
      CALL SWORDS(FILE,I,J,K)
      COMMAND = 'DIR/COL=1/OUT=DIR.TMP '//FILE(I:J)
      CALL SWORDS(COMMAND,II,JJ,KK)
      CALL LIB$SPAWN(COMMAND(II:JJ))
      CALL GTUNIT(777,UNIT,IER)
      OPEN (UNIT=UNIT, FILE='DIR.TMP',
     &  STATUS='OLD',DISPOSE='DELETE',ERR=990)
      READ (UNIT=UNIT,FMT='(A)',ERR=990) COMMAND
      COMMAND = 'COPY '//FILE(I:J)//' '//FILE(I:J)
      CALL SWORDS(COMMAND,II,JJ,KK)
      CALL LIB$SPAWN(COMMAND(II:JJ))
  990 CLOSE(UNIT)
      CALL RLUNIT(777,UNIT,IER)
      CALL LIB$SPAWN('DELETE/NOCONF DIR.TMP;')
C&ELSE
C&ENDIF
  999 RETURN
      END
