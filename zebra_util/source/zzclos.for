      SUBROUTINE ZZCLOS (LUN,ERROR,OPTION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close a ZEBRA file and perform a FORTRAN
C-                         close.
C-
C-   Inputs  : LUN         Logical Unit Number of input/output stream
C-   Outputs : ERROR        0 -- OK
C-                         -1 --- Error closing file
C-                         -2 --- Invalid option
C-
C-   Controls: OPTION      'READ' or 'INPUT'
C-                         'WRITE' or 'OUTPUT'
C-
C-   Created  21-NOV-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,ERROR
      CHARACTER*1   TYPE
      CHARACTER*(*) OPTION
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      TYPE = OPTION(1:1)
      CALL UPCASE (TYPE,TYPE)
C
      IF ( (TYPE .EQ. 'R') .OR. (TYPE .EQ. 'I') ) THEN
        CALL FZENDI (LUN,'T')
        CLOSE (UNIT=LUN,ERR=900)
        ERROR = IQUEST(1)
      ELSEIF ( (TYPE .EQ. 'W') .OR. (TYPE .EQ. 'O') ) THEN
        CALL FZENDO (LUN,'T')
        CLOSE (UNIT=LUN,ERR=900)
        ERROR = IQUEST(1)
      ELSE
        ERROR =-2
      ENDIF
      GOTO 999
C
  900 CONTINUE
      ERROR =-1
  999 RETURN
      END
