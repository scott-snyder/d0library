      SUBROUTINE ECCLOS(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Close an event catalog.  Clear the dirty
C-     field if the file has been opened for writing.
C-
C-   INPUTS  : ILUN   - LOGICAL UNIT
C-   OUTPUTS :
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR,NR
      INTEGER IECHED,IRPREC,IRECEC,ISTAT
      INTEGER  LIB$DELETE_FILE
      EXTERNAL LIB$DELETE_FILE
C----------------------------------------------------------------------
C
C  Make sure the unit/bank correspondance is correct
C
      CALL ECLSET(ILUN,IERR)
      IF( IERR.NE.0 ) GOTO 991
C
C  Clear dirty flag if necessary...
C
      IF( IQ(LECHD+JRW).NE.0 ) THEN
        CALL ECDIRT(ILUN,0,IERR)
        IF( IERR.NE.0 ) GOTO 992
      ENDIF
C
      CLOSE(ILUN,ERR=998)
C
C  Delete the lock file if this is a write-able file
C
      IF( IQ(LECHD+JRW).NE.0 ) THEN
        CALL UHTOC(IQ(LFLOK+1),4,FNLOCK,LEN(FNLOCK))
        ISTAT=LIB$DELETE_FILE(FNLOCK)
      ENDIF
C
C  Check that no sorted run data remains. This may indicate a corrupted
C  catalog...
C
      IF( LDNEW.GT.0 ) GOTO 993
      IF( LDATA.GT.0 ) GOTO 994
C
 999  CONTINUE
      IERR=0
      RETURN

 991  CONTINUE
      IERR=-1
      RETURN

 992  CONTINUE
      IERR=-2
      RETURN

 993  CONTINUE
      IERR=-3
      RETURN

 994  CONTINUE
      IERR=-4
      RETURN

 998  CONTINUE
      IERR=-8
      RETURN
      END
