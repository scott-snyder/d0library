      SUBROUTINE D0DAD_CHECK(D0DAD_NAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Steering routine for consistancy checks.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-Sep-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) D0DAD_NAME
      INTEGER IERR
      CHARACTER*128 FILENAME,EXT
      INTEGER FLEN,ELEN
C-----------------------------------------------------------------------
C
      IERR=0
      CALL FILENAME_PARSE(D0DAD_NAME,'DEV+DIR+NAM',FILENAME,FLEN)
      CALL FILENAME_PARSE(D0DAD_NAME,'EXT',EXT,ELEN)
      CALL CLTOU(EXT)
C
      IF( EXT.EQ.'.D0DADF' ) THEN
        CALL D0DAD_DFCHECK(D0DAD_NAME,IERR)
        IF( IERR.NE.0 ) IERR = -2
      ELSEIF(ELEN.EQ.0 .OR. EXT.EQ.'.EVTCAT' .OR. EXT.EQ.'.FILECAT')THEN
        CALL D0DAD_CATCHECK(FILENAME,IERR)
        IF( IERR.NE.0 ) IERR = -3
      ELSE
        IERR = -1
      ENDIF
C
  999 RETURN
      END
