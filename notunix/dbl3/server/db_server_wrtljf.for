      SUBROUTINE DB_SERVER_WRTLJF (IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To save the last journal file number in a file.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IERR   if not 0 then trouble
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*6   OUT_STRING
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INTEGER       IERR
C
      IERR = 0
      OUT_STRING(1:6) = NEXT_JFILE_STR(1:6)
      OPEN (UNIT=LUNLJF,FILE=FILENAME_JF
     +,ACCESS='SEQUENTIAL'
     +,FORM='FORMATTED',STATUS='UNKNOWN',ERR=100)
      WRITE(LUNLJF,9001) OUT_STRING
 9001 FORMAT(A)
      CLOSE (UNIT=LUNLJF)
      RETURN
  100 IERR = 1
      RETURN
      END
