      SUBROUTINE DB_SERVER_READLJF (IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads a file to find out last journal file number
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IERR    If not 0 then trouble
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INTEGER        IERR
C
      IERR = 0
      OPEN (UNIT=LUNLJF,FILE=FILENAME_JF
     +,ACCESS='SEQUENTIAL'
     +,FORM='FORMATTED',STATUS='UNKNOWN',ERR=100)
      READ(LUNLJF,*)    LAST_JFILE
      CLOSE (UNIT=LUNLJF)
      RETURN
  100 IERR = 1
      RETURN
      END
