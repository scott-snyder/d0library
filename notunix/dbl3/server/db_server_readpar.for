      SUBROUTINE DB_SERVER_READPAR (IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To read from parameter file record size
C-                         and wakeup time intervals.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IERR    If not 0 then trouble
C-
C-   Modified   9-AUG-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
      INTEGER        IERR
C
      IERR = 0
      OPEN (UNIT=LUNPAR,FILE=FILENAME_PAR
     +,ACCESS='SEQUENTIAL'
     +,FORM='FORMATTED',STATUS='UNKNOWN',ERR=100)
      READ(LUNPAR,*)    IRECL
      READ(LUNPAR,9000) DTIME
      READ(LUNPAR,*) DISK_UPDATE
      READ(LUNPAR,*) ISTOP
 9000 FORMAT(A)
      CLOSE (UNIT=LUNPAR)
      RETURN
  100 IERR = 1
      RETURN
      END
