      SUBROUTINE DBOFF_SERVER_READPAR (FILENAME_DAT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created    9-AUG-1991   SHAHRIAR ABACHI
C-   Modified   5-JUL-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILENAME_DAT
      INCLUDE       'D0$INC:DB_SRVR_UNITS.INC'
      INCLUDE       'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE       'D0$INC:DB_SRVR_PARAM.INC'
      LOGICAL FOUND
      INTEGER        IERR,LEN,TRULEN
C
      IERR = 0
      CALL D0OPEN (LUNPAR,FILENAME_DAT,'IF', FOUND)
      IF(.NOT. FOUND) GOTO 100
C
      TOPD = ' '
      READ(LUNPAR,9000) TOPD
      READ(LUNPAR,9000) DTIME
      READ(LUNPAR,*)    ISTOP
      READ(LUNPAR,*)    DISK_UPDATE
9000  FORMAT(A)
      CLOSE (UNIT=LUNPAR)
      RETURN
100   IERR = 1
      RETURN
      END
