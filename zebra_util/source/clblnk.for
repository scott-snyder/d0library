      SUBROUTINE CLBLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a link area for CALIB routines.
C-
C-   Inputs  : None
C-
C-   Outputs : NONE
C-
C-   Controls: None
C-
C-   Created   1-DEC-1980   Jan Guida  (Taken from EZZCLK.)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Declare a permanent link area to ZEBRA for CALIB banks
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
c        ICALIB = 0
c        NCALIB = 0       ! Number of CALIB banks created
        CALL MZLINK (IXSTP,'/LKCALIB/',CALIB_LNK(1),CALIB_LNK(MXCALIB),
     &    CALIB_LNK(1))
      ENDIF
C
  999 RETURN
      END
