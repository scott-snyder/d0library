      SUBROUTINE EZZCLK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a link area for SRCP routines.
C-
C-   Inputs  : None
C-
C-   Outputs : NONE
C-
C-   Controls: None
C-
C-   Created   4-OCT-1988   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Declare a permanent link area to ZEBRA for SRCP banks
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ERRSRC= EZS_SUCCESS             ! Clear error flag
        ISRCP = 0                       ! Pointer into link area
        NSRCP = 0                       ! Number of SRCP banks created
        ISTACK= 0                       ! Stack pointer
        CALL MZLINK (IXSTP,'/LKSRCP/',KSRCP(1),KSRCP(MXSRCP),KSRCP(1))
      ENDIF
C
  999 RETURN
      END
