      SUBROUTINE SHPSRC(LSHAPE, TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OBTAIN SRCP SHAPE DATA TO FILL CHSA BANK
C-
C-   Inputs  :     LSHAPE    pointer to CSHA bank
C-                 TITLE     SRCP array title
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Altered:    CSHA
C-
C-   Created   4-FEB-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CSHA.DEF'
      INCLUDE 'D0$INC:SCPARR.INC'
C
      CHARACTER TITLE*(*)
      INTEGER LSHAPE
C
      CALL GTSRCP(TITLE, IVAL, 1)      ! get SRCP array
C
      C(LSHAPE + IGDESC) = RVAL(2)     ! shape name
      IC(LSHAPE + IGNPAR) = IVAL(11)   ! numb params
      CALL UCOPY( RVAL(12), C(LSHAPE+IGPAR1), IVAL(11) )
C----------------------------------------------------------------------
  999 RETURN
      END
