      SUBROUTINE CGEH
C-----------------------------------------------------------------------
C
C     This subroutine creates and fills the calorimeter geometry header
C     bank CGEH the standard D0 geometry.
C     banks lifted:      CGEH, CSHA
C
C     Author:       S Kahn       31 Oct 1986
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:CGHEAD.DEF'
      INCLUDE 'D0$LINKS:IZCGEH.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INTEGER MCGEH(5), IOCGEH
C
      CHARACTER*4 CHAR4
      EQUIVALENCE (CHAR4,MCGEH(1))
      DATA MCGEH / 0, 5, 4, 9, 9/
      DATA CHAR4 /'CGEH'/
      CALL MZFORM('CGEH','8I1F',IOCGEH)
C
      CALL MZLIFT(IDVSTP,LCGEH,LSCAL,-IZCGEH,MCGEH,0)   ! lift CGEH
C
      CALL UCOPY(C(LSCAL+1),C(LCGEH+1),MCGEH(4))!copy SCAL to CGEH
C
      RETURN
      END
      

