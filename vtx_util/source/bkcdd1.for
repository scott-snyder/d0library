      SUBROUTINE BKCDD1
C======================================================================
C
C   Purpose and Methods :  Books the bank "CDD1" containing Flash ADC
C                          raw data for the CDC
C
C-  Inputs : None
C-  Output :
C-
C   Created   4-JAN-1987  T. Trippe
C            20-APR-1987  G. Rahal   adapted to CDC
C             6-OCT-1988  T. Trippe  readapted to VTX
C            27-JUN-1989  P. Grudberg - removed GCUNIT
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD1.LINK/LIST'
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'
      INTEGER MPCDD1(5)
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA MPCDD1 / 0, 0, 0, 7000, 1 /
C
C======================================================================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'CDD1', MPCDD1, 4, 4 )
      ENDIF
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG(' Error in booking CDD1','BKCDD1',
     &    ' Header bank HEAD not booked','W')
        GO TO 999
      ENDIF
C
C ****  Book CDD1
C
      LCDD1 = LQ ( LHEAD - IZCDD1 )
      IF ( LCDD1 .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDD1, LHEAD, -IZCDD1, MPCDD1, 3 )
      ENDIF
C
C
  999 CONTINUE
      RETURN
      END
