
      SUBROUTINE BKCDD2
C======================================================================
C
C   Purpose and Methods :  Books the bank "CDD2" containing Flash ADC
C                          raw data for the CDC
C
C-  Inputs : None
C-  Output :
C-
C    Created   4-JAN-1987  T. Trippe
C             20-APR-1987  G. Rahal   adapted to CDC
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau   use ERRMSG and put in
C-                                                 version number
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZCDD2.LINK/LIST'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INTEGER MPCDD2(5), ISETVN
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA MPCDD2 / 0, 0, 0, 7000, 1 /
C
C======================================================================
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'CDD2', MPCDD2, 4, 4 )
      ENDIF
      IF ( LHEAD .EQ. 0 ) THEN
        CALL ERRMSG('DTRAKS','BKCDD2',
     &      ' Header bank LHEAD not booked ','W')
        GO TO 999
      ENDIF
C
C ****  Book CDD2
C
      LCDD2 = LQ ( LHEAD - IZCDD2 )
      IF ( LCDD2 .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDD2, LHEAD, -IZCDD2, MPCDD2, 3 )
      ENDIF
C
      IQ(LCDD2) = ISETVN(IQ(LCDD2),0)
  999 CONTINUE
      RETURN
      END
