      SUBROUTINE BKCDD3(LKCDD3)
C----------------------------------------------------------------------
C
C   Purpose and Methods :  Books the bank "CDD3" containing Flash ADC
C                          raw data for the FDC - Version 0
C
C   Inputs : none
C   Output : LKCDD3 = Bank Link
C 
C   Created   4-JAN-1987  T. Trippe
C            20-APR-1987  G. Rahal       adapted to CDC
C            30-SEP-1988  Jeffrey Bantly adapted to FDC
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC' 
      INCLUDE 'D0$INC:FDCLNK.INC'
      INCLUDE 'D0$LINKS:IZCDD3.LINK/LIST'
      INTEGER MPCDD3(5),LKCDD3,IVERS
      CHARACTER*80 VARMSG
      LOGICAL FIRST
      DATA IVERS / 0 /
      DATA FIRST / .TRUE. /
      DATA MPCDD3 / 0, 0, 0, 7000, 1 /
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL UCTOH( 'CDD3', MPCDD3, 4, 4 )
      ENDIF
      IF ( LHEAD .EQ. 0 ) THEN
        WRITE (VARMSG,*)  '  *** BKCDD3 ***',
     &                    ' Header bank LHEAD not booked'
        CALL ERRMSG('FDC-LHEAD=0','BKCDD3',VARMSG,'F')
        GO TO 999
      ENDIF
C
C ****  Book CDD3
C
      LKCDD3 = LQ ( LHEAD - IZCDD3 )
      IF ( LKCDD3 .EQ. 0 ) THEN
        CALL MZLIFT ( IXMAIN, LCDD3, LHEAD, -IZCDD3, MPCDD3, 3 )
        LKCDD3 = LCDD3
        CALL MVBITS ( IVERS, 0, 5, IQ(LKCDD3), 13 )
        IQ ( LCDD3 + 1 ) = 1
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
