      SUBROUTINE BLDGNH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build the pedestal banks
C-
C-   Inputs  : None, but ZEBRA structure up to SCDC should exist.
C-   Outputs :
C-
C-   Created  17-FEB-1988   Olivier Callot
C-   Updated  20-APR-1990   Qizhong Li-Demarteau  modified to handle
C-                                                CALIB data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER MPDGNL(5), LOWRUN, HIGRUN, NWFADC, NBFADC
      INTEGER LAY, SEC, ADC, IP, LDGNL
      PARAMETER( NWFADC = 1 )
      PARAMETER( NBFADC = 11)
      REAL    GAIN
      PARAMETER( GAIN = 1. )
      CHARACTER*4 CHAR4
      EQUIVALENCE (CHAR4,MPDGNL(1))
      DATA    MPDGNL / 0, 0, 0, 0, 0 /
      DATA    CHAR4/'DGNL'/
C----------------------------------------------------------------------
C
      IF (LDGNH .EQ. 0) CALL BKDGNH(LDGNH)
      CALL MZFORM( 'DGNL', '4I -F', MPDGNL(5) )
      MPDGNL(4) = 4 + 32*NBFADC*NWFADC
C
C ****  Book the layer banks
C
      DO 10 LAY = 0, 3
        CALL MZLIFT( IDVSTP, LDGNL, LDGNH, -(LAY+1), MPDGNL, -1)
        IC( LDGNL-5 ) = LAY
        IC( LDGNL+1 ) = LOWRUN
        IC( LDGNL+2 ) = HIGRUN
        IC( LDGNL+3 ) = NWFADC
        IC( LDGNL+4 ) = NBFADC
        DO 20 SEC = 0, 31
          DO 30 ADC = 0, 10
            IP = LDGNL + ( SEC*NBFADC + ADC ) * NWFADC + 4
            C( IP+1 ) = GAIN
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
