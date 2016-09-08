      SUBROUTINE BLDPDH
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
      INTEGER MPDPDL(5), LOWRUN, HIGRUN, NWFADC, NBFADC
      INTEGER LAY, SEC, ADC, IP, LDPDL
      PARAMETER( NWFADC = 2 )
      PARAMETER( NBFADC = 11)
      REAL    RMEAN, SIGMA
      PARAMETER( RMEAN = 20.)
      PARAMETER( SIGMA = 1.5)
      DATA    MPDPDL / 4HDPDL, 0, 0, 0, 0 /
C----------------------------------------------------------------------
C
      IF (LDPDH .EQ. 0) CALL BKDPDH(LDPDH)
      CALL MZFORM( 'DPDL', '4I -F', MPDPDL(5) )
      MPDPDL(4) = 4 + 32*NBFADC*NWFADC
C
C ****  Book the layer banks
C
      DO 10 LAY = 0, 3
        CALL MZLIFT( IDVSTP, LDPDL, LDPDH, -(LAY+1), MPDPDL, -1)
        IC( LDPDL-5 ) = LAY
        IC( LDPDL+1 ) = LOWRUN
        IC( LDPDL+2 ) = HIGRUN
        IC( LDPDL+3 ) = NWFADC
        IC( LDPDL+4 ) = NBFADC
        DO 20 SEC = 0, 31
          DO 30 ADC = 0, 10
            IP = LDPDL + ( SEC*NBFADC + ADC ) * NWFADC + 4
            C( IP+1 ) = RMEAN
            C( IP+2 ) = SIGMA
   30     CONTINUE
   20   CONTINUE
   10 CONTINUE
  999 RETURN
      END
