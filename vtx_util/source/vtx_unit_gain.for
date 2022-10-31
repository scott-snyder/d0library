      SUBROUTINE VTX_UNIT_GAIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILL VGNL BANKS WITH 1.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-SEP-1992   M. Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NWFADC,NBFADC
      INTEGER MXLAY,IER,MXWIRE,NSEC(3)
      INTEGER LAYER,SECTOR,ICATG
      INTEGER GZVGNL,LVGNL,IDRIFT,N,ADC,IP
      PARAMETER( NWFADC = 1)
      PARAMETER( NBFADC = 16)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET( 'MXLAY', MXLAY, IER )
        CALL EZGET( 'MXWIRE', MXWIRE, IER )
        CALL EZGET_iarr( 'NSEC', NSEC, IER )
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      DO LAYER = 0,MXLAY
        LVGNL = GZVGNL(LAYER)
        N = 0
        DO ICATG = 0,2
          DO IDRIFT = -20,20
            N = N + 1
            C(LVGNL+5+N) = 1. ! Area vs drift correction
          END DO
        END DO
        DO SECTOR = 0,NSEC(LAYER+1)
          DO 30 ADC = 0, NBFADC-1
            IP = LVGNL + ( SECTOR*NBFADC + ADC ) * NWFADC + 5 + 3*41
            C( IP+1 ) = 1.
   30     CONTINUE
        END DO
      END DO
C
  999 RETURN
      END
