      SUBROUTINE VTQDIV(LAYER,SECTOR,WIRE,PEAK0,PEAK1,ZCOORD,ZERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute z-coordinate from PEAK HEIGHTS
C-
C-   Inputs  : LAYER,SECTOR,WIRE -- usual
C-             PEAK0,PEAK1 -- linarized, ped-subtracted peak heights
C-   Outputs : ZCOORD -- z in cm
C-             ZERR   -- z error in cm
C-   Controls:
C-
C-   Created  11-OCT-1993   Ed Oltman (Complete re-write)
C-   Updated   6-DEC-1993   Liang-Ping Chen use RINPUT_MC for MC data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVGNL.LINK'
c I/O:
      INTEGER  LAYER,SECTOR,WIRE
      REAL     PEAK0,PEAK1,ZCOORD,ZERROR
c Locals:
      LOGICAL  FIRST
      REAL     A0,A1
      INTEGER  ERR,LVGNL,LVGNL0,LVGNL1
      INTEGER  IER
      REAL     RINPUT, RPERCM
      REAL     ALPHA,SUM
      REAL     ZER_COEF(4),HLEN(0:2)
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_rarr('ZERR_COEF',ZER_COEF,ERR)
        CALL EZGET('RPERCM',RPERCM,IER)
        IF ( IQ(LHEAD+1) .GT. 1000 ) THEN   ! MC DATA
          CALL EZGET('RINPUT_MC',RINPUT,IER)
        ENDIF
        CALL EZRSET
        HLEN(0) = C(LVGEH+17)
        HLEN(1) = C(LVGEH+19)
        HLEN(2) = C(LVGEH+21)
      ENDIF
      LVGNL = LC(LVGNH - IZVGNL - LAYER)
      LVGNL0 = LVGNL + (SECTOR*IC(LVGNL+4)+2*WIRE  )*IC(LVGNL+3) + 129
      LVGNL1 = LVGNL + (SECTOR*IC(LVGNL+4)+2*WIRE+1)*IC(LVGNL+3) + 129
      A0 = PEAK0/C(LVGNL0)
      A1 = PEAK1/C(LVGNL1)
      ALPHA = (A1 - A0)/(A1 + A0)
      IF ( IQ(LHEAD+1) .GT. 1000 ) THEN     ! MC data
        ZCOORD = HLEN(LAYER)*(1.+RINPUT/RPERCM/HLEN(LAYER))*ALPHA
        ! a fix before VGNL in MC STP are updated for the new VGNL format
      ELSE
        ZCOORD = HLEN(LAYER)*C(LVGNL+7)*ALPHA
      ENDIF
      IF (ABS(ZCOORD).GT.HLEN(LAYER)) ZCOORD = SIGN(HLEN(LAYER),ZCOORD)
      SUM = PEAK0 + PEAK1
C
C ****  FOLLOWING IS A PARAMETRIZATION OF ERRORS FROM INTERNAL RESIDUALS.
C
      IF (SUM .LT. 400.) THEN
        ZERROR = ZER_COEF(1)
     &         + ZER_COEF(2)*SUM
     &         + ZER_COEF(3)*SUM*SUM
        ZERROR = 1./ZERROR
      ELSE
        ZERROR = ZER_COEF(4)
      ENDIF
  999 RETURN
      END
