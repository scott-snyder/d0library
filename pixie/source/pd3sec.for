      SUBROUTINE PD3SEC
C======================================================================
C
C  Description:  Displays the Central Drift Chambers for the Stonybrook
C  ============  cosmic test.
C
C
C-   Created   1-FEB-1988   Olivier Callot
C
C======================================================================
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER ICL, CENSEC
      REAL    RAY, PHI, DEGRAD, XW1, XW2, YW1, YW2
      PARAMETER( DEGRAD = 3.1415926535/180.)
      INTEGER LDRFT, IER,TYP
      CHARACTER*20 REM
      CHARACTER*4 CVAL
      LOGICAL EZERROR
C
      CALL EZPICK('PX_CDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFTHET','Cannot find PX_CDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('CDC SECTOR', CENSEC )
C
      LDRFT = LC( LDGEH - 3 )
      RAY = .5* ( C(LDRFT+13) + C(LDRFT+15) ) ! r between layers 1 and 2
      PHI = .5* ( C(LDRFT+14) + C(LDRFT+16) ) + 2*CENSEC*C(LDRFT+9)
      PHI = PHI * DEGRAD
      CALL J4RGET(1, XW1, XW2, YW1, YW2 )
      XW1 = XW1 + RAY * COS(PHI)
      YW1 = YW1 + RAY * SIN(PHI)
      XW2 = XW2 + RAY * COS(PHI)
      YW2 = YW2 + RAY * SIN(PHI)
      CALL JWINDO( XW1, XW2, YW1, YW2 )
      CALL PDSIDE( CENSEC-1, CENSEC+1 )
C
C ****  Reset
C
      CALL EZRSET
  999 RETURN
      END
