      SUBROUTINE PD3TRK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : display full tracks in CDC for 3 sectors
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER ICL, CENSEC
      REAL    RAY, PHI, DEGRAD, XW1, XW2, YW1, YW2, EXTRAP
      PARAMETER( EXTRAP = 0.0 )
      PARAMETER( DEGRAD = 3.1415926535/180.)
      INTEGER LDRFT
C
      CALL PUGETV('CDC SECTOR', CENSEC )
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
C      CALL PDTRCK( 0, 2, EXTRAP )
      CALL PDTRCK( CENSEC-1, CENSEC+1, EXTRAP )
C
C----------------------------------------------------------------------
  999 RETURN
      END
