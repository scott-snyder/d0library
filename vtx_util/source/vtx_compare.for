      SUBROUTINE VTX_COMPARE(LTRAK,SAVE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-   Inputs:  LTRAK -- Link to CDC or FDC
C-   Outputs :SAVE -- LINK TO VTXT IF FOUND, OTHERWISE 0
C-   Controls:
C-
C-   Created   6-OCT-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LTRAK,SAVE
c Locals:
      INTEGER ERR,LVTXT,N1,N2
      REAL    MAX_PHI,ISO_PHI,DEL,MIN,DIST,ENDXY,PHI_SAVE
      REAL    X,Y,PHI,XEX,YEX,PHIEX
      CHARACTER*4 BANK
c Externals:
      INTEGER GZVTXT
      REAL    ENDSEG1
c Data:
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('MAX_PHI',MAX_PHI,ERR)
        CALL EZGET('ISO_PHI',ISO_PHI,ERR)
        CALL EZGET('ENDXY',ENDXY,ERR)
        CALL EZRSET
      ENDIF
      N1 = 0
      N2 = 0
      SAVE = 0
      LVTXT = GZVTXT(0)
      IF (LVTXT .EQ. 0) GO TO 999
      CALL UHTOC(IQ(LTRAK-4),4,BANK,4)
      IF (BANK .EQ. 'DTRK') THEN
        XEX = Q(LTRAK+7)
        YEX = Q(LTRAK+8)
        PHIEX = Q(LTRAK+6)
      ELSE
        XEX = Q(LTRAK+4)
        YEX = Q(LTRAK+5)
        PHIEX = Q(LTRAK+6)
      ENDIF
C
C ****  Find VTXT with smallest ENDSEG w/resp to TRAK
C
      MIN = 99999.
      DO WHILE (LVTXT .GT. 0)
        X   = Q(LVTXT+7)
        Y   = Q(LVTXT+8)
        PHI = Q(LVTXT+6)
        DIST = ENDSEG1(XEX,YEX,PHIEX,X,Y,PHI)
        IF (DIST .LT. MIN) THEN
          MIN = DIST
          SAVE = LVTXT
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO
C
C ****  Apply cut to ENDSEG, delta_phi
C
      IF (MIN .GT. ENDXY) GO TO 900
      PHI_SAVE = Q(SAVE+6)
      DEL = ABS(PHI_SAVE-PHIEX)
      IF (DEL .GT. 3.) DEL = ABS(DEL-TWOPI)
      IF (DEL .GT. MAX_PHI) GO TO 900
C
C ****  Demand that the VTXT is isolated in (delta_phi)
C
      LVTXT = GZVTXT(0)
      DO WHILE (LVTXT .GT. 0)
        IF (LVTXT .NE. SAVE) THEN 
          DEL = ABS(PHI-PHI_SAVE)
          IF (DEL .GT. 3.) DEL = ABS(DEL-TWOPI)
          IF (DEL .LT. ISO_PHI) GO TO 900
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO
      GO TO 999
  900 SAVE = 0
  999 RETURN
      END
