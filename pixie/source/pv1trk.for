      SUBROUTINE PV1TRK(LVTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one track VTXT
C-
C-   Inputs  : LVTXT: bank address for the track to be drawn
C-   Outputs : none
C-
C-   Created  31-JUL-1990   Qizhong Li-Demarteau  modifed from PVTRAK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVTXT.LINK'
C
      INTEGER GZVTRH
      INTEGER LVRFT, LVTXT, LVTRH
      REAL A, B, CE, R, RMIN, RMAX, DEL, SS, PHI, XG, YG
      REAL XHPOS, YHPOS
      INTEGER IWFIRS, IWLAST, ILFIRS, ILLAST, IXV(32), NXV
      LOGICAL IDEBUG
      DATA IDEBUG/.FALSE./
C----------------------------------------------------------------------
      LVRFT = LC( LVGEH - 3 )
      IF ( LVTXT .LE. 0 ) GO TO 999
      CALL PUOPEN
      CALL PXCOLR('FOR')
C
C ****  Get the first and last wire fired
C
      CALL UBITS ( IQ(LVTXT+3), 32, IXV, NXV)
      IWFIRS = IXV(1) - 1
      IWLAST = IXV(NXV) - 1
      ILFIRS = IWFIRS/8
      ILLAST = IWLAST/8
      IWFIRS = MOD ( IWFIRS, 8)
      IWLAST = MOD ( IWLAST, 8)
      R      = SQRT(C(LVRFT+23+IWFIRS)**2 + C(LVRFT+31+IWFIRS)**2)
      IF ( IWFIRS .LE. 3 ) R = - R
      RMIN = C(LVRFT+7+ILFIRS*7) + R
      R    = SQRT(C(LVRFT+23+IWLAST)**2 + C(LVRFT+31+IWLAST)**2)
      IF ( IWLAST .LE. 3 ) R = - R
      RMAX = C(LVRFT+7+ILLAST*7) + R
C
C
      PHI   = Q(LVTXT+6)
      XG    = Q(LVTXT+7)
      YG    = Q(LVTXT+8)
      A = 1.
      R = RMIN
      B = XG*COS(PHI) +YG*SIN(PHI)
      CE = XG**2 + YG**2 - R**2
      DEL = B*B - A*CE
      IF ( DEL .LT. 0. ) GO TO 92
      SS = -B + SQRT(DEL)/A
      XHPOS = XG + SS*COS(PHI)
      YHPOS = YG + SS*SIN(PHI)
      CALL JMOVE( XHPOS, YHPOS )
      R = RMAX
      CE = XG**2 + YG**2 - R**2
      DEL = B*B - A*CE
      IF ( DEL .LT. 0. ) GO TO 92
      SS = -B + SQRT(DEL)/A
      XHPOS = XG + SS*COS(PHI)
      YHPOS = YG + SS*SIN(PHI)
      CALL JDRAW( XHPOS, YHPOS )
   92 CONTINUE
      CALL JRCLOS
  999 RETURN
      END

