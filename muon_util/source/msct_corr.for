      SUBROUTINE MSCT_CORR(IMOD,NCELL,TXYZ,XYZ,TSHIFT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Make correction for time variations along
C-                          length and width of scintillator counter.
C-
C-   Inputs  : IMOD             [I] : Module
C-             NCELL            [I] : Cell number
C-             TXYZ(3)          [R] : Track coords. at scintillator
C-             XYZ(3)           [R] : Scintillator center coor.
C-   Outputs : TSHIFT           [R] : Time shift (nsec)
C-   Controls:
C-
C-   Explanation :  A correction is made dependent on the position 
C-        along the length of the fiber (YS) which attempts to fix 
C-        incorrect constant for light propagation time, extra fiber  
C-        and non-linearity for hits near the center of counter.  Also 
C-        a correction is made for position along the width of counter
C-        (ZS) to correct for variations in fiber length.
C-
C-   Created  17-JUL-1995   Tim McMahon, Tao Hu
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER  IMOD,IOCT,ICELL,IEVEN,NCELL
      REAL     TXYZ(3), XYZ(3), TSHIFT
      REAL     YS,ZS,AZS,YS_SHIFT,AZS_SHIFT
C----------------------------------------------------------------------

C
C ___ Correct for timing variations along counter length and width
C
      IOCT  = MOD(IMOD,10)
C          3,11,19,27,35,43,51,59 gets mapped to 1-8
      ICELL = (NCELL+5)/8
      IEVEN = 0
      IF (ICELL.EQ.2 .OR. ICELL.EQ.4 .OR. ICELL.EQ.6 .OR.
     &      ICELL.EQ.8)     IEVEN = 1
C
      IF (IOCT.EQ.1.OR.IOCT.EQ.2) THEN
            YS  = TXYZ(1) - XYZ(1)
            ZS  = TXYZ(3) - XYZ(3)
      ELSE
            YS  = TXYZ(2) - XYZ(2)
            ZS  = TXYZ(3) - XYZ(3)
      ENDIF
C
C ___  orient YS
C
      IF ((IOCT.EQ.0.OR.IOCT.EQ.3).AND.IEVEN.EQ.1)  YS = -YS
      IF ( IOCT.EQ.1              .AND.IEVEN.EQ.0)  YS = -YS
      IF ( IOCT.EQ.2              .AND.IEVEN.EQ.1)  YS = -YS
      IF ((IOCT.EQ.7.OR.IOCT.EQ.4).AND.IEVEN.EQ.0)  YS = -YS
C
C ___  set limits to how far in YS and ZS correction is made
C
      IF (YS .GT.  140.)  YS =  140.
      IF (YS .LT. -140.)  YS = -140.
      AZS = ABS(ZS)
      IF (AZS .GT. 32.)  AZS = 32.
C
      YS_SHIFT = 0.
      AZS_SHIFT = 0.
C
      IF (YS.LT. -5. .OR. YS.GT. 5.) THEN      ! don't correct around center
          IF (YS.GT.0.) THEN
             YS_SHIFT = -.82001 - 2.04486E-2*YS + 1.96974E-4*YS*YS
          ELSE
             YS_SHIFT =  .43819 + 8.11091E-4*YS + 1.11467E-4*YS*YS
          ENDIF
      ENDIF
C
      AZS_SHIFT =  .41946 - 5.67510E-3*AZS - 1.05297E-3*AZS*AZS
C
      TSHIFT = YS_SHIFT + AZS_SHIFT
C
  999 RETURN
      END
