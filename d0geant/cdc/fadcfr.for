      FUNCTION FADCFR(I,WIDTH,P3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate fraction area of one FADC bin,
C-                         relative to area of whole pulse.
C-                         
C-              integrate a Rayleigh function:
C-              
C-              F(t) = P1 * (t - P2) * exp(-(t-P2)**2 / (2*P3**2)
C-              
C-                       integrated F(t) from P2+I to P2+I+1
C-              FADCFR = ----------------------------------------
C-                       integrated F(t) from P2 to P2+WIDTH
C-              (P2 is the leading edge of the pulse)         
C-
C-
C-   Inputs  : I:     relative FADC channel number in the pulse
C-             WIDTH: the total # of channels of the pulse
C-             P3:    rise time
C-   Outputs : FADCFR
C-   Controls: none
C-
C-   Created  27-MAR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZPULPR.INC/LIST'
C
      REAL    WIDTH, P3
      REAL    TW, PW, IREAL, BNAREA, TTAREA, FADCFR
      INTEGER I
C
C----------------------------------------------------------------------
C
      IREAL = I * NBPBIN
      PW = P3 - NBPBIN  
      TW = WIDTH * NBPBIN
C
C    now integrate F(t)
C
      BNAREA = EXP(-(IREAL / PW)**2 / 2) 
     &         - EXP(-((IREAL + NBPBIN) / PW)**2 / 2)
      TTAREA = 1 - EXP(-(TW / PW)**2 / 2) 
C
      FADCFR = BNAREA / TTAREA
C
  999 RETURN
      END
