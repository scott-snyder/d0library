      INTEGER FUNCTION GZL2DPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Level2 Zebra bank DPDH 
C-
C-   Returned value  : pointer to Zebra bank DPDH under SL2H bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSL2H, LSL2H
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZL2DPDH = 0
      LSL2H = GZSL2H()
      IF ( LSL2H .GT. 0 ) GZL2DPDH = LC( LC(LSL2H-9) - 1)
C                                          L2CDC      DPDH
C----------------------------------------------------------------------
  999 RETURN
      END
