      INTEGER FUNCTION GZL2FPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Level2 Zebra bank FPDH 
C-
C-   Returned value  : pointer to Zebra bank FPDH under SL2H bank
C-
C-   Cloned from GZL2DPDH
C-   Note - need link in SL2H to SFDC 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSL2H, LSL2H
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZL2FPDH = 0
      LSL2H = GZSL2H()
      IF ( LSL2H .GT. 0 ) GZL2FPDH = LC( LC(LSL2H-10) - 1)
C                                          L2CDC      FPDH
C----------------------------------------------------------------------
  999 RETURN
      END
