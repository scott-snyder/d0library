      FUNCTION L2_GZDPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DPDH in level 2
C-
C-   Returned value  : pointer to Zebra bank DPDH 
C-
C-   Created  10-AUG-1992   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
      INTEGER L2_GZDPDH, L2_GZSCDC
C----------------------------------------------------------------------
      L2_GZDPDH = 0
      LSCDC = L2_GZSCDC()
      IF ( LSCDC .GT. 0 ) L2_GZDPDH = LC(LSCDC - IZDPDH)
C
  999 RETURN
      END
