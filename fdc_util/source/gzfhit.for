      FUNCTION GZFHIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FHIT
C-
C-   Returned value  : pointer to Zebra bank FHIT
C-
C-   Created  10-JUL-1991   Qizhong Li-Demarteau
C-   Created  19-AUG-1991   Robert E. Avery
C-                      based on GZDHIT by Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFHIT.LINK'
      INTEGER GZFHIT, GZFDCH, LFDCH
C----------------------------------------------------------------------
      GZFHIT = 0
      LFDCH = GZFDCH()
      IF ( LFDCH .GT. 0 ) GZFHIT = LQ(LFDCH - IZFHIT)
  999 RETURN
      END
