      INTEGER FUNCTION GZPARH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to PARH 
C-
C-   Created  13-JAN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPARH.LINK'
      INTEGER GZPROC,LPROC,LPARH
C----------------------------------------------------------------------
C
      LPROC=GZPROC()
      LPARH=0
      IF(LPROC.NE.0)  LPARH=LQ(LPROC-IZPARH)
      GZPARH=LPARH
C
      RETURN
      END
