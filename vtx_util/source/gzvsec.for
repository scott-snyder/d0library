      INTEGER FUNCTION GZVSEC(ILAYV,ISECV)
C-----------------------------------------------------------------------
C-  Integer function GZVSEC returns the link to the bank "VSEC" for a 
C-  specified vertex detector layer and sector.
C-  Inputs:
C-    ILAYV and ISECV are the VTX layer and sector respectively.
C-  Output:
C-    GZVSEC is the link to "VSEC", 0 if "VSEC" not booked.
C-
C-   T. Trippe, 4 Jan. 1987, D. Zieminska Mar. 1987                         
C-----------------------------------------------------------------------
      IMPLICIT NONE                           
      INTEGER ILAYV,ISECV,LVLAY,GZVLAY
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZVSEC.LINK/LIST'
      GZVSEC=0                              
      LVLAY=GZVLAY(ILAYV) 
      IF (LVLAY.NE.0) GZVSEC=LQ(LVLAY-IZVSEC-ISECV)
      RETURN
      END
