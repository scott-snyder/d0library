      INTEGER FUNCTION GZD0RGZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS LINK OF D0RG in ZEBCOM
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-SEP-1989   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZD0RGZ.LINK'
      INTEGER LHSTR,GZHSTR
C----------------------------------------------------------------------
      GZD0RGZ = 0
      LHSTR = GZHSTR()
      IF ( LHSTR.GT.0 ) GZD0RGZ = LQ(LHSTR-IZD0RGZ)
  999 RETURN
      END
