C DEC/CMS REPLACEMENT HISTORY, Element GZISCM.FOR
C *1     5-JAN-1990 13:32:31 SERBAN "get pointer to ISCM, ISAJET commands"
C DEC/CMS REPLACEMENT HISTORY, Element GZISCM.FOR
      FUNCTION GZISCM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : pointer to ISCM bank (ISAJET commands)
C-
C-   Created   5-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISCM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$LINKS:IZISCM.LINK'
      INTEGER LISAB
C----------------------------------------------------------------------
C
      GZISCM=0
      LISAB=LQ(LHEAD-IZISAB)
      IF(LISAB.GT.0) GZISCM=LQ(LISAB-IZISCM)
  999 RETURN
      END
