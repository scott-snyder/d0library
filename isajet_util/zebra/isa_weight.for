C DEC/CMS REPLACEMENT HISTORY, Element ISA_WEIGHT.FOR
C *1    30-JAN-1990 18:06:29 SERBAN "get ISAJET weight"
C DEC/CMS REPLACEMENT HISTORY, Element ISA_WEIGHT.FOR
      FUNCTION ISA_WEIGHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      return weight for ISAJET event
C-   Returned value  : weight (microbarns/event)
C-
C-   Created  30-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ISA_WEIGHT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INTEGER GZISAE,LISAE
C----------------------------------------------------------------------
C
      ISA_WEIGHT=0
      LISAE=GZISAE()
      IF(LISAE.GT.0) ISA_WEIGHT=Q(LISAE+12)
  999 RETURN
      END
