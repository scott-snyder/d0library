      INTEGER FUNCTION EVONUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Return event output number
C-
C-   Created   1-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LASTEV,IREC
      SAVE LASTEV
C----------------------------------------------------------------------
      DATA LASTEV/0/
      EVONUM=LASTEV
      IF(LHEAD.NE.0) THEN
        IREC=MOD(IQ(LHEAD+1),1000)
        IF(IREC.GT.4) LASTEV=IQ(LHEAD+9)
        EVONUM=LASTEV
      ENDIF
  999 RETURN
      END
