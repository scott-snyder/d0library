      INTEGER FUNCTION RUNNO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return run value for last event in memory
C-
C-   Created  30-NOV-1987   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LASTRN,IREC
      SAVE LASTRN
C----------------------------------------------------------------------
      DATA LASTRN/0/
      RUNNO=LASTRN
      IF(LHEAD.NE.0) THEN
        IREC=MOD(IQ(LHEAD+1),1000)
        IF(IREC.GT.4.OR.IREC.EQ.1) LASTRN=IQ(LHEAD+6)
        RUNNO=LASTRN
      ENDIF
  999 RETURN
      END
