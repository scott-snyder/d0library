      SUBROUTINE GTL2EM_VALUE(ICAND,IPOINT,PASSED,FDUMMY,IDUMMY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a REAL or INTEGER value from L2EM bank.
C-
C-   Inputs  : ICAND  = number of bank in linear chain 
C-                      (from GTL2EM_COUNT).
C-             IPOINT = bank offset for requested value (see L2EM.ZEB)
C-             PASSED = only return quantity for passing candidates
C-
C-   Outputs : FDUMMY   = value from bank if stored as REAL
C-             IDUMMY   = value from bank if stored as INTEGER
C-             IER =  0 = OK
C-             IER = -1 = no L2EM banks on event
C-             IER = -2 = no L2EM bank corresponding to ICAND
C-             IER = -3 = bad offset pointer (i.e IPOINT > bank length)
C-             IER = -4 = L2_EM candidate did not pass and PASSED=.TRUE.
C-
C-   Controls: No control!
C-
C-   Created  19-JUL-1993   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ICAND,IPOINT,IDUMMY,IER,LL2EM,GZL2EM,I
      REAL FDUMMY
      LOGICAL PASSED
C----------------------------------------------------------------------
      IER = 0
      LL2EM = GZL2EM()
C
      IF(LL2EM.LE.0)THEN
        IER = -1
        GOTO 999
      ENDIF
C
      DO I = 1,ICAND-1
        LL2EM = LQ(LL2EM)
      ENDDO
C
      IF(LL2EM.LE.0)THEN
        IER = -2
        GOTO 999
      ENDIF
C
      IF(IPOINT.GT.IQ(LL2EM-1))THEN
        IER = -3
        GOTO 999
      ENDIF
C
      IF(PASSED.AND.(IQ(LL2EM+28).NE.0))THEN
        IER = -4
        GOTO 999
      ENDIF
C
      FDUMMY = Q(LL2EM+IPOINT)
      IDUMMY = IQ(LL2EM+IPOINT)
C
  999 RETURN
      END
