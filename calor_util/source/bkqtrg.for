      SUBROUTINE BKQTRG(LQTRG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-JUN-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZQTRG.LINK'
      INTEGER LANLS,GZANLS,LQTRG,IOH,IVER,NTT,NDATA
      LOGICAL FIRST
      DATA IVER / 1 /
      DATA NTT / 5 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL MZFORM('QTRG','4I 1F / 2I 1F',IOH)
      ENDIF
C
C  BOOK UNDER ANLS
C
      LANLS = GZANLS()
      IF (LANLS.LE.0) THEN
        CALL BKANLS(LANLS)
      ENDIF
C
      NDATA = 5 + NTT*3
      CALL MZBOOK(IXMAIN,LQTRG,LANLS,-IZQTRG,'QTRG',2,2,NDATA,IOH,0)
      IQ(LQTRG+1) = IVER
      IQ(LQTRG+2) = NTT
C
  999 RETURN
      END
