      LOGICAL FUNCTION D0DAD_RE_RANGE(ISELR,ISELE,IRUN,IEVT)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Given a run/event range, determine whether the
C-      input run and event lie within the range.  If ISELR=ISELE=0,
C-      all events are selected.
C-
C-   Inputs  : ISELR - ISELR(1) ==> First allowed run, ISELR(2) ==> last
C-             ISELE - ISELE(1) ==> First allowed run, ISELE(2) ==> last
C-             IRUN  - Input run
C-             IEVT  - Input event
C-   Outputs :
C-   Controls:
C-
C-   Created  18-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRUN,IEVT,ISELR(2),ISELE(2)
      LOGICAL LRUN,LEVT
C-----------------------------------------------------------------------
      D0DAD_RE_RANGE=.FALSE.
C
C  Check for no test
C
      LRUN=ISELR(1).EQ.0 .AND. ISELR(2).EQ.0
      LEVT=ISELE(1).EQ.0 .AND. ISELE(2).EQ.0
      IF( LRUN .AND. LEVT ) THEN
        D0DAD_RE_RANGE=.TRUE.
        RETURN
      ENDIF
C
C  Skip if RUN not allowed
C
      IF( ISELR(1).NE.0 .AND. IRUN.LT.ISELR(1) ) GOTO 999
      IF( ISELR(2).NE.0 .AND. IRUN.GT.ISELR(2) ) GOTO 999
      IF(ISELR(2).EQ.0.AND.ISELR(1).NE.0.AND.IRUN.GT.ISELR(1)) GOTO 999
C
C  Check if need to test on event range.
C
      IF( LEVT ) THEN
        D0DAD_RE_RANGE=.TRUE.
        RETURN
      ENDIF
C
C  Skip if EVENT not allowed
C
      IF( ISELE(1).NE.0 .AND. IEVT.LT.ISELE(1) ) GOTO 999
      IF( ISELE(2).NE.0 .AND. IEVT.GT.ISELE(2) ) GOTO 999
      IF(ISELE(2).EQ.0.AND.ISELE(1).NE.0.AND.IEVT.GT.ISELE(1)) GOTO 999
C
      D0DAD_RE_RANGE=.TRUE.
C
 999  RETURN
      END
