      SUBROUTINE RRLAM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates Total Number of absorption
C-                         lengths in any direction in DZERO
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  04-APR-1986   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:ABSLN.INC/LIST'
      INTEGER IUNIT,IER
C
      INTEGER IX,IY,NUMED,IDHIST,NXX,NYY
      REAL PHI,THET,ST,TOTABS,PHID,THETI
C
      REAL X(6)
C
      CHARACTER*40 TITC
      INTEGER TITH(10)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL GTUNIT(30,IUNIT,IER)
      OPEN(UNIT=IUNIT,FILE='GEANT$LEGO',STATUS='NEW',
     +  FORM='UNFORMATTED')
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        IDHIST = 4400        !DO IT IN PHI VS THETA SPACE
        NXX = 17
        NYY = 500
        XMI = 0.
        XMA = PI/8.                !PHI INTERVAL N = 4. 16 SEGS MAKE 2*PI
        YMI = -PI/2.
        YMA = PI/2.        !THETA INTERVAL
        DELX = (XMA-XMI)/(NXX-1)
        DELY = (YMA-YMI)/(NYY)
      ENDIF
C
      DO 104 IX = 1,NXX
        PHI = XMI + (IX-1.0)*DELX
        IDHIST = IDHIST + 1
        PHID = PHI*180./PI
        WRITE(TITC,105)PHID
  105   FORMAT(' ABSLN VS THETA , PHI = ',F8.3,' $')
        CALL UCTOH(TITC,TITH,4,40)
        CALL HBOOK1(IDHIST,TITH,NYY,YMI,YMA)
        DO 204 IY = 1,NYY
          THET = YMI + (IY-0.5)*DELY
          THETI = YMA - THET                !SO THAT 1ST BIN  = 90 DEGREES
          ST = SIN(THET)
          X(1) = 0.
          X(2) = 0.
          X(3) = 0.
          X(4) = ST*COS(PHI)
          X(5) = ST*SIN(PHI)
          X(6) = COS(THET)
          TOTABS = 0.0
          NUMED = 0
          CALL ABSORB(X,NUMED,TOTABS)
          CALL HFILL(IDHIST,THETI,TOTABS)
  204   CONTINUE
        CALL HSTORE(IDHIST,IUNIT)
        CALL HPRINT(IDHIST)
  104 CONTINUE
      CLOSE(UNIT=IUNIT)
      CALL RLUNIT(30,IUNIT,IER)
      STOP
      END
