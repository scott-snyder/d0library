      SUBROUTINE PFPICK_SECTOR(UNIT,HALF,QUAD,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Choose which FDC Sector to display for 
C-      various FDC displays. (If hardcopy, don't ask questions.)
C-      
C-
C-   Inputs  : UNIT
C-   Outputs : HALF,QUAD,SECTOR
C-
C-   Created  19-FEB-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C INPUT:
      INTEGER UNIT
C OUTPUT:
      INTEGER HALF,QUAD,SECTOR
C LOCAL:
      INTEGER II, JJ
      INTEGER IER
      INTEGER LEN
      INTEGER RUNSAV(0:1), IDSAV(0:1), RUN, ID
C
      CHARACTER*60 ANSWER
      CHARACTER*80 PROM1,PROM2,PROM3,PROM4
C
      LOGICAL PRINT_LIST_RCP
      LOGICAL EZERROR
      LOGICAL FLGVAL,HARDCOPY 
C
      DATA PROM1 /' Half N(orth) or S(outh)? (2=Back to Menu)>'/
      DATA PROM2 /' Quadrant? (0 to 7)>'/
      DATA PROM3 /' Sector? (0-5)>'/
      DATA PROM4 /' Sector?(0-35 for Phi)>'/
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_SECTOR_HITS',
     &    'Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV( 'FDC HALF', HALF)
      CALL PUGETV( 'FDC QUAD', QUAD)
      CALL PUGETV( 'FDC SECT', SECTOR)
      CALL PUGETV( 'FDC PRINT LIST',PRINT_LIST_RCP)
      CALL EZRSET
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( .NOT.HARDCOPY ) THEN         
        IF ( PRINT_LIST_RCP ) THEN
          CALL PFHBYS(UNIT)
        ELSE
          CALL EVNTID(RUN,ID)
          IF (RUN.NE.RUNSAV(UNIT) .OR. ID.NE.IDSAV(UNIT)) THEN
            RUNSAV(UNIT)=RUN
            IDSAV(UNIT)=ID
            CALL PFHBYS(UNIT)
          ENDIF
        ENDIF
      ENDIF
C
      IF ( UNIT.EQ.0 ) THEN             ! Theta Chamber
        IF ( .NOT.HARDCOPY ) THEN         
C
C   Display sectors with the hits and get the necessary information
C   to identify FADC data desired
C
          CALL OUTMSG('1')
          ANSWER=' '
          LEN=0
          CALL GETPAR(1,PROM1,'U',ANSWER) 
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF ( LEN.NE.0 ) THEN
            IF (ANSWER(1:1).EQ.'N'.OR.ANSWER(1:1).EQ.'n') THEN
              HALF = 0
            ELSE IF (ANSWER(1:1).EQ.'S'.OR.ANSWER(1:1).EQ.'s') THEN
              HALF = 1
            ELSE
              READ ( ANSWER(1:LEN),*,ERR=999) HALF 
            END IF
          END IF
          IF (HALF.EQ.2) GOTO 999
C
          ANSWER=' '
          LEN=0
          CALL GETPAR(1,PROM2,'U',ANSWER) 
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF (LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=999)QUAD
C
          ANSWER=' '
          LEN=0
          CALL GETPAR(1,PROM3,'U',ANSWER) 
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF (LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=999) SECTOR
        ENDIF
C
        IF (HALF.LT.0 .OR. HALF.GT.1) HALF = 0
        IF (QUAD.LT.0 .OR. QUAD.GT.7) QUAD = 0
        IF (SECTOR.LT.0 .OR. SECTOR.GT.5) SECTOR = 0
C
      ELSE                              ! Phi chamber
C
C   If hard copy, use defaults (previous values), else prompt user.
C
        IF ( .NOT.HARDCOPY ) THEN         
C
C   Display sectors with the hits and get the necessary information
C   to identify FADC data desired
C
          CALL OUTMSG('1')
          ANSWER=' '
          LEN=0
          CALL GETPAR(1,PROM1,'U',ANSWER) 
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF ( LEN .NE. 0 ) THEN
            IF (ANSWER(1:1).EQ.'N'.OR.ANSWER(1:1).EQ.'n') THEN
              HALF = 0
            ELSE IF (ANSWER(1:1).EQ.'S'.OR.ANSWER(1:1).EQ.'s') THEN
              HALF = 1
            ELSE
              READ ( ANSWER(1:LEN),*,ERR=999) HALF 
            END IF
          END IF
          IF (HALF .EQ. 2) GOTO 999
C
          ANSWER=' '
          LEN=0
          CALL GETPAR(1,PROM4,'U',ANSWER) 
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF (LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=999) SECTOR
        ENDIF
C
        IF (HALF .LT. 0 .OR. HALF .GT. 1) HALF=0
        IF (SECTOR .LT. 0 .OR. SECTOR .GT. 35 ) SECTOR=0
      ENDIF
C
  999 RETURN
      END
