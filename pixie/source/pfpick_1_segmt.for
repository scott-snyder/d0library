      FUNCTION PFPICK_1_SEGMT(SEGMENT,LAYER,HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prompt user to supply a single Segment
C-      (for whatever).
C-
C-   Returned value  : .TRUE. if good segment picked.
C-   Inputs  : none
C-   Outputs : SEGMENT,LAYER,HALF
C-
C-   Created  14-NOV-1991   Robert E. Avery
C-   Updated  17-FEB-1992   Robert E. Avery  If hardcopy, don't ask questions.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PFPICK_1_SEGMT 
C
      INTEGER HALF,SEGMENT,LAYER
C
      INTEGER IERR
      INTEGER IADD,NHITS,IHALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER NSEGLYR(0:1),NSEGM(0:1,0:2),NZBANK
      INTEGER LSEGM(0:1,0:2),GZFSEG
      INTEGER ICALL,IER,LEN,NLYR,IMOD,II,JJ
      INTEGER HALF_PREV,SEG_PREV,LAYER_PREV
      REAL    X_INTERCEPT,X_SLOPE 
      REAL    FSTAGR,RADIUS,CHISQ
      REAL    XC,YC,ZC
C
      INTEGER ICONT(62)
      REAL    CONT(62)
      EQUIVALENCE(ICONT,CONT)
C
      LOGICAL FLGVAL,HARDCOPY 
C
      CHARACTER*5 CHALF(0:1)
      CHARACTER*60 PROM1,PROM2,PROM3
      CHARACTER*80 STRING
      CHARACTER*112 FTEXT
C
      DATA CHALF/'    N','    S'/
      DATA ICALL /0/
      DATA PROM1/' Enter HALF - N(orth) or S(outh) (default prev)>'/
      DATA PROM2/' Enter Layer 0,1,2 (default previous)>'/
      DATA PROM3/' Enter Segment Number (default previous else 1)>'/
      DATA SEG_PREV  /1/
C----------------------------------------------------------------------
      PFPICK_1_SEGMT = .FALSE.
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( HARDCOPY ) THEN         
        PFPICK_1_SEGMT = .TRUE.
        HALF = HALF_PREV
        LAYER = LAYER_PREV 
        SEGMENT = SEG_PREV
        GOTO 999
      ENDIF
C
C  Find the number of segments by FDC Half and FDC Layer
C
      DO HALF=0,1
        NSEGLYR(HALF) = 0
        DO LAYER=0,2
          LSEGM(HALF,LAYER) = GZFSEG(HALF,LAYER)
          IF(LSEGM(HALF,LAYER).GT.5) THEN
            NSEGM(HALF,LAYER)=NZBANK(0,LSEGM(HALF,LAYER))
            NSEGLYR(HALF) = NSEGLYR(HALF) + 1
          ELSE
            NSEGM(HALF,LAYER)=0
          ENDIF
        ENDDO
      ENDDO
      IF( (NSEGLYR(0)+NSEGLYR(1)).LE.0) THEN 
        CALL OUTMSG(' No segments found in FDC, try next event.')
        GOTO 999
      END IF
C
C  Display layers with segments.
C
      HALF=-1
      CALL INTMSG(' ')
      DO IHALF=0,1
        IF(NSEGLYR(IHALF).GT.0) THEN
          IF (IHALF.EQ.0) WRITE(FTEXT,100) 
     &      (NSEGM(IHALF,LAYER),LAYER=0,2)
          IF (IHALF.EQ.1) WRITE(FTEXT,110) 
     &      (NSEGM(IHALF,LAYER),LAYER=0,2)
          CALL INTMSG(FTEXT)
        ELSE
          HALF=1-IHALF
        ENDIF
      ENDDO
C
C  Make choice of FDC Half containing segments to draw, if necessary.
C
      CALL OUTMSG('1')
      IF(HALF.LT. 0) THEN
        HALF = HALF_PREV
        CALL OUTMSG('    Choose FDC Half of segments (N or S)')
        STRING=' '
        LEN=0
        CALL GETPAR(1,PROM1,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LEN)
        IF ( LEN .NE. 0 ) THEN
          IF (STRING(1:1).EQ.'N'.OR.STRING(1:1).EQ.'n') THEN
            HALF = 0
          ELSE IF (STRING(1:1).EQ.'S'.OR.STRING(1:1).EQ.'s') THEN
            HALF = 1
          ELSE
            READ (STRING(1:LEN),*,ERR=999) HALF 
          END IF
        END IF
        IF (HALF.LT.0 .OR. HALF.GT.1) THEN
          CALL INTMSG(' Bad choice of Half.  Default to North.')
          HALF = 0
        END IF
      ENDIF
      HALF_PREV = HALF
C
C  Choose Layer
C
      STRING=' '
      LEN=0
      LAYER = LAYER_PREV 
      CALL GETPAR(1,PROM2,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LEN)
      IF (STRING(1:1).EQ.'I'.OR.STRING(1:1).EQ.'i') THEN
        LAYER = 0
      ELSE IF (STRING(1:1).EQ.'O'.OR.STRING(1:1).EQ.'o') THEN
        LAYER = 1
      ELSE IF (STRING(1:1).EQ.'P'.OR.STRING(1:1).EQ.'p') THEN
        LAYER = 2
      ELSEIF (LEN.NE.0) THEN
        READ (STRING(1:LEN),*,ERR=999) LAYER
      END IF
      IF(LAYER .LT. 0 .OR. LAYER .GT. 2 ) THEN
        CALL INTMSG(' Bad choice of layer.  Default to 0')
        LAYER = 0
      ENDIF
      LAYER_PREV = LAYER
C
C  Choose Segment
C
      IMOD=HALF*3+LAYER
      IF(LAYER.LE.1) THEN
        IF (LAYER.EQ.0) 
     &    CALL INTMSG(' Inner Theta (Non-X-sect, Full Cell):')
        IF (LAYER.EQ.1) 
     &    CALL INTMSG(' Outer Theta (Non-X-sect, Full Cell):')
        CALL INTMSG('  SEGNO  HALF  QUAD  SECT NHITS   SLOPE'//
     &              '  XDRIFT  CHISQ')
      ELSE
        CALL INTMSG(' Phi: (Non-X-sect):')
        CALL INTMSG('  SEGNO  HALF  SECT NHITS   SLOPE'//
     &              '  XDRIFT   CHISQ')
      ENDIF
C
      DO SEGMENT = 1,NSEGM(HALF,LAYER)
        CALL GTFSEG(IMOD,SEGMENT,CONT)
        IADD=ICONT(2)
        NHITS=ICONT(3)
        CALL FCODER(IADD,IHALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        IF ( (ABS(ICONT(1)).LT.1000) 
     &   .AND. ( (UNIT.EQ.1).OR.(SECTOR.GT.2) ) )THEN  
C
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
          IF(UNIT.EQ.0) THEN
            RADIUS = ((XC)**2. + (YC)**2.)**.5
     &             - FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE) 
            X_SLOPE = CONT(30)
            X_INTERCEPT = CONT(31) + ZC * X_SLOPE- RADIUS 
            CHISQ = MIN( 999.0, CONT(32) )
            WRITE( FTEXT,101) SEGMENT,CHALF(HALF), QUAD, SECTOR, NHITS,
     &                       X_SLOPE,X_INTERCEPT ,CHISQ 
          ELSE
            X_SLOPE = CONT(51)
            X_INTERCEPT = CONT(52) + ZC * X_SLOPE
            CHISQ = MIN( 999.0, CONT(53) )
            WRITE( FTEXT,111) SEGMENT,CHALF(HALF), SECTOR, NHITS,
     &                       X_SLOPE,X_INTERCEPT ,CHISQ 
          ENDIF
          CALL INTMSG(FTEXT)
        ENDIF
      ENDDO
C
      STRING=' '
      LEN=0
      SEGMENT = SEG_PREV
      CALL GETPAR(1,PROM3,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LEN)
      IF(LEN.NE.0) READ(STRING(1:LEN),*,ERR=999) SEGMENT
      IF(SEGMENT.LT.0 .OR. SEGMENT.GT.NSEGM(HALF,LAYER)) THEN
        IF ( NSEGM(HALF,LAYER) .GT. 0) THEN
          SEGMENT = 1
          CALL OUTMSG(' Bad choice of segment, default to 1')
        ELSE
          SEGMENT = 0
          CALL OUTMSG(' Bad choice of segment')
          GOTO 999
        ENDIF
      ENDIF
      SEG_PREV = SEGMENT
C
      PFPICK_1_SEGMT = .TRUE.
C
  100 FORMAT(' North FDC has segments in Inner Theta(',I4,
     &  '), Outer Theta(',I4,'), Phi(',I4,')')
  110 FORMAT(' South FDC has segments in Inner Theta(',I4,
     &  '), Outer Theta(',I4,'), Phi(',I4,')')
  101 FORMAT(X,I6,A6,3I6,3F8.2)
  111 FORMAT(X,I6,A6,2I6,3F8.2)
C
  999 RETURN
      END
