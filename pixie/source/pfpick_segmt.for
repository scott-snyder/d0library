      FUNCTION PFPICK_SEGMT(LADDER,HALF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Requests which segments the user wants in 3-D
C-                         FDC Segment display
C-
C-   Returned value  : .FALSE. if no segments in event, or other 
C-                              significant error, else .TRUE. .
C-   Inputs  : none
C-   Outputs : LADDER(0:2) = FDC segments by layer
C-             HALF   = FDC Half of track
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated   7-AUG-1990   Jeffrey Bantly  put in 3 views
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank checks 
C-   Updated   9-FEB-1991   Robert E. Avery  Convert to function,
C-                                pass back FALSE for no tracks or error. 
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves
C-                                Include EZPICK and EZRSET for PUxETV calls.
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array.
C-   Updated   9-SEP-1991   Robert E. Avery  Use previous half as default.
C-   Updated  18-NOV-1991   Robert E. Avery  Change Segment information. 
C-   Updated  25-JAN-1992   Robert E. Avery  Only display segment information 
C-                              the first time called in event.
C-   Updated  17-FEB-1992   Robert E. Avery  If hardcopy, don't ask questions.
C-   Updated  17-FEB-1992   Robert E. Avery  Remove X02,Y02 calculation. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      LOGICAL PFPICK_SEGMT
C
C  OUTPUT:
      INTEGER HALF,LADDER(0:2)
C
C  LOCAL:
      INTEGER IERR
      INTEGER IADD,NHITS,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER ISEG,NSEGLYR(0:1),NSEGM(0:1,0:2),NZBANK
      INTEGER LSEGM(0:1,0:2),GZFSEG,LKFTRH,GZFTRH
      INTEGER ICALL,IER,LEN,ILYR,NLYR,IMOD,PREV_SEG(0:2),II,JJ
      INTEGER IHALF,HALF_PREV,JSEG(0:2)
      INTEGER RUNSAV(0:1), IDSAV(0:1), RUN, ID
C
      REAL    XMIN, XMAX, YMIN, YMAX, Z0(2), XC, YC, ZC
      REAL    X2SUM,Y2SUM
      REAL    CONT(62),PHI,THETA,FIADD,FNHITS,RESID
      REAL    RADIUS,FSTAGR,SLOPE,INTERCEPT,CHISQ,AVEION
      EQUIVALENCE(IADD,FIADD)
      EQUIVALENCE(NHITS,FNHITS)
C
      CHARACTER*1 CHALF(0:1)
      CHARACTER*5 CUNIT(0:1)
      CHARACTER*60 PROM1,PROM2
      CHARACTER*80 STRING
      CHARACTER*112 FTEXT
      CHARACTER*25 LAYER_STRING(0:2,0:1)
C
      LOGICAL PRINT_LIST, PRINT_LIST_RCP
      LOGICAL EZERROR
      LOGICAL FLGVAL,HARDCOPY 
C
      DATA CHALF/'N','S'/
      DATA CUNIT/'THETA',' PHI '/
      DATA ICALL /0/
      DATA PROM1/' Enter HALF - N(orth) or S(outh) (default prev)>'/
      DATA PROM2/' Enter Segment Number (default previous else 1)>'/
      DATA PREV_SEG  /1,1,1/
      DATA PRINT_LIST_RCP /.TRUE./
      DATA LAYER_STRING / ' North Inner Theta:',
     &                    ' North Outer Theta:',
     &                    ' North Phi:',
     &                    ' South Inner Theta:',
     &                    ' South Outer Theta:',
     &                    ' South Phi:'/
C----------------------------------------------------------------------
      PFPICK_SEGMT = .FALSE.
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( HARDCOPY  ) THEN
        PFPICK_SEGMT = .TRUE.
        HALF=HALF_PREV 
        DO ILYR =  0, 2
          LADDER(ILYR) = PREV_SEG(ILYR) 
        ENDDO
        GOTO 999
      ENDIF
C
      CALL OUTMSG('1')
      LKFTRH=GZFTRH()
      IF (LKFTRH.LE.0) THEN
        CALL OUTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DSGDR','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
C  Find the number of segments by FDC Half and FDC Layer
C
      DO IHALF=0,1
        NSEGLYR(IHALF) = 0
        DO ILYR=0,2
          LSEGM(IHALF,ILYR) = GZFSEG(IHALF,ILYR)
          IF (LSEGM(IHALF,ILYR).GT.5) THEN
            NSEGM(IHALF,ILYR)=NZBANK(0,LSEGM(IHALF,ILYR))
            NSEGLYR(IHALF) = NSEGLYR(IHALF) + 1
          ELSE
            NSEGM(IHALF,ILYR)=0
          ENDIF
        ENDDO
      ENDDO
      IF ( (NSEGLYR(0)+NSEGLYR(1)).LE.0) THEN 
        CALL OUTMSG(' No segments found in FDC, try next event.')
        GOTO 990
      END IF
C
C  Display layers with segments.
C
      HALF=-1
      CALL INTMSG(' ')
      DO 5 IHALF=0,1
        IF (NSEGLYR(IHALF).GT.0) THEN
          IF (IHALF.EQ.0) WRITE(FTEXT,100) (NSEGM(IHALF,ILYR),ILYR=0,2)
          IF (IHALF.EQ.1) WRITE(FTEXT,200) (NSEGM(IHALF,ILYR),ILYR=0,2)
          CALL INTMSG(FTEXT)
        ELSE
          HALF=1-IHALF
        ENDIF
    5 CONTINUE
C
C  Make choice of FDC Half containing segments to draw, if necessary.
C
      IF (HALF.LT. 0) THEN
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
            READ (STRING(1:LEN),*,ERR=980) HALF 
          END IF
        END IF
        IF (HALF.LT.0 .OR. HALF.GT.1) THEN
          CALL INTMSG(' Bad choice of Half.  Default to North.')
          HALF = 0
        END IF
      ENDIF
      HALF_PREV = HALF
C
C  Decide whether or not to print full list:
C
      CALL PUGETV('FDC PRINT LIST',PRINT_LIST_RCP)
      IF ( PRINT_LIST_RCP ) THEN
        PRINT_LIST = .TRUE.
      ELSE
        CALL EVNTID(RUN,ID)
        IF (RUN.NE.RUNSAV(HALF) .OR. ID.NE.IDSAV(HALF)) THEN
          RUNSAV(HALF)=RUN
          IDSAV(HALF)=ID
          PRINT_LIST = .TRUE.
        ELSE
          PRINT_LIST = .FALSE.
        ENDIF
      ENDIF
C
C  Make choice of segments to draw, one per layer.  0=none for that layer.
C
      NLYR = 0
      CALL INTMSG(' ')
      DO 10 ILYR=0,2
C
        IF ( PRINT_LIST ) THEN
          CALL INTMSG( LAYER_STRING(ILYR,HALF) )
          IF (ILYR.LE.1) THEN
            CALL INTMSG('  Seg  Hlf Quad  Sct Nhit   Phi  Theta'//
     &        '  Slope XDrift  Chisq AveIon') 
          ELSE
            CALL INTMSG('  Seg  Hlf  Sct Nhit   Phi  Slope'//
     &        ' XDrift  Chisq AveIon') 
          ENDIF
C
          IMOD=HALF*3+ILYR
          DO 20 ISEG=1,NSEGM(HALF,ILYR)
            CALL GTFSEG(IMOD,ISEG,CONT)
            FIADD=CONT(2)
            FNHITS=CONT(3)
            CALL FCODER(IADD,IHALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
            CALL GTFALH(IHALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
            IF (UNIT.EQ.0) THEN
              PHI=CONT(20)
              THETA=CONT(21)
              SLOPE = CONT(30)
              RADIUS = ((XC)**2. + (YC)**2.)**.5
     &               - FSTAGR(IHALF,UNIT,QUAD,SECTOR,WIRE) 
              INTERCEPT = CONT(31) + ZC * SLOPE- RADIUS 
              CHISQ = MIN( 999.0, CONT(32) )
              AVEION = CONT(33)
              WRITE( FTEXT,102) ISEG,CHALF(IHALF),QUAD,SECTOR,
     &          NHITS,PHI,THETA,SLOPE,INTERCEPT,CHISQ,AVEION
            ELSE
              PHI=CONT(36)
              SLOPE = CONT(55)
              INTERCEPT = CONT(56) + ZC * SLOPE
              CHISQ = MIN( 999.0, CONT(57) )
              AVEION = CONT(58)
              WRITE( FTEXT,106) ISEG,CHALF(IHALF),SECTOR,
     &          NHITS,PHI,SLOPE,INTERCEPT,CHISQ,AVEION
            ENDIF
            CALL INTMSG(FTEXT)
   20     CONTINUE
        ENDIF
C
        IF (ILYR.EQ.0) WRITE(FTEXT,107) NSEGM(HALF,ILYR)
        IF (ILYR.EQ.1) WRITE(FTEXT,117) NSEGM(HALF,ILYR)
        IF (ILYR.EQ.2) WRITE(FTEXT,127) NSEGM(HALF,ILYR)
        CALL OUTMSG(FTEXT)
        STRING=' '
        LEN=0
        CALL GETPAR(1,PROM2,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LEN)
        IF (LEN.NE.0) READ(STRING(1:LEN),*,ERR=980) JSEG(ILYR)
        IF (LEN.LE.0) JSEG(ILYR) = PREV_SEG(ILYR)
        IF (JSEG(ILYR).LT.0 .OR. JSEG(ILYR).GT.NSEGM(HALF,ILYR)) THEN
          JSEG(ILYR) = 0
          CALL OUTMSG(' Bad choice of segment; use default value of 0.')
        ENDIF
        PREV_SEG(ILYR) = JSEG(ILYR)
        LADDER(ILYR) = JSEG(ILYR)
        IF (LADDER(ILYR).GT.0) NLYR=NLYR+1
   10 CONTINUE
C
      IF (NLYR.GT.0) PFPICK_SEGMT = .TRUE.
      GOTO 990
C
  100 FORMAT(' North FDC has segments in Inner Theta(',I4,
     &  '), Outer Theta(',I4,'), Phi(',I4,')')
  200 FORMAT(' South FDC has segments in Inner Theta(',I4,
     &  '), Outer Theta(',I4,'), Phi(',I4,')')
  102 FORMAT(X,I4,A4,3I5,6F7.2)
  106 FORMAT(X,I4,A4,2I5,5F7.2)
  107 FORMAT(' Inner Theta segment to draw (0 to ',I4,'):')
  117 FORMAT(' Outer Theta segment to draw (0 to ',I4,'):')
  127 FORMAT(' Phi segment to draw (0 to ',I4,'):')
C----------------------------------------------------------------------
C
  980 CONTINUE
      CALL INTMSG(' Error reading input.')
C
  990 CONTINUE
      CALL EZRSET
C
  999 RETURN
      END
