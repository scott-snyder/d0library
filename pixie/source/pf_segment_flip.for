      SUBROUTINE PF_SEGMENT_FLIP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pixie action routine allows user to choose
C-      an FDC segment that will be drawn flipped to the opposite side
C-      of the FDC sector.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  14-NOV-1991   Robert E. Avery
C-   Updatye  26-MAR-1992   Lupe Howell Modify for Unix
C-   Updated  16-AUG-1993   Robert E. Avery  Remove use of LUSER bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FLOCAL.INC/LIST'
      INCLUDE 'D0$INC:PXPARA.INC'

      INTEGER MAXHIT
      PARAMETER( MAXHIT = 15 )

      INTEGER IADD,NHIT
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB
      INTEGER SEGMENT,LAYER
      INTEGER IXFSEG
      INTEGER OFFSET1, OFFSET2 
      INTEGER MODULE,HIT,LR
      INTEGER SAVE_DRAW_PERP
      INTEGER ISTRQU,IER
C
      REAL    X_SLOPE 
      REAL    X_RESID 
      REAL    X_INTERCEPT 
      REAL    RADIUS 
      REAL    X_DRIFT 
      REAL    Z(MAXHIT),X_FLIPPED(MAXHIT)
      REAL    RESID(MAXHIT)
      REAL    CHISQ,CHISQ_NEW
      REAL    DELTA, DELTA_NEW 
      REAL    XC,YC,ZC
      REAL    FSTAGR
      REAL    XPOS ,YPOS 
C
      LOGICAL PFPICK_1_SEGMT,OK 
      LOGICAL FIRST 
      LOGICAL EZERROR
C
      REAL    CSIZE
      PARAMETER( CSIZE = 1.5 )
C
      CHARACTER*100 TEXT
C
      INTEGER ICONT(62)
      REAL    CONT(62)
      EQUIVALENCE(ICONT,CONT)
C----------------------------------------------------------------------
C
C
      IF ( FIRST ) THEN
        CALL MZFORM('FDCT','2I ',IXFSEG)
        FIRST = .FALSE.
      ENDIF
C
      OK = PFPICK_1_SEGMT(SEGMENT,LAYER,HALF)
      IF ( .NOT. OK ) THEN
        GOTO 999
      ENDIF
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF_PR_SEGMENTS',
     &      'Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C Flip the segment
      MODULE = HALF*3+LAYER
      CALL GTFSEG(MODULE,SEGMENT,CONT)
      IADD = ICONT(2)
      NHIT = ICONT(3)
      CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
      IF (ABS(ICONT(1)).GT.50) THEN  
        CALL INTMSG(' Can not flip a X-sector segment.')
      ENDIF
      IF ( (UNIT.EQ.0).AND.(SECTOR.LE.2) ) THEN
        CALL INTMSG(' Can not flip a half cell segment.')
      ENDIF
C
      IF ( UNIT .EQ. 0) THEN
        OFFSET1 = 21
        OFFSET2 = 29
      ELSE
        OFFSET1 = 33
        OFFSET2 = 54
      ENDIF
      X_SLOPE = CONT(OFFSET2+1)
      X_INTERCEPT = CONT(OFFSET2+2)
      CHISQ = CONT(OFFSET2+3)
      DELTA = 0
      DO HIT = 1, NHIT
        X_RESID = CONT(OFFSET1+HIT)
        WIRE = INT(CONT(3+HIT)/2.)
        LR = INT(CONT(3+HIT)) - WIRE*2
        CALL GTFALH(HALF,UNIT,QUAD,SECTOR,WIRE,XC,YC,ZC)
        Z(HIT) = ZC
        IF ( UNIT.EQ.0 ) THEN
          RADIUS = ((XC)**2. + (YC)**2.)**.5
        ELSE
          RADIUS = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE) 
        ENDIF
        X_DRIFT = X_RESID + X_INTERCEPT + ZC * X_SLOPE - RADIUS
        X_FLIPPED(HIT) = RADIUS - X_DRIFT 

        DELTA = DELTA + X_RESID *(-1)**(LR+WIRE)
      ENDDO
      DELTA = ABS(DELTA)
C
C  Refit
C
      CALL SEGFIT(Z,X_FLIPPED,NHIT,
     &  X_SLOPE, X_INTERCEPT ,CHISQ_NEW, RESID)
C
C  Compute quantities associated w. flipped segment
C
      DELTA_NEW = 0
      DO HIT = 1, NHIT
        WIRE = INT(CONT(3+HIT)/2.)
        LR = INT(CONT(3+HIT)) - WIRE*2
        DELTA_NEW = DELTA_NEW + RESID(HIT) *(-1)**(LR+WIRE)
      ENDDO
      DELTA_NEW = ABS(DELTA_NEW)
C
      CALL GTFALH(HALF,UNIT,QUAD,SECTOR,0,XC,YC,ZC)
      IF ( UNIT.EQ.0 ) THEN
        RADIUS = ((XC)**2. + (YC)**2.)**.5
      ELSE
        RADIUS = FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE) 
      ENDIF
      X_DRIFT = (X_INTERCEPT-RADIUS) + X_SLOPE * ZC 
C
C  Save in TEMP BANK
C
      CALL MZLINT(IXCOM,'/FLOCAL/',FLOCAL,LFLOC,FLOCAL)
      CALL MZBOOK(IXMAIN,LFLOC,0,2,'FSEG',0,0,2,IXFSEG,0)
      Q(LFLOC+1) = X_DRIFT
      Q(LFLOC+2) = X_SLOPE
C
C Draw it.
C
      CALL PUGETV('FDC DRAW PERP',SAVE_DRAW_PERP)
      CALL PUSETV('FDC DRAW PERP',0)
      IF(UNIT.EQ.0) THEN
        CALL PF_THETA_SECT_VIEW(HALF,QUAD,SECTOR)
      ELSEIF(UNIT.EQ.1) THEN
        CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
      ENDIF
      CALL PUSETV('FDC DRAW PERP',SAVE_DRAW_PERP)
C
C Print some info:
C
      CALL PUGETV( 'STRING QUALITY', ISTRQU)
      IF (ISTRQU .LT. 3) CALL PUSETV( 'STRING QUALITY', 3)
      CALL PUOPEN
      CALL JJUST( 1, 2)
C
      YPOS = -YWIND2*0.8
      XPOS = -XWIND2
      WRITE( TEXT,100)CHISQ,DELTA
  100 FORMAT(' Original Segment fit,  Chisq = ',F7.2,', Delta = ',F7.3)
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
C
      YPOS = -YWIND2*0.9
      WRITE( TEXT,200)CHISQ_NEW, DELTA_NEW
  200 FORMAT( ' Flipped Segment fit,  Chisq = ',F7.2,', Delta = ',F7.3)
      CALL PUVSTR( XPOS, YPOS , CSIZE, CSIZE, TEXT )
      CALL JRCLOS
C
C Drop user bank
C
      IF ( LFLOC.GT.0 ) THEN
        CALL MZDROP(IXCOM,LFLOC,'L')
        FLOCAL(1) = 0
        LFLOC = 0
      ENDIF
C
      CALL PUSETV( 'STRING QUALITY', ISTRQU)
      CALL EZRSET
  999 RETURN
      END
