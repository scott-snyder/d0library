      SUBROUTINE THITAC(HALF,QUAD,SECTOR,WIRBEG,WIREND,
     &                               YLOC,ZLOC,HLOC,HOHITS,ENOUGH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Accumulate unused hits from a Theta sector
C-
C-   Inputs  : HALF,QUAD,SECTOR = Location of Theta sector
C-             WIRBEG,WIREND = beginning and ending wires of interest
C-   Outputs : YLOC,ZLOC   = Drift distance and z position of hits by wire
C-             HLOC(WIRE,1)= Wire
C-             HLOC(WIRE,2)= Hit location in FPSC bank
C-             HLOC(WIRE,3)= L/R ambiguity
C-             HOHITS      = Number of hits per wire
C-             ENOUGH      = Logical set TRUE if enough hits for a segment
C-
C-   Created   4-JAN-1990   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   8-NOV-1990   Jeffrey Bantly  add mirror hits for 1/2 cell
C-                                          hits close to wire. 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated  21-FEB-1992   Susan K. Blessing  Change call to include
C-    WIRBEG and WIREND.
C-   Updated   6-JUL-1992   Tacy Joffe-Minor  include HITS_PER_WIRE 
C-   Updated  14-MAY-1993   Robert E. Avery  Maximum distance controlled by
C-                               MAXTIMEFACT rather than hardwired.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER WIRBEG,WIREND
      INTEGER NHITS(0:NBTSEN-1),IPTR(0:NBTSEN-1)
      INTEGER I1,IWIRE,IHIT,JWIRE,JHIT,LR,LRMAX,NEL,NWORDS
      INTEGER ISTAT,INEFF
      INTEGER HLOC(0:NBTSEN-1,MX_HIT_WIRE*2,3)
      INTEGER HOHITS(0:NBTSEN-1)
      INTEGER NOFF,ICALL,IER
      INTEGER HITS_PER_WIRE
C
      REAL    YLOC(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    ZLOC(0:NBTSEN-1,MX_HIT_WIRE*2)
      REAL    RADIUS,STAGGER,XC,YC,ZC,DIR
      REAL    CONT(18)
      REAL    STAT
      REAL    QHIT(18*MX_HIT_TSEC)
      EQUIVALENCE (ISTAT,STAT)
      REAL    FSTAGR
      REAL MAXDIST, MAXFACT
C
      LOGICAL ENOUGH
C
      SAVE ICALL,INEFF
      DATA ICALL /0/
C----------------------------------------------------------------------
      IF(ICALL.EQ.0) THEN
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('TINEFF',INEFF,IER)
        CALL EZGET('HITS_PER_WIRE',HITS_PER_WIRE,IER)
        CALL EZGET('MAXTIMEFACT',MAXFACT,IER)        
        CALL EZRSET
        MAXDIST = 5.5 * MAXFACT
      ENDIF
C
C  Initializations.
C
      UNIT=0
      LRMAX=1
      IF (SECTOR .LE. 2) LRMAX=0
      DIR=1.
      IF(SECTOR .EQ. 1) DIR=-1.
      CALL GTFTSC(HALF,QUAD,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
C
C  Accumulate unused hits in a sector
C
      CALL GTFTSC(HALF,QUAD,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      NOFF=2*NEL+4
      JWIRE = 0
      CALL VZERO(HOHITS,NBTSEN)
C
C  Loop over desired sense wires in sector.
C
      DO 10 IWIRE = WIRBEG,WIREND
        JHIT = 0
        CALL GTFALH(HALF,UNIT,QUAD,SECTOR,IWIRE,XC,YC,ZC)
        IF(ZC.EQ. 0.0) GOTO 10
        RADIUS=((XC)**2. + (YC)**2.)**.5
        STAGGER=0.
        STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTOR,IWIRE)
C
C  Loop over all hits on sense wire.
C
        DO 20 IHIT=1,MIN(HITS_PER_WIRE,NHITS(IWIRE))
          I1=IPTR(IWIRE)-NOFF+NWORDS*(IHIT-1)
          IF(QHIT(I1+2).GT.MAXDIST) GO TO 20
          STAT=QHIT(I1+9)
          IF (BTEST(ISTAT,2)) GO TO 20              ! Skip used hit
          IF( JHIT .EQ. 0 ) JWIRE = JWIRE + 1
C
C  Store hit info for hits and mirror hits if present.
C
          DO 30 LR = 0,LRMAX
            JHIT = JHIT + 1
            YLOC(IWIRE,JHIT)=RADIUS+(QHIT(I1+2+LR)*DIR)-STAGGER
C                    stagger subtracted to use hits located from central wire
C                    plane.  XC,YC includes the stagger, FTSC does not.
            ZLOC(IWIRE,JHIT)=ZC         ! record wire z position
            HLOC(IWIRE,JHIT,1)=IWIRE    ! record wire
            HLOC(IWIRE,JHIT,2)=IHIT     ! record hit location
            HLOC(IWIRE,JHIT,3)=LR       ! record L/R ambiguity
   30     CONTINUE
C
C  If half-sector  (S<3) then check for hits close to wires on neg side.
C
          IF(LRMAX.EQ.0 ) THEN
            IF(SECTOR.LE.2) THEN
              IF(QHIT(I1+2+1).NE.0.0) THEN
                LR=1
                JHIT = JHIT + 1
                YLOC(IWIRE,JHIT)=RADIUS+(QHIT(I1+2+LR)*DIR)-STAGGER
                ZLOC(IWIRE,JHIT)=ZC         ! record wire z position
                HLOC(IWIRE,JHIT,1)=IWIRE    ! record wire
                HLOC(IWIRE,JHIT,2)=IHIT     ! record hit location
                HLOC(IWIRE,JHIT,3)=LR       ! record L/R ambiguity
              ENDIF
            ENDIF
          ENDIF
C
   20   CONTINUE
C
        HOHITS(IWIRE)=JHIT              ! Record number of hits on wire
   10 CONTINUE
C
      ENOUGH = .TRUE.
      IF( JWIRE .LT. NBTSEN-INEFF ) ENOUGH = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
