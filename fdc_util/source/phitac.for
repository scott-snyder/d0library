      SUBROUTINE PHITAC(HALF,SECTOR,YLOC,ZLOC,HLOC,HOHITS,ENOUGH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Accumulate unused hits from a Phi sector
C-
C-   Inputs  : HALF,SECTOR = Location of Phi sector
C-   Outputs : YLOC,ZLOC   = Drift distance and z position of hits by wire
C-             HLOC(WIRE,1)= Wire
C-             HLOC(WIRE,2)= Hit location in FPSC bank
C-             HLOC(WIRE,3)= L/R ambiguity
C-             HOHITS      = Number of hits per wire
C-             ENOUGH      = Logical set TRUE if enough hits for a segment
C-
C-   Created   4-JAN-1990   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-   Updated   6-JUL-1992   Tacy Joffe-Minor  include HITS_PER_WIRE 
C-   Updated  14-MAY-1993   Robert E. Avery  Maximum distance controlled by
C-                               MAXTIMEFACT rather than hardwired.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,SECTOR
      INTEGER NHITS(0:NBPSEN-1),IPTR(0:NBPSEN-1)
      INTEGER I1,IWIRE,IHIT,JWIRE,JHIT,LR,NEL,NWORDS
      INTEGER ISTAT,II,INEFF
      INTEGER HLOC(0:NBPSEN-1,MX_HIT_WIRE*2,3)
      INTEGER HOHITS(0:NBPSEN-1)
      INTEGER NOFF,ICALL,IER
      INTEGER HITS_PER_WIRE
C
      REAL    YLOC(0:NBPSEN-1,MX_HIT_WIRE*2)
      REAL    ZLOC(0:NBPSEN-1,MX_HIT_WIRE*2)
      REAL    RADIUS,XC,YC,ZC
      REAL    QHIT(18*MX_HIT_PSEC)
      REAL    CONT(36)
      REAL    STAT
      EQUIVALENCE (ISTAT,STAT)
      REAL MAXDIST, MAXFACT
C
      LOGICAL ENOUGH
C
      SAVE ICALL,INEFF
      DATA ICALL /0/
C
C----------------------------------------------------------------------
      IF(ICALL.EQ.0) THEN
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PINEFF',INEFF,IER)
        CALL EZGET('HITS_PER_WIRE',HITS_PER_WIRE,IER)
        CALL EZGET('MAXTIMEFACT',MAXFACT,IER)        
        CALL EZRSET
        MAXDIST = 5.5 * MAXFACT
      ENDIF
C
C  Initializations.
C
      CALL GTFPSC(HALF,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
C
C  Accumulate unused hits in a sector
C
      CALL GTFPSC(HALF,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      NOFF=2*NEL+4
      JWIRE = 0
      CALL VZERO(HOHITS,NBPSEN-1+1)
C
      DO 10 IWIRE=0,NBPSEN-1
        JHIT = 0
        CALL GTFALH(HALF,1,0,SECTOR,IWIRE,XC,YC,ZC)
        IF(ZC.EQ. 0.0) GOTO 10
C
        DO 20 IHIT=1,MIN(HITS_PER_WIRE,NHITS(IWIRE))
          I1=IPTR(IWIRE)-NOFF+NWORDS*(IHIT-1)
          IF(QHIT(I1+2).GT.MAXDIST) GO TO 20
          STAT=QHIT(I1+9)
          IF (BTEST(ISTAT,2)) GO TO 20              ! Skip used hit
          IF( JHIT .EQ. 0 ) JWIRE = JWIRE + 1
          DO 30 LR = 0,1
            JHIT = JHIT + 1
            YLOC(IWIRE,JHIT)=QHIT(I1+2+LR)      ! put in stagger only if
C                                                 XC,YC is added in.
            ZLOC(IWIRE,JHIT)=ZC         ! record wire z position
            HLOC(IWIRE,JHIT,1)=IWIRE    ! record wire 
            HLOC(IWIRE,JHIT,2)=IHIT     ! record hit location
            HLOC(IWIRE,JHIT,3)=LR       ! record L/R ambiguity
   30     CONTINUE
   20   CONTINUE
C
        HOHITS(IWIRE)=JHIT              ! Record number of hits on wire
   10 CONTINUE
C
      ENOUGH = .TRUE.
      IF( JWIRE .LT. NBPSEN-INEFF ) ENOUGH = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
