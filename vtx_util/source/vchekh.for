      SUBROUTINE VCHEKH(TASK,LAYER,SECTOR,NHIT,NGOOD)
C------------------------------------------------------------------------  
C
C  Loop over hits in cell (LAYER,SECTOR) and flag hits which should not 
C  be used in track finding. For Monte Carlo ideal hits, simulate two-hit 
C  resolution. Loop over hits and check time. Flag hits that have time within 
C  84 ns from previous hit on the same wire. Good hits are those that remain 
C  unflagged.  
C  Check if tracking has already been done in this cell (bit 2 set).
C  If yes, return NGOOD=0. ***NOTE: THIS FEATURE CHANGED 11-FEB-93***
C                     
C  Input: 
C         TASK           = 0 for tracking in volumes
C                        = 1 for full tracking
C                        = 2 vertex finding
C         LAYER,SECTOR
C         NHIT           = number of hits in VSEC
C
C  Output:
C         NGOOD = number of good hits
C
C  Daria Zieminska Jul., 1987
C-   Updated  11-FEB-1993   Ed Oltman Remove tagging of sectors:Segment finding
C                 
C                 
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER TASK,LAYER,SECTOR,WIRE,NGOOD,NEL,NWORDS
      INTEGER STAT,DONE,ION,SMEARD,IER,ICALL
      INTEGER NWIRES,MAXHIT
      CHARACTER*4 PATH
      PARAMETER (NWIRES=7)      ! maximum wire# (counting from 0)
      PARAMETER (MAXHIT=200)
      REAL QHIT(18*MAXHIT)
      REAL CONT(18),TIME              
      INTEGER NHITS(0:NWIRES),IPTR(0:NWIRES)
      INTEGER NHIT,IHIT,IPT,LOC,IWORD
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LUSER 
C-----------------------------------------------------------------
C  Variables used when the information on ISAJET track is used. 
      INTEGER ISATRA,IORIG
      REAL PX,PY,PZ,P,MASS,PHI,THETA,ETA,PT 
C-----------------------------------------------------------------
      DATA ICALL/0/
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('ION',ION,IER)
        CALL EZGET('IDONE',DONE,IER)
        CALL EZGET('SMEARD',SMEARD,IER)
        CALL EZRSET
        ICALL=1
      END IF
      CALL PATHGT(PATH)
      STAT=IQ(LVSEC(SECTOR,LAYER))
      IF (BTEST(STAT,DONE)) THEN
        NGOOD=0
        GO TO 1000
      ELSE
        IF (TASK.EQ.0) THEN
          IF (BTEST(STAT,ION)) THEN
C
C ****  Following tagging removed 11-Feb-93: Do segment finding from scratch
C ****  for each road since segment finding includes conversion of drift 
C ****  distance to space points (VPOINT).  This transformation is now 
C ****  z-dependant, hence, depends on z of road
C
            CONTINUE
C            IQ(LVSEC(SECTOR,LAYER))=IBSET(STAT,DONE)
          ELSE
            NGOOD=0
            GO TO 1000
          END IF
        ELSE IF (TASK.EQ.1) THEN 
          IQ(LVSEC(SECTOR,LAYER))=IBSET(STAT,DONE)
        END IF
      END IF
      NGOOD=NHIT
      IF (PATH.EQ.'RECO') GO TO 1000
      LUSER=LQ(LHEAD-IZUSER) 
      CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
      CALL SMVSEC(LVSEC(SECTOR,LAYER),NEL,NWORDS,QHIT)
      IQ(LVSEC(SECTOR,LAYER))=IBSET(STAT,SMEARD)
      DO 200 WIRE=0,NWIRES
        IF (NHITS(WIRE).LE.0) GO TO 200
        DO 300 IHIT=1,NHITS(WIRE)        
          IPT=IPTR(WIRE)-2*NEL-4+NWORDS*(IHIT-1)
C------------------------------------------------------------------
C
C  get ISAJET parameters of the track, if needed 
C
C          CALL UCOPY(QHIT(IPT+11),ISATRA,1)
C          CALL GTISP1(ISATRA,IORIG,PX,PY,PZ,P,MASS,PHI,THETA,ETA)
C  
C------------------------------------------------------------------
          IF (IHIT.GT.1) THEN
            IF (ABS(QHIT(IPT+9)-TIME).LT.84.) THEN
              LOC=LVSEC(SECTOR,LAYER)+IPT+2*NEL+3
              IWORD=IQ(LOC+10)
              IQ(LOC+10)=IBSET(IWORD,2)
              NGOOD=NGOOD-1
            END IF
          END IF
          TIME=QHIT(IPT+9)
          IF (QHIT(IPT+9).LT.30.) THEN
            LOC=LVSEC(SECTOR,LAYER)+IPT+2*NEL+3
            IWORD=IQ(LOC+10)
            IF (.NOT.BTEST(IWORD,2)) THEN
              IQ(LOC+10)=IBSET(IWORD,2)
              NGOOD=NGOOD-1
            END IF
          END IF                                      
  300   CONTINUE
  200 CONTINUE
 1000 CONTINUE
      RETURN                  
      END
