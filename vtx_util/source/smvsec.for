      SUBROUTINE SMVSEC(LVSEC,NEL,NWORDS,CONT)
C------------------------------------------------------------------------  
C
C  Fetch Zebra bank VSEC (bank of of 'ideal' hits from Geant) and smear
C  the drift and z coordinate
C  
C  Input:  LVSEC  = location of bank 'VSEC'
C                     
C  Output: CONT   = modified bank contents 
C                   (smeared drift time and distance and z coordinate)
C                     
C  D.Zieminska Sept.,1987
C  D.Zieminska Apr., 1988  added pametrization of resolution 
C
C  The single hit resolution is parametrized in the following way:
C  For drift_dist > 0.2 cm  sigma=0.0030+0.004*drift**2 (see D0-651)
C  For 0.025 cm < drift_dist < 0.2 cm  sigma=0.00063/drift (C.Klopfenstein)  
C  For drift_dist < 0.025 cm - hit discarded
C
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INTEGER NHIT,MAXHIT,LVSEC,IHIT,LOCHIT,NEL,NWORDS,SMEARD,STAT
      REAL SIG,SIGT,DRIFT,RANDOM      
      PARAMETER (MAXHIT=200)
      REAL CONT(18*MAXHIT)
      CHARACTER*4 PATH 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER IER, ICALL
      DATA ICALL/0/
C-----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('SMEARD',SMEARD,IER)
        CALL EZRSET
        ICALL=1
      END IF
C                      
      NHIT=IQ(LVSEC+1)
      STAT=IQ(LVSEC)
      CALL PATHGT(PATH)
      IF (BTEST(STAT,SMEARD).OR.PATH.EQ.'RECO') GO TO 900 
        DO 100 IHIT=1,NHIT
          LOCHIT=LVSEC+2*NEL+3+NWORDS*(IHIT-1)
          DRIFT=Q(LOCHIT+9)*0.00084             ! drift distance
          IF (DRIFT.LT.0.025) GO TO 100
          IF (DRIFT.GT.0.2) THEN
            SIG=0.003+0.004*DRIFT**2
          ELSE
            SIG=0.0006/DRIFT
          END IF
          SIGT=SIG/0.00084
          Q(LOCHIT+5)=SIG
          CALL NORRAN(RANDOM)
          Q(LOCHIT+9)=Q(LOCHIT+9)+RANDOM*SIGT   ! smear drift time
          Q(LOCHIT+2)=Q(LOCHIT+2)+RANDOM*SIG    ! smear drift coordinate
          Q(LOCHIT+3)=Q(LOCHIT+3)-RANDOM*SIG    ! on both sides
          Q(LOCHIT+6)=1.0                       ! z coord. accuracy        
          SIG=Q(LOCHIT+6)                       
          CALL NORRAN(RANDOM)
          Q(LOCHIT+4)=Q(LOCHIT+4)+RANDOM*SIG    ! smear z coordinate
  100   CONTINUE
  900   CONTINUE
        CALL UCOPY(Q(LVSEC+2*NEL+4),CONT,NWORDS*NHIT)
 1000 RETURN
      END
