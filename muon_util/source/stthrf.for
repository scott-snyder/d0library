      SUBROUTINE STTHRF(LMUOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : refill STTH bank using information in SATN/S
C-
C-   Inputs  : LMUOT - address of the MUOT bank
C-   Outputs :
C-   Controls:
C-
C-   Created  31-AUG-1995   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER NMAX
      PARAMETER (NMAX=40)
      INTEGER IFLG,NHA,NHBC,NTA,NTBC,L2F1,L2F2
      REAL PT,THT,PHI,XYZI(6),XYZO(6),CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE
      INTEGER NHIT,IHIT(NMAX),IGEO(NMAX),IDIS(NMAX)
      INTEGER IADD(NMAX)
      REAL DRFT(NMAX),GEOM(6,NMAX)
      REAL    P,DP,DPS,BDL
      INTEGER LSATR,GZSATN,GZSATS,LMUOT,muot_ltoi
      EXTERNAL GZSATN,GZSATS,muot_ltoi
      INTEGER TRS,TRN,QUAD,NTR,I,J,IREF,imuot
      INTEGER NST,NSC,ITUBE,MODULE
      REAL RTUBE(3,2),VTUBE(3,2),TLEN(2)
      CHARACTER*80 MSG
C----------------------------------------------------------------------
      CALL GRLINK('STTHRF',IREF)
      LRLINK(IREF)=LMUOT
      CALL MVBITS(IQ(LMUOT+6),12,4,TRS,0) ! reference to track in SATN/S
      QUAD=IQ(LMUOT+3)
      IF(QUAD.EQ.13) THEN
        LSATR=GZSATN()
      ELSE
        LSATR=GZSATS()
      ENDIF
      NTR=IQ(LSATR+1)
      if(ntr.ne.0) then
      IF(TRS.LE.0.OR.TRS.GT.NTR) THEN ! recover reference using muon momentum
        P=Q(LMUOT+23)
        DPS=1000.
        TRS=0
        DO TRN=1,IQ(LSATR+1)
          DP=ABS(P-Q(LSATR+3+(TRN-1)*150))
          IF(DP.LT.DPS) THEN
            DPS=DP
            TRS=TRN
          END IF
        END DO
        CALL MVBITS(TRS,0,4,IQ(LMUOT+6),12) ! reference to track in SATN/S bank
      ENDIF
      CALL GTSATR(QUAD-12,TRS,IFLG,PT,THT,PHI,XYZI,XYZO,NHA,NHBC,
     &    CHI,CHO,DTRK,PLAN,DVTX,ECAL,EFE,NTA,NTBC,L2F1,L2F2,BDL,
     &    NHIT,IHIT,IGEO,IDIS)
C
C   Book and fill bank STTH
C
      DO I=1,NHIT
        NST  =  IBITS (IHIT(I), 0, 5)    ! station number
        NSC  =  IBITS (IHIT(I), 5, 5)    ! section number
        ITUBE = IBITS (IHIT(I), 16, 16)  ! tube number
        CALL    SATOPM(NST,NSC,MODULE)
        IADD(I) = 256*MODULE + ITUBE
        CALL GTSSTG(NST,NSC,-IGEO(I),RTUBE,VTUBE,TLEN)
        DO J=1,3
          GEOM(J,I) = RTUBE(J,1)
          GEOM(J+3,I) = VTUBE(J,1)
        END DO
        DRFT(I) = FLOAT(IDIS(I))*1.E-5
      END DO
      imuot=muot_ltoi(lmuot)
      CALL STTHFL(iMUOT,NHIT,IADD,GEOM,DRFT)
      else
c         this never should occur, but somehow happens 
        WRITE (MSG, '(''  STTHRF: no track in SATN/S '',I7,1x,i7)')
     &         iq(lhead+12),iq(lhead+9)
        CALL INTMSG (msg)
        end if
      LMUOT=LRLINK(IREF)
      CALL RRLINK('STTHRF',IREF)
  999 RETURN
      END
