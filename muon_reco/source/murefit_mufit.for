      SUBROUTINE MUREFIT_MUFIT(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make an MFIT bank based on refit MUOT bank
C-
C-   Inputs: none
C-   Outputs: none
C-
C-   Created  07-MAR-1993   D.Wood, based on MUFIT and MFIT_FILL
C-                          for the case KFIT=0
C-   Modified 17-Nov-1995   D.Wood, intialize IERL=0
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      LOGICAL FIRST
      INTEGER QUAD,IMTRK,ICHARGE,MUVERT,ITER,IERR
      INTEGER HITA,HIT_W,HIT_S,NFITDA,NFITDBC
C
      REAL DIC(3),DOC(3)
      REAL DE_T,DE_C,PFIT,DPFIT,CHI_TRACK
      REAL PX,PY,PZ,PP,PT
      REAL MAG(3)
      REAL VMU(3),B_TOR,EPS
      INTEGER LMUON,AMISS,IERZ,IFM,IFC
      INTEGER IND_MUOT,IMUOT,IMUON,LMUOT,LMFIT
      INTEGER NSMUON,NSMFIT,IV
      INTEGER sakfit,ierl
C
      INTEGER  GZMUON,MUOT_LTOI
C
      EXTERNAL GZMUON
C
      DATA FIRST/.TRUE./
      DATA EPS/0.000001/
C ======================================================================
      IERR = 0
      IF(FIRST)THEN
C ****  Read MURECO.RCP file
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('MUVERT',MUVERT,IERZ)
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET('SAKFIT',SAKFIT,IERZ)
        IF( IERZ.NE.0 ) SAKFIT = 2
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C loop over MUON banks
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        NSMUON = IQ(LMUON-2)
        LMUOT = LQ(LMUON-NSMUON-1)
        IF(LMUOT.GT.0) THEN
C must be careful - MUOT banks are not necessarily ordered the same as
C MUON banks
          IND_MUOT = MUOT_LTOI(LMUOT)
          IF(IND_MUOT.GT.0) THEN
            IERL = 0
            IF(iq(lmuot+3).eq.13.or.iq(lmuot+3).eq.14) then ! samus track
              imtrk=iq(lmuon-5)
              IF( SAKFIT.GE.1 ) CALL SAKMFIT(Ind_muot,IMTRK,IERL)
              IF( SAKFIT.GE.2 ) CALL SA_MFIT_FILL(Ind_muot,IMTRK,IERL)
            END IF
            IF (ierl.NE.1) THEN
              AMISS = IQ(LMUOT+4)
C get the relevant stuff from the MUOT bank
              CALL COP_MUOT(MUVERT,IND_MUOT,HITA,
     &          HIT_W,HIT_S,NFITDA,NFITDBC,
     &          QUAD,MAG,DIC,DOC,VMU,PFIT,DPFIT,DE_T,DE_C,
     &          PX,PY,PZ,ITER,CHI_TRACK,B_TOR)
C
C guard against zebra stampedes
              CALL GRLINK('MUREFIT_MUFIT', IMUON)
              LRLINK(IMUON) = LMUON
              CALL GRLINK('MUREFIT_MUFIT', IMUOT)
              LRLINK(IMUOT) = LMUOT
C book the MFIT bank
              CALL BKMFIT(IMUON,LMFIT)
              LMUON = LRLINK(IMUON)
              LMUOT = LRLINK(IMUOT)
              CALL RRLINK('MUREFIT_MUFIT',IMUON)
              CALL RRLINK('MUREFIT_MUFIT',IMUOT)
C fill in reference link to MUOT
              NSMFIT = IQ(LMFIT - 2)
              LQ(LMFIT - NSMFIT - 1) = LMUOT
C
C fit method = copy of MUOT
              IFM = 3
C track origin
              IF(MUVERT.NE.0) THEN
                IFC = 3                    ! "other" (e.g. cosmic)
              ELSE
                IF(HITA.GT.0) THEN
                  IFC = 2                  ! A-layer
                ELSE
                  IFC = 1                  ! vertex
                ENDIF
              ENDIF
              IF(IFC .EQ. 1 .OR. IFC .EQ. 3) THEN
                CALL VERXYZ2(MUVERT,VMU,IV)
              ENDIF
C
C fill the MFIT contents
              ICHARGE = INT(PFIT/ABS(PFIT))
C
              IQ(LMFIT+1) = 1
              IQ(LMFIT+2) = HIT_W
              IQ(LMFIT+3) = HIT_S
              IQ(LMFIT+4) = QUAD
              IQ(LMFIT+5) = 2
              IQ(LMFIT+6) = IFC
              IF(AMISS .EQ. 1 .OR. AMISS .EQ. 4)IQ(LMFIT+6)=1
              IF(ABS(VMU(1)) .LE. EPS .AND. ABS(VMU(2)) .LE. EPS)
     &          IQ(LMFIT+6)=1
              IQ(LMFIT+7) = IFM
              IQ(LMFIT+8) = 0
              IQ(LMFIT+9) = 0
              IQ(LMFIT+10) = ICHARGE
C
              Q(LMFIT+11)= VMU(1)
              Q(LMFIT+12)= VMU(2)
              Q(LMFIT+13)= VMU(3)
C
              Q(LMFIT+14) = MAG(1)
              Q(LMFIT+15) = MAG(2)
              Q(LMFIT+16) = MAG(3)
C
              Q(LMFIT+17)= DOC(1)
              Q(LMFIT+18)= DOC(2)
              Q(LMFIT+19)= DOC(3)
C
              Q(LMFIT+20)= PX
              Q(LMFIT+21)= PY
              Q(LMFIT+22)= PZ
              PP = ABS(PFIT) !NOTE! MOMENTUM IS NOT SIGNED
              Q(LMFIT+23)= PP
              PT = SQRT(PX*PX + PY*PY)
              Q(LMFIT+24)= PT
              Q(LMFIT+25)= (DPFIT * PX )**2
              Q(LMFIT+26)= (DPFIT * PY )**2
              Q(LMFIT+27)= (DPFIT * PZ )**2
              Q(LMFIT+28)= (DPFIT * PP )**2
              Q(LMFIT+29)= (DPFIT * PT )**2
              Q(LMFIT+30)= CHI_TRACK
              Q(LMFIT+31)= B_TOR
              Q(LMFIT+32)= DE_C
              Q(LMFIT+33)= DE_T
C
            ENDIF
          END IF
        ENDIF
        LMUON = LQ(LMUON)
      ENDDO                             ! end of loop over MUON banks
C
C
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
CC-----------------------------------------------------------------C
