      SUBROUTINE PCENERGY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate Max. E/ET in Eta-Phi bin and in Phi
C-                         Histogram.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-JUL-1992   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
C----------------------------------------------------------------------
      REAL EMENBN,HDENBN,IMENBN,TOTENBN
      REAL EMETBN,HDETBN,IMETBN,TOTETBN
      REAL EMENHS,HDENHS,IMENHS,TOTENHS
      REAL EMETHS,HDETHS,IMETHS,TOTETHS
      REAL EEM(NPHIL,-NETAL:NETAL)    ! EM energy
      REAL EHD(NPHIL,-NETAL:NETAL)    ! HAD energy(fine+coarse)
      REAL EICDMG(NPHIL,-NETAL:NETAL) ! Energy in ICD and MG
      REAL EHPH(NPHIL),EMPH(NPHIL),EIMPH(NPHIL)
      REAL ETEM(NPHIL,-NETAL:NETAL)    ! EM Et
      REAL ETHD(NPHIL,-NETAL:NETAL)    ! HAD Et(fine+coarse)
      REAL ETICDMG(NPHIL,-NETAL:NETAL) ! Et in ICD and MG
      REAL ETHPH(NPHIL),ETMPH(NPHIL),ETIMPH(NPHIL)
      REAL ENER,ET,ESUM,ETSUM
      INTEGER LCAEH,GZCAEH,IPCAEH,NRP,NCH
      INTEGER I,IE,IP,IL,JPHI,IETA
      INTEGER RUN,EVT,RUNSAVE,EVTSAVE
      INTEGER IOPT
      REAL MXEMBN,MXHDBN,MXIMBN,MXTOTBN
      REAL MXEMHS,MXHDHS,MXIMHS,MXTOTHS
      SAVE RUNSAVE,EVTSAVE
      DATA RUNSAVE,EVTSAVE/ -1, -1 /
C----------------------------------------------------------------------
C-
      CALL EVNTID(RUN,EVT)
      IF(RUN.EQ.RUNSAVE .AND. EVT.EQ.EVTSAVE)  GO TO 999
      RUNSAVE = RUN
      EVTSAVE = EVT
C-
      LCAEH = GZCAEH()
      IF ( LCAEH .LE. 0 )             GO TO 999
      NRP   = IQ(LCAEH+2)
      NCH   = IQ(LCAEH+3)
C-
C--- Fill Energy/Et Array
C-
      CALL VZERO(EEM,NPHIL*(2*NETAL+1))
      CALL VZERO(EHD,NPHIL*(2*NETAL+1))
      CALL VZERO(EICDMG,NPHIL*(2*NETAL+1))
C-
      CALL VZERO(ETEM,NPHIL*(2*NETAL+1))
      CALL VZERO(ETHD,NPHIL*(2*NETAL+1))
      CALL VZERO(ETICDMG,NPHIL*(2*NETAL+1))
C-
      DO 10 I=1,NCH
        IPCAEH = LCAEH+3+(I-1)*NRP
        ENER   = Q(IPCAEH+4)
        ET     = Q(IPCAEH+5)
        IE     = IQ(IPCAEH+9)
        IP     = IQ(IPCAEH+10)
        IL     = IQ(IPCAEH+11)
        IF(ENER.LE.0) GO TO 10
        IF(IE.GE.-32 .AND. IE.LE.32) THEN
          IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
            EEM(IP,IE)  = EEM(IP,IE) + ENER
            ETEM(IP,IE) = ETEM(IP,IE) + ET
          ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
            EHD(IP,IE)  = EHD(IP,IE) + ENER
            ETHD(IP,IE) = ETHD(IP,IE) + ET
          ELSEIF(IL.GE.MNLYMG .AND. IL.LE.MXLYMG) THEN
            EICDMG(IP,IE)  = EICDMG(IP,IE) + ENER
            ETICDMG(IP,IE) = ETICDMG(IP,IE) + ET
          ENDIF
        ELSE
          ENER = ENER/2.
          ET   = ET/2.
          IF(IL.GE.MNLYEM .AND. IL.LE.MXLYEM) THEN
            EEM(IP,IE)    = EEM(IP,IE) + ENER
            EEM(IP+1,IE)  = EEM(IP+1,IE) + ENER
            ETEM(IP,IE)   = ETEM(IP,IE) + ET
            ETEM(IP+1,IE) = ETEM(IP+1,IE) + ET
          ELSEIF(IL.GE.MNLYFH .AND. IL.LE.MXLYCH) THEN
            EHD(IP,IE)    = EHD(IP,IE) + ENER
            EHD(IP+1,IE)  = EHD(IP+1,IE) + ENER
            ETHD(IP,IE)   = ETHD(IP,IE) + ET
            ETHD(IP+1,IE) = ETHD(IP+1,IE) + ET
          ENDIF
        ENDIF
   10 CONTINUE
C-
C--- Get Max. Energy/Et for Eta-Phi Bin and Phi Histogram
C-
      EMENBN = 0.
      HDENBN = 0.
      IMENBN = 0.
      TOTENBN= 0.
      EMENHS = 0.
      HDENHS = 0.
      IMENHS = 0.
      TOTENHS= 0.
C-
      EMETBN = 0.
      HDETBN = 0.
      IMETBN = 0.
      TOTETBN= 0.
      EMETHS = 0.
      HDETHS = 0.
      IMETHS = 0.
      TOTETHS= 0.
C---
      DO JPHI=1,NPHIL
        EMPH(JPHI)  = 0.
        EHPH(JPHI)  = 0.
        EIMPH(JPHI) = 0.
C-
        ETMPH(JPHI) = 0.
        ETHPH(JPHI) = 0.
        ETIMPH(JPHI)= 0.
        DO 20 IETA=-NETAL,NETAL
          IF(IETA .EQ. 0)      GO TO 20
          IF(EEM(JPHI,IETA) .GT. EMENBN)      THEN
            EMENBN = EEM(JPHI,IETA)
          ENDIF
          IF(EHD(JPHI,IETA) .GT. HDENBN)      THEN
            HDENBN = EHD(JPHI,IETA)
          ENDIF
          IF(EICDMG(JPHI,IETA) .GT. IMENBN)   THEN
            IMENBN = EICDMG(JPHI,IETA)
          ENDIF
          EMPH(JPHI) = EMPH(JPHI)+EEM(JPHI,IETA)
          EHPH(JPHI) = EHPH(JPHI)+EHD(JPHI,IETA)
          EIMPH(JPHI)= EIMPH(JPHI)+EICDMG(JPHI,IETA)
          ESUM = EEM(JPHI,IETA)+EHD(JPHI,IETA)+EICDMG(JPHI,IETA)
          IF (ESUM .GT. TOTENBN) TOTENBN = ESUM
C-
          IF(ETEM(JPHI,IETA) .GT. EMETBN)      THEN
            EMETBN = ETEM(JPHI,IETA)
          ENDIF
          IF(ETHD(JPHI,IETA) .GT. HDETBN)      THEN
            HDETBN = ETHD(JPHI,IETA)
          ENDIF
          IF(ETICDMG(JPHI,IETA) .GT. IMETBN)   THEN
            IMETBN = ETICDMG(JPHI,IETA)
          ENDIF
          ETMPH(JPHI)=ETMPH(JPHI)+ETEM(JPHI,IETA)
          ETHPH(JPHI)=ETHPH(JPHI)+ETHD(JPHI,IETA)
          ETIMPH(JPHI)=ETIMPH(JPHI)+ETICDMG(JPHI,IETA)
          ETSUM = ETEM(JPHI,IETA)+ETHD(JPHI,IETA)+ETICDMG(JPHI,IETA)
          IF (ETSUM .GT. TOTETBN) TOTETBN = ETSUM
   20   CONTINUE
        IF(EMPH(JPHI) .GT. EMENHS)   EMENHS = EMPH(JPHI)
        IF(EHPH(JPHI) .GT. EMENHS)   HDENHS = EHPH(JPHI)
        IF(EIMPH(JPHI) .GT. IMENHS)  IMENHS = EIMPH(JPHI)
        ESUM = EMPH(JPHI)+EHPH(JPHI)+EIMPH(JPHI)
        IF (ESUM .GT. TOTENHS) TOTENHS = ESUM
C-
        IF(ETMPH(JPHI) .GT. EMETHS)   EMETHS = ETMPH(JPHI)
        IF(ETHPH(JPHI) .GT. EMETHS)   HDETHS = ETHPH(JPHI)
        IF(ETIMPH(JPHI) .GT. IMETHS)  IMETHS = ETIMPH(JPHI)
        ETSUM = ETMPH(JPHI)+ETHPH(JPHI)+ETIMPH(JPHI)
        IF (ETSUM .GT. TOTETHS) TOTETHS = ETSUM
      ENDDO
      RETURN
C----------------------------------------------------------------------
      ENTRY PC_GET_MAXEORET(IOPT,MXEMBN,MXHDBN,MXIMBN,MXTOTBN,
     &                           MXEMHS,MXHDHS,MXIMHS,MXTOTHS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Max. E/ET in Eta-Phi bin and in Phi
C-                         Histogram.
C-
C-   Inputs  : IOPT   [I] : 1 for Energy and 2 for Et
C-   Outputs : MXEMBN [F] : Max. EM E/Et in Eta-Phi bin
C-             MXHDBN [F] : Max. HD E/Et in Eta-Phi bin
C-             MXIMBN [F] : Max. ICD-MG E/Et in Eta-Phi bin
C-            MXTOTBN [F] : Max. Total E/Et in Eta-Phi bin
C-             MXEMHS [F] : Max. EM E/Et in Phi Histogram
C-             MXHDHS [F] : Max. HD E/Et in Phi Histogram
C-             MXIMHS [F] : Max. ICD-MG E/Et in Phi Histogram
C-            MXTOTHS [F] : Max. Total E/Et in Phi Histogram
C-
C---
      MXEMBN = 0.
      MXHDBN = 0.
      MXIMBN = 0.
      MXTOTBN= 0.
      MXEMHS = 0.
      MXHDHS = 0.
      MXIMHS = 0.
      MXTOTHS= 0.
      IF (IOPT .EQ. 1) THEN
        MXEMBN = EMENBN
        MXHDBN = HDENBN
        MXIMBN = IMENBN
        MXTOTBN= TOTENBN
        MXEMHS = EMENHS
        MXHDHS = HDENHS
        MXIMHS = IMENHS
        MXTOTHS= TOTENHS
      ELSEIF(IOPT .EQ. 2) THEN
        MXEMBN = EMETBN
        MXHDBN = HDETBN
        MXIMBN = IMETBN
        MXTOTBN= TOTETBN
        MXEMHS = EMETHS
        MXHDHS = HDETHS
        MXIMHS = IMETHS
        MXTOTHS= TOTETHS
      ENDIF
C---
  999 RETURN
      END
