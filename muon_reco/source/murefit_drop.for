      SUBROUTINE MUREFIT_DROP(IMODE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drop approriate banks to allow refitting
C-                         of raw muon hits.  If IMODE=1 or 3, hits on tracks
C-                         banks are shunted from MUOT to MUON to allow
C-                         subsequent rematching of the MUON and MUOT
C-                         banks
C-
C-   Inputs  : IMODE = 1 for wholesale mureco dropping, >3 for 
C-             saving and shunting MUON, etc., 2 for dropping PMUO only,
C-             0 for nothing dropped 
C-   Outputs : IERR = error code (0 = OK)
C-   Controls: 
C-
C-   Created   2-MAR-1993   Darien R. Wood
C-   Modified 14-JUL-1993   DRW, drop PMUO banks (only) for IMODE=2 
C-   Modified 27-JUL-1993   DRW, fix dropping for IMODE=2, drop nothing
C-                          for IMODE=0
C-   Modified 21-JUN-1995   RE Hall; keep also MUOT under MRFT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMFIT.LINK'
      INCLUDE 'D0$LINKS:IZMRFT.LINK'
      INTEGER LPMUO,LMUOT,LMUON,LMHTT,LSTTH,NS,LMTRH,LMUHT
      INTEGER LPARH,LMFIT,IERR,IMODE,LPMUO_NEXT,LMRFT
      INTEGER NDMTRH,NPUSH
      INTEGER  GZPMUO,GZMUOT,GZMUHT,GZMTRH,GZPARH,GZMUON
      EXTERNAL GZPMUO,GZMUOT,GZMUHT,GZMTRH,GZPARH,GZMUON
C----------------------------------------------------------------------
      IERR = 0
      IF(IMODE.EQ.3 .OR. IMODE.EQ.2) THEN
C loop over muon banks and add 3 more links (possible garbage collect each
C time)
        LMUON = GZMUON(0)
        DO WHILE(LMUON.GT.0)
          CALL BKMRFT(LMUON,0,LMRFT)
          LMUON = LQ(LMUON)
        ENDDO
      ENDIF
      IF(IMODE.EQ.3) THEN
C loop over PMUO banks and shunt MHTT banks to MUON
        LPMUO = GZPMUO(0)
        DO WHILE(LPMUO.GT.0)
          NS = IQ(LPMUO-2)
          LMUOT = LQ(LPMUO-NS-1)
          LMUON = LQ(LPMUO-NS-2)
          IF(LMUOT.GT.0 .AND. LMUON.GT.0) THEN
            LMRFT = LQ(LMUON-IZMRFT)
            IF(LMRFT.GT.0) THEN
C keep also MUOT as link 4
              IF(LQ(LMRFT-4).GT.0) THEN
                CALL MZDROP(IXMAIN,LQ(LMRFT-4),' ')
              ENDIF  
              CALL ZSHUNT(IXMAIN,LMUOT,LMRFT,-4,0)
C
              LMHTT = LQ(LMUOT-1)
              IF(LMHTT.GT.0) THEN
                IF(LQ(LMRFT-1).GT.0) THEN
                  CALL MZDROP(IXMAIN,LQ(LMRFT-1),' ')
                ENDIF  
                CALL ZSHUNT(IXMAIN,LMHTT,LMRFT,-1,0)
              ENDIF
              LSTTH = LQ(LMUOT-2)
              IF(LSTTH.GT.0) THEN
                IF(LQ(LMRFT-2).GT.0) THEN
                  CALL MZDROP(IXMAIN,LQ(LMRFT-2),' ')
                ENDIF  
                CALL ZSHUNT(IXMAIN,LSTTH,LMRFT,-2,0)
              ENDIF
            ENDIF
C clear the reference link in MUON
            LQ(LMUON-11) = 0
C drop the MFIT bank
            LMFIT = LQ(LMUON-IZMFIT)
            CALL MZDROP(IXMAIN,LMFIT,' ')
          ENDIF  
          LPMUO = LQ(LPMUO)
        ENDDO  
C drop all MUOT banks
        LMUOT = GZMUOT(0)
        IF(LMUOT.GT.0) THEN
          CALL MZDROP(IXMAIN,LMUOT,'L')
        ENDIF  
C set the number of MUOT tracks to zero
        LMTRH = GZMTRH(0)
        IF(LMTRH.GT.0) THEN
          IQ(LMTRH+1) = 0
          NDMTRH = IQ(LMTRH-1)
C save the level 1.5 words from MTRH in first MRFT bank
          IF(NDMTRH.GE.10) THEN
            LMUON = GZMUON(0)
            IF(LMUON.GT.0) THEN
              LMRFT = LQ(LMUON-IZMRFT)
              IF(LMRFT.GT.0) THEN
                CALL UCOPY(IQ(LMTRH+8),IQ(LMRFT+1),3)
              ENDIF
            ENDIF
          ENDIF
C push MTRH bank if necessary (bigger in V12.15 onward)
          NPUSH = 15 - NDMTRH
          IF(NPUSH.GT.0) THEN
            CALL MZPUSH(IXMAIN,LMTRH,0,NPUSH,' ')
          ENDIF
        ENDIF
      ENDIF
C The new MUOT banks can be matched with the old MUON banks
C with MUREFIT_MATCH
C
C
      IF(IMODE.GT.0) THEN
C drop all PMUO banks
        LPMUO = GZPMUO(0)
        IF(IMODE.EQ.2 .OR. IMODE.EQ.3) THEN
          DO WHILE(LPMUO.GT.0)
            LPMUO_NEXT = LQ(LPMUO)
C hang pmuo copies below muon banks
            NS = IQ(LPMUO-2)
            LMUON = LQ(LPMUO-NS-2)
            IF(LMUON.GT.0) THEN
              LMRFT = LQ(LMUON-IZMRFT)
              IF(LMRFT.GT.0) THEN
                CALL ZSHUNT(IXMAIN,LPMUO,LMRFT,-3,0)
              ENDIF
            ENDIF
            LPMUO = LPMUO_NEXT
          ENDDO
        ELSE
          IF(LPMUO.GT.0) THEN
            CALL MZDROP(IXMAIN,LPMUO,'L')
          ENDIF
        ENDIF  
C set the number of PMUO banks to zero
        LPARH = GZPARH()
        IF(LPARH.GT.0) THEN
          IQ(LPARH+2) = 0
        ENDIF  
      ENDIF  
C
      IF(IMODE.EQ.1) THEN
C starting over from scratch, bombs away
        LMTRH = GZMTRH(0)
        IF(LMTRH.GT.0) THEN
          CALL MZDROP(IXMAIN,LMTRH,' ')
        ENDIF
      ENDIF  
C
C drop processed hits
      IF(IMODE.EQ.1 .OR. IMODE.EQ.3) THEN
        LMUHT = GZMUHT()
        IF(LMUHT.GT.0) THEN
          CALL MZDROP(IXMAIN,LMUHT,' ')
        ENDIF
      ENDIF  
C now we should be ready to rerun MUANLZ.  
C  
  999 RETURN
      END                                       
