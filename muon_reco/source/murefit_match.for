      SUBROUTINE MUREFIT_MATCH(IERR,NTMATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : match refit MUOT tracks to exhisting MUON
C-                         banks.  The old MUOT banks need to have been
C-                         dropped with MUREFIT_DROP so that the hit
C-                         information is preserved.
C-
C-   Inputs  : 
C-   Outputs : NTMATCH = number of MUON-MUOT matches made
C-   Controls: 
C-
C-   Created   5-MAR-1993   Darien R. Wood
C-   Modified  6-Dec-1993   DRW  Exclude A-layer stubs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMRFT.LINK'
      INTEGER LMUON,LMUOT,LMHTT1,LMHTT2,LPMUO
      INTEGER NSMUON,NSPMUO,IADD1,IADD2,NMHTT1,NMHTT2,LMRFT
      INTEGER NHMATCH,NTMATCH,IH1,IH2,IERR,LMUON_NEXT,IMUON
      INTEGER LMTRH
      INTEGER LMUOT_OLD
      INTEGER  GZMUON,GZMUOT,GZPMUO,GZMTRH,GZMRFT
      EXTERNAL GZMUON,GZMUOT,GZPMUO,GZMTRH,GZMRFT
C----------------------------------------------------------------------
C first, restore Level 1.5 words to MTRH
      LMTRH = GZMTRH()
      LMUON = GZMUON(0)
      LMRFT = 0
      IF(LMUON.GT.0) LMRFT = LQ(LMUON-IZMRFT)
      IF(LMTRH.GT.0 .AND. LMRFT.GT.0) THEN
        CALL UCOPY(IQ(LMRFT+1),IQ(LMTRH+8),3)
      ENDIF
C now do matching
      NTMATCH = 0
      IERR = 0
C Loop over MUON banks
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        LMHTT1 = LQ(LMUON-15)
        NSMUON = IQ(LMUON-2)
        LMRFT = LQ(LMUON-IZMRFT)
        IF(LMRFT.GT.0) THEN
          LMHTT1 = LQ(LMRFT-1)
          IF(LMHTT1.GT.0) THEN
            NMHTT1 = IQ(LMHTT1-1)/5
            IF(NMHTT1.GT.0) THEN
C loop over MUOT banks, and look for a match
              LMUOT = GZMUOT(0)
              DO WHILE(LMUOT.GT.0)
C exclude a-layer stubs and bad ifw4's
                IF(IQ(LMUOT+4).NE.5 .AND. IQ(LMUOT+7).LE.2) THEN
                  LMHTT2 = LQ(LMUOT-1)
                  IF(LMHTT2.GT.0) THEN
                    NMHTT2 = IQ(LMHTT2-1)/5
                    IF(NMHTT2.GT.0) THEN
C count the number of matching hit addresses
                      NHMATCH = 0
                      DO IH1=1,NMHTT1
                        IADD1 = IQ(LMHTT1+5*(IH1-1)+1)
                        DO IH2=1,NMHTT2
                          IADD2 = IQ(LMHTT2+5*(IH2-1)+1)
                          IF(IADD1.EQ.IADD2) NHMATCH = NHMATCH + 1
                        ENDDO
                      ENDDO
C call it matched if half or more of the hits are the same
                      IF(NHMATCH.GE.NMHTT1/2) THEN
                        LQ(LMUON-NSMUON-1) = LMUOT
                        NTMATCH = NTMATCH + 1
C transplant IFW3 bits 16-19 from old muot to new 
                        LMUOT_OLD=LQ(LMRFT-4)
                        IF(LMUOT_OLD.GT.0) THEN
                          CALL MVBITS(IQ(LMUOT_OLD+6),16,4,             
     &                                IQ(LMUOT+6),16)
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF  
                ENDIF  
                LMUOT = LQ(LMUOT)
              ENDDO  
            ENDIF
          ENDIF
        ENDIF  
        LMUON = LQ(LMUON)
      ENDDO  
C clean up MUON banks
C      LMUON = GZMUON(0)
C      DO WHILE(LMUON.GT.0)
C        LMHTT1 = LQ(LMUON-14)
C        IF(LMHTT1.GT.0) THEN
C          CALL MZDROP(IXMAIN,LMHTT1,' ')
C        ENDIF  
C        LSTTH1 = LQ(LMUON-15)
C        IF(LSTTH1.GT.0) THEN
C          CALL MZDROP(IXMAIN,LSTTH1,' ')
C        ENDIF  
C        LMUON = LQ(LMUON)
C      ENDDO
C drop unmatch MUON banks and make the bank id numbers sequential
      IMUON = 0
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        LMUON_NEXT = LQ(LMUON)
        IF(LQ(LMUON-NSMUON-1).LE.0) THEN
          CALL MZDROP(IXMAIN,LMUON,' ')
        ELSE
          IMUON = IMUON + 1
          IQ(LMUON-5) = IMUON
        ENDIF
        LMUON = LMUON_NEXT
      ENDDO  
C
C fix up links in PMUO banks
      LPMUO = GZPMUO(0)
      DO WHILE(LPMUO.GT.0)
        NSPMUO = IQ(LPMUO-2)
        LMUON = LQ(LPMUO-NSPMUO-2)
        IF(LMUON.GT.0) THEN
          LMUOT = LQ(LMUON-NSMUON-1)
          IF(LMUOT.GT.0) THEN
            LQ(LPMUO-NSPMUO-1) = LMUOT
          ELSE
            LQ(LPMUO-NSPMUO-1) = 0
          ENDIF
        ENDIF  
        LPMUO = LQ(LPMUO)
      ENDDO  
C
C now we can redo the global fit
C
  999 RETURN
      END
