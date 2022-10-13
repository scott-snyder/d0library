
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
C-   Modified  17-Jan-1995   DRW  include matching of stubs to stubs,
C-                                recover trigger information from old MUOTs,
C-                                re-use old SAMUS and Overlap MUOTs
C-   Updated  28-AUG-1995   Andrei Mayorov  re_use only old OVERLAP MUOT, but
C-                                          not SAMUS
C-   Updated   7-SEP-1995   Andrei Mayorov   SAREFIT option
C-   Updated   28-SEP-1995  DRW save all overlap,a-stub,and unmatched WAMUS
C-                          muons with old MUOTs, set ifw2 bit 21 for new
C-                          MUOTs, bit 23 for unrefittable MUOTs
C-                          Look for old MHTT and STTH banks under old
C-                          MUOT banks instead of directly under MRFT
C-   Updated   30-SEP-1995  DRW drop old MSEG banks when new MUOT is matched
C-                              and drop MUOTs without MUON banks
C-   Updated  27-NOV-1995   Andrei Mayorov  check if there is reference on old
C-                                          samus track  (nold)
C
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMRFT_MUON.LINK'
      INCLUDE 'D0$LINKS:IZMSEG.LINK'
      INTEGER LMUON,LMUOT,LMHTT1,LMHTT2,LPMUO
      INTEGER NSMUON,NSPMUO,IADD1,IADD2,NMHTT1,NMHTT2,LMRFT
      INTEGER NHMATCH,NTMATCH,IH1,IH2,IERR,LMUON_NEXT,IMUON
      INTEGER LMTRH,LMSEG,LMUOT_NEXT,NMUON
      INTEGER LMUOT_OLD,LMUOT_NEW,IFW1_OLD,IFW3_OLD,IFW3_NEW
      INTEGER NPSAM_OLD,sarefit,IQUAD
      LOGICAL OK,MATCHED,FIRST
      DATA FIRST/.TRUE./
      INTEGER  GZMUON,GZMUOT,GZPMUO,GZMTRH,GZMRFT
      INTEGER lstth1,lstth2,nhits1,nhits2,ier
      EXTERNAL GZMUON,GZMUOT,GZPMUO,GZMTRH,GZMRFT
      INTEGER lsatr_old,lsatr,nnew,nold,gzsatn,gzsats,i,nhits
      EXTERNAL gzsatn,gzsats
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('SAREFIT',SAREFIT,IER)
        IF(ier.NE.0) sarefit=1
        CALL EZRSET
      END IF
C first, restore Level 1.5 words to MTRH
      LMTRH = GZMTRH(0)
      LMUON = GZMUON(0)
      LMRFT = 0
      IF(LMUON.GT.0) THEN
        LMRFT = LQ(LMUON-IZMRFT_MUON)
      END IF
      IF(LMTRH.GT.0 .AND. LMRFT.GT.0) THEN
        CALL UCOPY(IQ(LMRFT+1),IQ(LMTRH+8),3)
      ENDIF
C set "new MUOT" IFW2 bit in the new MUOT banks
      LMUOT=GZMUOT(0)
      DO WHILE(LMUOT.GT.0)
        IQ(LMUOT+5) = IBSET(IQ(LMUOT+5),21)
        LMUOT=LQ(LMUOT)
      ENDDO
C now do matching
      NTMATCH = 0
      IERR = 0
C Loop over MUON banks
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        NSMUON = IQ(LMUON-2)
        LMRFT = LQ(LMUON-IZMRFT_MUON)
        IF(LMRFT.GT.0) THEN
          LMUOT_OLD = LQ(LMRFT-4)
          IF(LMUOT_OLD.GT.0) THEN
            IFW1_OLD = IQ(LMUOT_OLD+4)
            NPSAM_OLD = IQ(LMUOT_OLD+2)
          ELSE
            IFW1_OLD = 0
            NPSAM_OLD = 0
          ENDIF
          IF (iq(lmuon+9).eq.13.or.iq(lmuon+9).eq.14) then ! samus
            IF (SAREFIT.EQ.0) THEN         ! match tracks by STTH
              IF(lmuot.GT.0) THEN
                LsTTh1 = LQ(LMUOT_OLD-2)
                IF (lstth1.GT.0) THEN
                  nhits1=iq(lstth1-1)/8
                  lmuot=gzmuot(0)
                  DO WHILE(LMUOT.GT.0)
                    lstth2=lq(lmuot-2)
                    IF (lstth2.GT.0) THEN
                      nhits2=iq(lstth2-1)/8
                      nhmatch=0
                      DO ih1=1,nhits1
                        iadd1=iq(lstth1+(ih1-1)*8+1)
                        DO ih2=1,nhits2
                          IF(iadd1.EQ.iq(lstth2+(ih2-1)*8+1))
     &                      nhmatch=nhmatch+1
                        ENDDO
                      END DO
                      IF(NHMATCH.GE.Nhits1/2) THEN
                        LQ(LMUON-NSMUON-1) = LMUOT
                        NTMATCH = NTMATCH + 1
                        go to 100
                      ENDIF
                    END IF
                    LMUOT = LQ(LMUOT)
                  END DO
                END IF
              ELSE
                CALL errmsg('MUOT bank is missing','MUREFIT_MATCH',' ',
     &            'W')
              ENDIF
            ELSE                              ! shunt SAMUS banks back
              LMRFT = LQ(LMUON-IZMRFT_MUON)
              LMUOT_OLD = LQ(LMRFT-4)
C              lSTTH1=lq(LMRFT-2)
              NTMATCH = NTMATCH + 1
              CALL ZSHUNT(IXMAIN,LMUOT_OLD,LMTRH,-1,0)
C              CALL zshunt(ixmain,lstth1,lmuot_old,-2,0)
              LQ(LMUON-NSMUON-1) = LMUOT_OLD
              NTMATCH = NTMATCH + 1
              lmtrh=gzmtrh(0)
              IF(lmtrh.GT.0) iq(lmtrh+1)=iq(lmtrh+1)+1 ! restore number of found
                                                       ! tracks
            END IF
          ELSE
C now do non-SAMUS (WAMUS & overlap)
            LMHTT1 = 0
            IF(LMUOT_OLD.GT.0) LMHTT1 = LQ(LMUOT_OLD-1)
            IF(LMHTT1.GT.0) THEN
              NMHTT1 = IQ(LMHTT1-1)/5
              IF(NMHTT1.GT.0) THEN
C loop over MUOT banks, and look for a match
                LMUOT = GZMUOT(0)
                DO WHILE(LMUOT.GT.0)
C exclude bad ifw4's (new MUOT), a-layer stubs (old and new)
C and overlap tracks (old MUOT)
                  OK = .TRUE.
                  IF(MOD(IQ(LMUOT+4),10).EQ.5) THEN
                    OK = .FALSE.
                  ENDIF
                  IF(MOD(IFW1_OLD,10).EQ.5) THEN
                    OK = .FALSE.
                  ENDIF
                  IF(NPSAM_OLD.GE.1) THEN
                    OK = .FALSE.
                  ENDIF
                  IF(IQ(LMUOT+7).GT.2) OK = .FALSE.
                  IF(OK) THEN
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
C drop any old MSEG banks so that they will be updated from new MUOT
                          LMSEG = LQ(LMUON-IZMSEG)
                          IF(LMSEG.GT.0) THEN
                            CALL MZDROP(IXMAIN,LMSEG,'L')
                          ENDIF
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDIF
                  LMUOT = LQ(LMUOT)
                ENDDO
              ENDIF
            end if
          ENDIF
        ENDIF
  100   LMUON = LQ(LMUON)
      ENDDO
C drop unmatch MUON banks and make the bank id numbers sequential
      IMUON = 0
      LMUON = GZMUON(0)
      LMTRH = GZMTRH(0)
      DO WHILE(LMUON.GT.0)
        LMUON_NEXT = LQ(LMUON)
        MATCHED = .FALSE.
        LMUOT_NEW = LQ(LMUON-NSMUON-1)
        IF(LMUOT_NEW.GT.0) THEN
C matched muon, use new results
          MATCHED = .TRUE.
          LMRFT = LQ(LMUON-IZMRFT_MUON)
          IF(LMRFT.GT.0) THEN
            LMUOT_OLD = LQ(LMRFT-4)
            IF(LMUOT_OLD.GT.0) THEN
C record trigger information from old MUOT into new muot (IFW3 bits 16-19)
              IFW3_OLD = IQ(LMUOT_OLD+6)
              IFW3_NEW = IQ(LMUOT_NEW+6)
              CALL MVBITS(IFW3_OLD,16,4,IFW3_NEW,16)
              IQ(LMUOT_NEW+6) = IFW3_NEW
            ENDIF
          ENDIF
        ELSE
C check to see if original MUOT is available for unmatched MUONs
          LMRFT = LQ(LMUON-IZMRFT_MUON)
          IF(LMRFT.GT.0) THEN
            LMUOT_OLD = LQ(LMRFT-4)
            IF(LMUOT_OLD.GT.0) THEN
C use old MUOT as new MUOT, and call it a match
              MATCHED = .TRUE.
              CALL ZSHUNT(IXMAIN,LMUOT_OLD,LMTRH,-1,0)
              LQ(LMUON-NSMUON-1) = LMUOT_OLD
              lmtrh=gzmtrh(0)
              IQUAD = IQ(LMUOT_OLD+3)
              IF(IQUAD.EQ.13 .OR. IQUAD.EQ.14) THEN
                if(SAREFIT.ne.1) then
                  if (IQUAD.eq.13) then ! refill from old SATN/S
                    lsatr_old=lq(lq(lmtrh-5)-1)
                    lsatr=gzsatn()
                  else if(iq(lmuot_old+3).eq.14) then
                    lsatr_old=lq(lq(lmtrh-5)-2)
                    lsatr=gzsats()
                  ENDIF
                  nnew=iq(lsatr+1)
                  nold=ibits(iq(lmuot_old+6),12,3)-1
                  if(nold.ge.0) then
                    iq(lsatr+150*nnew+2)=iq(lsatr_old+150*nold+2)
                    do i=2,16
                      q(lsatr+150*nnew+1+i)=q(lsatr_old+150*nold+1+i)
                    end do
                    iq(lsatr+150*nnew+18)=iq(lsatr_old+150*nold+18)
                    iq(lsatr+150*nnew+19)=iq(lsatr_old+150*nold+19)
                    do i=19,25
                      q(lsatr+150*nnew+1+i)=q(lsatr_old+150*nold+1+i)
                    end do
                    do i=26,30
                      iq(lsatr+150*nnew+1+i)=iq(lsatr_old+150*nold+1+i)
                    end do
                    nhits=iq(lsatr+150*nnew+1+30)
                    do i=31,31+nhits*3
                      iq(lsatr+150*nnew+1+i)=iq(lsatr_old+150*nold+1+i)
                    end do
                    iq(lsatr+1)=iq(lsatr+1)+1
                    CALL MVBITS(iq(lsatr+1),0,4,iq(lmuot_old+6),12)
                  ENDIF
                end if
              ENDIF
C if the muon has no MSEG bank, flag it as "unrefittable" (ifw2 bit 23)
              IF(LQ(LMUON-8).LE.0) THEN
                IQ(LMUOT_OLD+5) = IBSET(IQ(LMUOT_OLD+5),23)
              ENDIF
              NTMATCH = NTMATCH + 1
            ENDIF
          ENDIF
        ENDIF
        IF(MATCHED) THEN
          IMUON = IMUON + 1
          IQ(LMUON-5) = IMUON
        ELSE
          CALL MZDROP(IXMAIN,LMUON,' ')
        ENDIF
        LMUON = LMUON_NEXT
      ENDDO
C
C get rid of MUOTs with no MUON cross links and clean up numbering and
C ordering
      LMUOT = GZMUOT(0)
      DO WHILE(LMUOT.GT.0)
        IQ(LMUOT-5) = -1
        LMUOT = LQ(LMUOT)
      ENDDO
      NMUON = 0
      LMUON = GZMUON(0)
      DO WHILE(LMUON.GT.0)
        NMUON = NMUON + 1
        LMUOT = LQ(LMUON-11)
        IQ(LMUOT-5) = IQ(LMUON-5)
        LMUON = LQ(LMUON)
      ENDDO
      LMUOT = GZMUOT(0)
      DO WHILE(LMUOT.GT.0)
        LMUOT_NEXT = LQ(LMUOT)
        IF(IQ(LMUOT-5).EQ.-1) THEN
          CALL MZDROP(IXMAIN,LMUOT,' ')
        ENDIF
        LMUOT = LMUOT_NEXT
      ENDDO
      LMUOT = GZMUOT(0)
      CALL ZSORTI(IXMAIN,LMUOT,-5)
      LMTRH = GZMTRH(0)
      IQ(LMTRH+1) = NMUON
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
