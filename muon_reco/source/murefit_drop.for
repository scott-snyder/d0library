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
C-   Updated  23-AUG-1995   Andrei Mayorov  keep SAMT under MRFT
C-   Updated  31-AUG-1995   Andrei Mayorov  refill STTH and restore reffer. on
C-                                          SATN/S  if necessary
C-   Updated  28-SEP-1995   DRW, store old MHTT and STTH banks under old
C-                               MUOT instead of directly under MRFT
C-   Updated   5-DEC-1995   Daria Zieminska  use MZREPL instead of MZPUSH
C                            (followed FIX_VERH example)
C    Updated   6-DEC-1995   DRW, only book new MTRH bank when size is wrong,
C                                copy links from old MTRH
C-   Fixed the call to GZMUHT (1 argument)                     
C-   Updated  20-JAN-1996   RE Hall; drop MTOF banks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZMFIT.LINK'
      INCLUDE 'D0$LINKS:IZMTOF.LINK'
      INCLUDE 'D0$LINKS:IZMRFT_MUON.LINK'
      INTEGER LPMUO,LMUOT,LMUON,LMTRH,LMUHT,ns
C      INTEGER LMHTT,LSTTH
      INTEGER LMTOF
      INTEGER LPARH,LMFIT,IERR,IMODE,LPMUO_NEXT,LMRFT
      INTEGER NDMTRH,NLMTRH
      INTEGER LTMP,LNEW,NL,ND,MMBK(5)
      INTEGER  GZPMUO,GZMUOT,GZMUHT,GZMTRH,GZPARH,GZMUON,gzsatn,gzsats
      EXTERNAL GZPMUO,GZMUOT,GZMUHT,GZMTRH,GZPARH,GZMUON,gzsatn,gzsats
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
         CALL UCTOH('MTRH',MMBK(1),4,4)   ! IDH (hollerith bank name)
         NL=5
         MMBK(2)=NL                       ! NL (total number of links)
         MMBK(3)=NL                       ! NS (number of struct. links)
         ND=15
         MMBK(4)=ND                       ! ND (number of data words)
         CALL MZFORM('MTRH','13I 2F',MMBK(5))  ! NIO (bank format)
      END IF
      IERR = 0
      IF(IMODE.EQ.3 .OR. IMODE.EQ.2) THEN
        LMTRH = GZMTRH(0)
        IF(LMTRH.GT.0) THEN
          NDMTRH = IQ(LMTRH-1) ! number of data words in old MTRH
          NLMTRH = IQ(LMTRH-2)
C add new links for MRFT, SAMT and data words if necessary
          IF(NDMTRH.NE.ND .OR. NLMTRH.NE.NL) THEN
C
C  Define I/O characteristics of MTRH using MZREPL
C  Book a temporary MTRH bank correctly and replace old MTRH bank
C
            CALL MZBOOK(IXMAIN,LTMP,LTMP,1,'MTMP',2,2,1,0,-1)
C
            LQ(LTMP-1) = LMTRH                           ! Old MTRH bank
C
            CALL MZLIFT(IXMAIN,LNEW,LTMP,-2,MMBK,0)      ! New MTRH bank
C
C copy from old to new
C
            CALL UCOPY(IQ(LMTRH+1),IQ(LNEW+1),NDMTRH)           ! data
            CALL UCOPY(LQ(LMTRH-3),LQ(LNEW-3),NLMTRH)           ! links
C
            CALL MZREPL(IXMAIN,LTMP,' ')                   ! replace
          END IF
        END IF
c
        LMUON = GZMUON(0)
        DO WHILE(LMUON.GT.0)
          CALL BKMRFT(0,LMUON,0,LMRFT)
          IF(iq(lmuon+9).eq.13.or.iq(lmuon+9).eq.14) then ! SAMUS muon
            ns=iq(lmuon-2)
            lmuot=lq(lmuon-ns-1)
            IF (lmuot.GT.0) THEN
              IF(lq(lmuot-2).le.0) then ! no STTH bank - refill
                CALL stthrf(lmuot)
              END IF
            END IF
          ENDIF
C drop current MTOF banks- they will be redone
          LMTOF = LQ(LMUON-IZMTOF)
            IF(LMTOF.GT.0) THEN
              CALL MZDROP(IXMAIN,LMTOF,'L')
            END IF
          LMUON = LQ(LMUON)
        ENDDO
      END IF
      IF(IMODE.EQ.3) THEN
C loop over PMUO banks and shunt MHTT banks to MUON
        LPMUO = GZPMUO(0)
        DO WHILE(LPMUO.GT.0)
          NS = IQ(LPMUO-2)
          LMUOT = LQ(LPMUO-NS-1)
          LMUON = LQ(LPMUO-NS-2)
          IF(LMUOT.GT.0 .AND. LMUON.GT.0) THEN
            LMRFT = LQ(LMUON-IZMRFT_muon)
            IF(LMRFT.GT.0) THEN
C keep also MUOT as link 4
              IF(LQ(LMRFT-4).GT.0) THEN
                CALL MZDROP(IXMAIN,LQ(LMRFT-4),' ')
              ENDIF
              CALL ZSHUNT(IXMAIN,LMUOT,LMRFT,-4,0)
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
C save the level 1.5 words from MTRH in first MRFT bank
          IF(NDMTRH.GE.10) THEN
            LMUON = GZMUON(0)
            IF(LMUON.GT.0) THEN
              LMRFT = LQ(LMUON-IZMRFT_muon)
              IF(LMRFT.GT.0) THEN
                CALL UCOPY(IQ(LMTRH+8),IQ(LMRFT+1),3)
              ENDIF
            ENDIF
          ENDIF
C push MTRH bank if necessary (bigger in V12.15 onward)
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
              LMRFT = LQ(LMUON-IZMRFT_muon)
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
c          CALL MZDROP(IXMAIN,LMTRH,' ')
          lmuon=gzmuon(0)
          IF(lmuon.GT.0) CALL MZDROP(IXMAIN,LMuon,'L')
          lmuot=gzmuot(0)
          IF(lmuot.GT.0) CALL MZDROP(IXMAIN,LMuot,'L')
        ENDIF
      ENDIF
C
C drop processed hits
      IF(IMODE.EQ.1 .OR. IMODE.EQ.3) THEN
        LMUHT = GZMUHT(0)
        IF(LMUHT.GT.0) THEN
          CALL MZDROP(IXMAIN,LMUHT,' ')
        ENDIF
      ENDIF
C now we should be ready to rerun MUANLZ.
C
  999 RETURN
      END
