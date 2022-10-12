      FUNCTION L2_COMPARE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : does an event by event comparison of what's
C-          in the FILT and FRES banks.  of course, it assumes that the
C-          events read in already have such banks.  this package should
C-          be run after the level2 simulation for MC
C-          L2_COMPARE is the PROCESS_EVENT hook
C-
C-   NOTE:  L2_COMPARE will be assuming that the 1st "path" will contain
C-          the original (e.g. DATA), and the 2nd will contain the 
C-          simulation
C-
C-   NOTE:  Set EVENT_PRINT to TRUE in RCP to get output for single
C-          events
C-
C-
C-   NOTE:  For the FILT bank, we compare SET bits and PASSED bits only,
C-          forgetting about comparing TRIED and UNBIASED
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-  Entry points:
C-    L2_COMPARE_INI  grand initialization
C-    L2_COMPARE_DIA  optional dialog for overriding RCP options
C-    L2_COMPARE_SUM  summary at the end
C-
C-
C-  Internal calls:
C-    L2_COMP_JETS,L2_COMP_EM, L2_COMP_MUOT, L2_COMP_PNUT, L2_COMP_RMAS
C-    L2_COMP_LETA,L2_COMP_ACOL
C-        L2_NXT_INT_DIFF, L2_NXT_REAL_DIFF, L2_COMPARE_PB32,L2_COMPARE_PB128
C-        L2_COMPARE_MARK_BIT,L2_COMPARE_FILT,L2_COMPARE_FILT_OUT
C-
C-   Created  18-MAR-1992   Drew Baden
C-   Modified 01-Jan-1994   Andrzej Zieminski--summary stream, L1 prescale
C    Modified 17-Feb-1994   Amber Boehnlein--Got TRGR MASK from correct
C                           crate
C-   Modified 08-Jun-1994   Andrzej Zieminski: L0VT and new RCP parameters
C-   Updated  16-JUN-1994   Lewis T. Goss - Added calls to RMAS,LETA, ACOL banks
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
C     thresholds for JAUX comparisons
C
      REAL JAUX_ET,JAUX_ETA,JAUX_PHI,JAUX_EMF,JAUX_ETAS,JAUX_PHIS
C
C     thresholds for L2EM comparisons
C
      REAL L2EM_ET,L2EM_SUMEM,L2EM_EM1R,L2EM_EM12R,L2EM_EM3R,L2EM_EM4R
      REAL L2EM_FH1R,L2EM_SIGMA3,L2EM_SIGMA5,L2EM_SIG3MID
      REAL L2EM_SH13,L2EM_SH24,L2EM_SH35,L2EM_SH57,L2EM_CONER
      REAL L2EM_FCONE,L2EM_DETA,L2EM_DPHI
C
C     thresholds for PNUT comparisons
C
      REAL PNUT_EX,PNUT_EY,PNUT_ET,PNUT_PHI,PNUT_ETSCALAR
C
C     thresholds for L0VT comparisons
C
      REAL L0VT_ZFAST,L0VT_ZSLOW 
C
C     thresholds for MUOT comparisons
C
      REAL MUOT_XIN,MUOT_YIN,MUOT_ZIN,MUOT_XOUT,MUOT_YOUT,MUOT_ZOUT
      REAL MUOT_XCIN,MUOT_YCIN,MUOT_ZCIN,MUOT_CHB,MUOT_CHNB
      REAL MUOT_XCOUT,MUOT_YCOUT,MUOT_ZCOUT,MUOT_BDL,MUOT_P,MUOT_DP
      REAL MUOT_ECAL,MUOT_EFE
C
C     thresholds for RMAS comparisons
C
      REAL RMAS_MASS,RMAS_ETAB,RMAS_ET,RMAS_ETA,RMAS_PHI
C
C     thresholds for LETA comparisons
C
      REAL LETA_DETA,LETA_ET,LETA_ETA,LETA_PHI
C
C     thresholds for ACOL comparisons
C
      REAL ACOL_ET,ACOL_ETA,ACOL_PHI
C
      INTEGER I,J,LUN,LUN1,IUSER,IER,NRUN,NEV,L1W,IB,NEVTS,PRTLEV
      INTEGER GZFRES,LFRES,GZFILT,LFILT,GZTRGR,LTRGR
      INTEGER GZFIND_CRATE,LTRGR_LEVEL1,NLETA,NACOL,NBACOL,NTACOL
      INTEGER IOFF,NBSET,NBTRY,NBPASS,NBUN,LEN,NRMAS,NBRMAS,NTRMAS
      INTEGER NBJAUX,NBL2EM,NBTRGR,NTJAUX,NTL2EM,NTTRGR,NTLETA,NBLETA
      INTEGER I1S(4,2),ITRY(4,2),IPASS(4,2),IUB(4,2)
      INTEGER IL1W(4),NFILT,NPNUT,NBPNUT,NTPNUT,NTMUOT
      INTEGER LPROC,LLPARH(4),LLPNUT(4),LLRMAS(2),LLLETA(2),LLACOL(2)
      INTEGER LJAUX(4),LJPAR,GZJPAR,LL2EM(4),LLFRES(4),NJAUX,NL2EM
      INTEGER LL0VT(4),NL0VT,NBL0VT,NTL0VT
      INTEGER LLMTRH(4),LMUOT(4),NMUOT,NBMUOT,LLMUOT(4)
      LOGICAL FPSET,FPTRY,FPPASS,FPUN,OKOK
      LOGICAL L2_COMPARE,L2_COMPARE_INI,L2_COMPARE_DIA,L2_COMPARE_SUM
      LOGICAL GOTIT,L2_COMPARE_FILT,L2_COMPARE_FILT_OUT,HEXIST
      LOGICAL DOFILT,DOTRGR,MATRIX,DOACOL
      LOGICAL DOFRES,DOL2JETS,DOL2EM,DOPNUT,DOMUOT,DOL0VT,DORMAS,DOLETA
C
      LOGICAL WRT_ALL,WRT_BAD,WRT_MU,PRTEMP,LLEBAD,LACBAD
      LOGICAL LEMBAD,LJTBAD,LPNBAD,LMUBAD,LVTBAD,LBAD,LEMPTY,LRMBAD
      INTEGER NEMPTY
C
      CHARACTER*128 L1M(2),LTRY(2),LPASS(2),LUB(2)
      CHARACTER*32 L1T(4),ACTION
      CHARACTER*22 TITLE
      CHARACTER*13 TMAIN
      CHARACTER*9  TRUN
C-
      DATA IUSER/690/
      DATA TMAIN/'L2_COMP_SUMM_'/
C
      L2_COMPARE = .TRUE.
C
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
      NEVTS = NEVTS + 1
      IF (PRTLEV.LT.1)
     &  WRITE(LUN,'('' =====> L2_COMPARE called for run/event '',2I7)')
     &  NRUN,NEV
C
      IF (.NOT.DOTRGR) GOTO 100
C
C     check L1 results - use TRGR bank instead of HEAD bank
C
       LTRGR=GZTRGR()  
      
      IB = 0
      DO WHILE (LTRGR.NE.0)
       LTRGR_LEVEL1 = GZFIND_CRATE( 'TRGR', LTRGR, 11 ) !getting TRGR
        IB = IB + 1
C
C       store Level 1 bits results from TRGR bank
C
        IL1W(IB) = IQ(LTRGR_LEVEL1+6)
C
C       loop over bits, see which trigger fired.  fill histos
C
        DO I=0,31
          CALL L2_COMPARE_MARK_BIT(I,IL1W(IB),L1T(IB))
          IF (MATRIX.AND.BTEST(IL1W(IB),I)) THEN
            IF (.NOT.HEXIST(90000+IB)) THEN
              CALL HBOOK2(90000+IB,'Level1 Accept Matrix',
     &          32,0.,32.,32,0.,32.,0.)
            ENDIF
            DO J=0,31
              IF (BTEST(IL1W(IB),J)) THEN
                CALL HFILL(90000+IB,I+.5,J+.5,1.)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
        LTRGR = LQ(LTRGR)
      ENDDO
C
C     compare successive TRGR banks
C
      IF (IB.LT.1) THEN
C
C       report error - should be at least one
C
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &         I3,'' TRGR BANKS!'')') NRUN,NEV,IB
      ELSE IF (IB.EQ.1) THEN
C
C       ok for testing mode or when L1SIM is not run - in that case
C       we use this for both the ORIGINAL and SIMULATION in comparing
C       FILT banks (L2 scripts)
C
        IF (PRTLEV.LT.1) THEN
          CALL L2_COMPARE_PB32(LUN,1,IB,L1T,'LEVEL 1 Filter Bits SET$')
        ENDIF
C
C       put results from ORIGINAL into SIMULATION word (not needed but...)
C
        IL1W(2) = IL1W(1)
      ELSE IF (IB.EQ.2) THEN
C
C       compare them
C
        IF (IL1W(1).NE.IL1W(2)) THEN
          NTTRGR = NTTRGR + 1
          CALL L2_COMPARE_PB32(LUN,1,IB,L1T,'LEVEL 1 Filter Bits SET$')
          NBTRGR = NBTRGR + 1
          L2_COMPARE = .FALSE.
        ELSE
          IF (PRTLEV.LT.1) THEN
            CALL L2_COMPARE_PB32(LUN,1,IB,L1T,
     &        'LEVEL 1 Filter Bits SET$')
          ENDIF
        ENDIF
      ELSE IF (IB.GT.2) THEN
C
C       more than 2?  probably this is because most people run L1SIM when
C       making MC raw data....just ignore the 1st bank in the chain.
C
        IF (PRTLEV.LT.1) THEN
          CALL L2_COMPARE_PB32(LUN,1,IB,L1T,'LEVEL 1 Filter Bits SET$')
        ELSE
          IF (IL1W(2).NE.IL1W(3))
     &      CALL L2_COMPARE_PB32(LUN,2,IB,L1T,
     &      'LEVEL 1 Filter Bits SET$')
        ENDIF
      ENDIF
  100 CONTINUE
C
C     loop over FILT banks, fill buffers ==> here we test L2 scripts
C
      IF (.NOT.DOFILT) GOTO 200
      LFILT = GZFILT()
      IB = 0
      DO WHILE (LFILT.NE.0)
        IB = IB + 1
C
C       store L1bits
C
        DO I=0,3
          I1S(I+1,IB) = IQ(LFILT+2+I)
          ITRY(I+1,IB) = IQ(LFILT+6+I)
          IPASS(I+1,IB) = IQ(LFILT+10+I)
          IUB(I+1,IB) = IQ(LFILT+14+I)
        ENDDO
C
C       store as characters if requested
C
        DO I=0,127
          IOFF = INT(I/32)
          CALL L2_COMPARE_MARK_BIT(I,IQ(LFILT+2+IOFF),L1M(IB))
          CALL L2_COMPARE_MARK_BIT(I,IQ(LFILT+6+IOFF),LTRY(IB))
          CALL L2_COMPARE_MARK_BIT(I,IQ(LFILT+10+IOFF),LPASS(IB))
          CALL L2_COMPARE_MARK_BIT(I,IQ(LFILT+14+IOFF),LUB(IB))
        ENDDO
C
C       store FRES pointer
C
        LLFRES(IB) = LQ(LFILT-4)
C
C       store PARH pointer
C
        LPROC = LQ(LFILT-2)
        LLPARH(IB) = LQ(LPROC-5)
        LLMTRH(IB) = LQ(LPROC-3)
C
C       next FILT
C
        LFILT = LQ(LFILT)
      ENDDO
C
      NFILT = IB
C
C     make sure there were only 2 banks BEFORE checking
C
      IF (NFILT.LT.1) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &    I3,'' FILT BANKS!'')') NRUN,NEV,NFILT
      ELSE IF (NFILT.EQ.1) THEN
C
C       ok for testing mode
C
        IF (PRTLEV.LT.1) THEN
          CALL L2_COMPARE_PB128(LUN,1,NFILT,L1M,'Filter Bits SET$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LTRY,'Filter Bits TRIED$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LPASS,'Filter Bits PASSED$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LUB,'Filter Bits UNBIASED$')
        ENDIF
      ELSE IF (NFILT.EQ.2) THEN
C
C       check for inconsistencies within the FILT banks
C       report if anything mysterious is found
C
        IF (.NOT.L2_COMPARE_FILT(LUN,PRTLEV,I1S,ITRY,IPASS)) THEN
          L2_COMPARE = .FALSE.
          WRITE(LUN,'(/,1X,70(''=''),/,
     &      '' L2_COMPARE: DISCREPANCY IN Run/Event '',2I7)')
     &      NRUN,NEV
          IF (PRTLEV.LT.2) THEN
            CALL L2_COMPARE_PB128(LUN,1,NFILT,L1M,'Filter Bits SET$')
            CALL L2_COMPARE_PB128(LUN,1,NFILT,LTRY,'Filter Bits TRIED$')
            CALL L2_COMPARE_PB128(LUN,1,NFILT,LPASS,
     &              'Filter Bits PASSED$')
          ENDIF
        ELSE
C
C         print buffers if requested
C
          IF (PRTLEV.LT.1) THEN
            CALL PRFILT(LUN,0,0,'ALL',0)
            CALL L2_COMPARE_PB128(LUN,1,NFILT,L1M,'Filter Bits SET$')
            CALL L2_COMPARE_PB128(LUN,1,NFILT,LTRY,'Filter Bits TRIED$')
            CALL L2_COMPARE_PB128(LUN,1,NFILT,LPASS,
     &        'Filter Bits PASSED$')
            CALL L2_COMPARE_PB128(LUN,1,NFILT,LUB,
     &        'Filter Bits UNBIASED$')
          ENDIF
        ENDIF
      ELSE IF (NFILT.GT.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &    I3,'' FILT BANKS!'')') NRUN,NEV,NFILT
        IF (PRTLEV.LT.2) THEN
          CALL L2_COMPARE_PB128(LUN,1,NFILT,L1M,'Filter Bits SET$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LTRY,'Filter Bits TRIED$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LPASS,'Filter Bits PASSED$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LUB,'Filter Bits UNBIASED$')
        ENDIF
      ENDIF
  200 CONTINUE
C
      IF (.NOT.DOFRES) GOTO 300
C
C     deal with FRES bank now
C
      NJAUX = 0
      NL2EM = 0
      NL0VT = 0
      LEMPTY = .TRUE.
      LEMBAD = .FALSE.
      LMUBAD = .FALSE.
      LJTBAD = .FALSE.
      LPNBAD = .FALSE.
      LVTBAD = .FALSE.
      LRMBAD = .FALSE.
      LLEBAD = .FALSE.
      LACBAD = .FALSE.
C
      IB = 0
      LJPAR = GZJPAR()
      DO I=1,NFILT
        LFRES = LLFRES(I)
        IF (LFRES.GT.0) THEN
          IB = IB + 1
          IF (IB.GT.4) THEN
            WRITE(LUN,'('' Run/Event '',2I7,
     &        '' has more than 4 FRES banks! '')') NRUN,NEV
            GOTO 300
          ENDIF
C
C         look at the links.  don't bother with the "last tool run" part
C         of LRES - for this to make sense you really need a HEAD bank for
C         each of the runs, but HEAD banks cannot be chained so no dice.
C         NOTE:  JPAR lives in STP, so we only expect to find one of these
C                if there are NONE, then there's something wrong....
C
          IF (LQ(LFRES-1).GT.0) THEN
            NJAUX = NJAUX + 1
            LJAUX(NJAUX) = LQ(LFRES-1)
          ENDIF
          IF (LQ(LFRES-4).GT.0) THEN
            NL2EM = NL2EM + 1
            LL2EM(NL2EM) = LQ(LFRES-4)
          ENDIF
          IF (LQ(LFRES-9).GT.0) THEN
            NL0VT = NL0VT + 1
            LL0VT(NL0VT) = LQ(LFRES-9)
          ENDIF
        ENDIF
      ENDDO
C
C     printout?
C
      IF (PRTLEV.LT.1) THEN
        IF (DOL2JETS) THEN
          CALL PRJPAR(LUN,LJPAR,0,'ALL',0)
          DO I=1,NJAUX
            CALL PRJAUX(LUN,LJAUX(I),0,'ALL',0)
          ENDDO
        ENDIF
        IF (DOL2EM) THEN
          DO I=1,NL2EM
            CALL PRL2EM(LUN,LL2EM(I),0,'ALL',0)
          ENDDO
        ENDIF
      ENDIF
C
C     now we know how many banks there are - if only 2,
C     then we know what to do.....
C
      IF (NJAUX.EQ.0) GOTO 295
      IF (DOL2JETS) THEN
        IF (NJAUX.NE.2) THEN
          WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &      I3,'' JAUX banks!'')') NRUN,NEV,NJAUX
          LJTBAD = .TRUE.
        ELSE
          NTJAUX = NTJAUX + 1
          LEMPTY = .FALSE.
          CALL L2_COMP_JETS(LUN,LJAUX,LJPAR,NJAUX,OKOK,
     &      JAUX_ET,JAUX_ETA,JAUX_PHI,JAUX_EMF,JAUX_ETAS,JAUX_PHIS)
          IF (.NOT.OKOK) THEN
            LJTBAD = .TRUE.
            NBJAUX = NBJAUX + 1
          ENDIF
        ENDIF
      ENDIF
C
 295  IF (NL2EM.EQ.0) GOTO 298
      IF (DOL2EM) THEN
        IF (NL2EM.NE.2) THEN
          WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &      I3,'' L2EM banks!'')') NRUN,NEV,NL2EM
          LEMBAD = .TRUE.
        ELSE
          LEMPTY = .FALSE.
          NTL2EM = NTL2EM + 1
          CALL L2_COMP_EM(LUN,LL2EM,NL2EM,OKOK,
     &      L2EM_ET,L2EM_SUMEM,L2EM_EM1R,L2EM_EM12R,L2EM_EM3R,L2EM_EM4R,
     &      L2EM_FH1R,L2EM_SIGMA3,L2EM_SIGMA5,L2EM_SIG3MID,L2EM_SH13,
     &      L2EM_SH24,L2EM_SH35,L2EM_SH57,L2EM_CONER,L2EM_FCONE,
     &      L2EM_DETA,L2EM_DPHI)
          IF (.NOT.OKOK) THEN
            LEMBAD = .TRUE.
            NBL2EM = NBL2EM + 1
          ENDIF
        ENDIF
      ENDIF
C
 298  IF (NL0VT.EQ.0) GOTO 300
      IF (DOL0VT) THEN
        IF (NL0VT.NE.2) THEN
          WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &      I3,'' L0VT banks!'')') NRUN,NEV,NL0VT
          LVTBAD = .TRUE.
        ELSE
          NTL0VT = NTL0VT + 1
          CALL L2_COMP_VT(LUN,LL0VT,NL0VT,OKOK,L0VT_ZFAST,L0VT_ZSLOW)
          IF (.NOT.OKOK) THEN
            LVTBAD = .TRUE.
            NBL0VT = NBL0VT + 1
          ENDIF
        ENDIF
      ENDIF
C
  300 CONTINUE
C
C     do PNUT
C
      IF (.NOT.DOPNUT) GOTO 400
      NPNUT = 0
      DO I=1,NFILT
        IF (LQ(LLPARH(I)-4).GT.0) THEN
          NPNUT = NPNUT + 1
          LLPNUT(NPNUT) = LQ(LLPARH(I)-4)
        ENDIF
      ENDDO
C
C     got the pointers, now compare them
C
      IF(NPNUT.EQ.0) GOTO 400
      IF (NPNUT.NE.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &      I3,'' FILT/PROC/PARH/PNUT banks!'')') NRUN,NEV,NPNUT
        LPNBAD = .TRUE.
      ELSE
        NTPNUT = NTPNUT + 1
        LEMPTY = .FALSE.
        CALL L2_COMP_PNUT(LUN,LLPNUT,NPNUT,OKOK,
     &    PNUT_EX,PNUT_EY,PNUT_ET,PNUT_PHI,PNUT_ETSCALAR)
        IF (.NOT.OKOK) THEN
          LPNBAD = .TRUE.
          NBPNUT = NBPNUT + 1
        ENDIF
      ENDIF
C
  400 CONTINUE
C
C     do MUOT
C
      IF (.NOT.DOMUOT) GOTO 500
      NMUOT = 0
      DO I=1,NFILT
        IF (LLMTRH(I).GT.0) THEN
          LLMUOT(I) = LQ(LLMTRH(I)-1)
          IF (LLMUOT(I).GT.0) THEN
            NMUOT = NMUOT + 1
            LMUOT(NMUOT) = LLMUOT(I)
          ENDIF
        ENDIF
      ENDDO
C
C     got the pointers, now compare them
C
      IF (NMUOT.EQ.0) GOTO 500
      IF (NMUOT.NE.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &      I3,'' FILT/PROC/MTRH/MUOT banks!'')') NRUN,NEV,NMUOT
        LMUBAD = .TRUE.
      ELSE
        NTMUOT=NTMUOT+1
        LEMPTY = .FALSE.
        CALL L2_COMP_MUOT(LUN,LMUOT,NMUOT,OKOK,
     &  MUOT_XIN,MUOT_YIN,MUOT_ZIN,MUOT_XOUT,MUOT_YOUT,MUOT_ZOUT,
     &  MUOT_XCIN,MUOT_YCIN,MUOT_ZCIN,MUOT_CHB,MUOT_CHNB,
     &  MUOT_XCOUT,MUOT_YCOUT,MUOT_ZCOUT,MUOT_BDL,MUOT_P,MUOT_DP,
     &  MUOT_ECAL,MUOT_EFE)
        IF (.NOT.OKOK) THEN
          LMUBAD = .TRUE.
          NBMUOT = NBMUOT + 1
        ENDIF
      ENDIF
C
  500 CONTINUE
C
C     do RMAS
C
      IF (.NOT.DORMAS) GOTO 600
      NRMAS = 0
      DO I=1,NFILT
        IF(LQ(LLFRES(I)-15).GT.0) THEN
          NRMAS = NRMAS + 1
          LLRMAS(NRMAS) = LQ(LLFRES(I)-15)
        ENDIF
      ENDDO
C
C     got the pointers, now compare them
C
      IF (NRMAS.EQ.0) GOTO 600
      IF (NRMAS.NE.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &    I3,'' FILT/FRES/RMAS banks!'')') NRUN,NEV,NRMAS
        LRMBAD = .TRUE.
      ELSE
        NTRMAS = NTRMAS + 1
        LEMPTY = .FALSE.
        CALL L2_COMP_RMAS(LUN,LLRMAS,NRMAS,OKOK,RMAS_MASS,RMAS_ETAB,
     &    RMAS_ET,RMAS_ETA,RMAS_PHI)
        IF (.NOT.OKOK) THEN
          LRMBAD = .TRUE.
          NBRMAS = NBRMAS + 1
        ENDIF
      END IF
C
  600 CONTINUE
C
C     do LETA
C
      IF (.NOT.DOLETA) GOTO 700
      NLETA = 0
      DO I=1,NFILT
        IF(LQ(LLFRES(I)-14).GT.0) THEN
          NLETA = NLETA + 1
          LLLETA(NLETA) = LQ(LLFRES(I)-14)
        ENDIF
      ENDDO
C
C     got the pointers, now compare them
C
      IF (NLETA.EQ.0) GOTO 700
      IF (NLETA.NE.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &    I3,'' FILT/FRES/LETA banks!'')') NRUN,NEV,NLETA
        LLEBAD = .TRUE.
      ELSE
        NTLETA = NTLETA + 1
        LEMPTY = .FALSE.
        CALL L2_COMP_LETA(LUN,LLLETA,NLETA,OKOK,LETA_DETA,LETA_ET,
     &    LETA_ETA,LETA_PHI)
        IF (.NOT.OKOK) THEN
          LLEBAD = .TRUE.
          NBLETA = NBLETA + 1
        ENDIF
      END IF
C
  700 CONTINUE
C
C     do ACOL
C
      IF (.NOT.DOACOL) GOTO 800
      NACOL = 0
      DO I=1,NFILT
        IF(LQ(LLFRES(I)-5).GT.0) THEN
          NACOL = NACOL + 1
          LLACOL(NACOL) = LQ(LLFRES(I)-5)
        ENDIF
      ENDDO
C
C     got the pointers, now compare them
C
      IF (NACOL.EQ.0) GOTO 800
      IF (NACOL.NE.2) THEN
        WRITE(LUN,'('' Run/Event '',2I7,'' had '',
     &    I3,'' FILT/FRES/ACOL banks!'')') NRUN,NEV,NACOL
        LACBAD = .TRUE.
      ELSE
        NTACOL = NTACOL + 1
        LEMPTY = .FALSE.
        CALL L2_COMP_ACOL(LUN,LLACOL,NACOL,OKOK,ACOL_ET,ACOL_ETA,
     &    ACOL_PHI)
        IF (.NOT.OKOK) THEN
          LACBAD = .TRUE.
          NBACOL = NBACOL + 1
        ENDIF
      END IF
C
  800 CONTINUE
C
C-  summary operations for a given events
C
      L2_COMPARE=.TRUE.
      IF(LEMPTY) THEN
        NEMPTY=NEMPTY+1
        IF (PRTEMP) THEN
          CALL L2_COMPARE_PB128(LUN,1,NFILT,L1M,'Filter Bits SET$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LTRY,'Filter Bits TRIED$')
          CALL L2_COMPARE_PB128(LUN,1,NFILT,LPASS,'Filter Bits PASSED$')
        ENDIF
      ENDIF
      LBAD = LVTBAD.OR.LJTBAD.OR.LEMBAD.OR.LPNBAD.OR.LMUBAD.OR.LRMBAD
     &  .OR.LLEBAD.OR.LACBAD
C
      CALL FLGSET('WRITE_STREAM_STA',.FALSE.)
      IF(WRT_ALL) THEN
        CALL FLGSET('WRITE_STREAM_STA',.TRUE.) 
        GOTO 900
      ENDIF
      IF(WRT_BAD.AND.LBAD) THEN
        CALL FLGSET('WRITE_STREAM_STA',.TRUE.) 
        GOTO 900
      ENDIF
      IF(WRT_MU.AND.LMUBAD) THEN
        CALL FLGSET('WRITE_STREAM_STA',.TRUE.) 
        GOTO 900
      ENDIF
C
 900  RETURN
C
      ENTRY L2_COMPARE_INI()
C-------------------------------------------------------------------
C
C     begin program initialization hook
C
C
      L2_COMPARE_INI = .TRUE.
      CALL ERRMAX(' ',-1,-1)
      PRTLEV = 2
      NBSET = 0
      NBTRY = 0
      NBPASS = 0
      NBUN = 0
      NBL2EM = 0
      NTL2EM = 0
      NTMUOT = 0
      NBMUOT = 0
      NBPNUT = 0
      NTPNUT = 0
      NBTRGR = 0
      NTTRGR = 0
      NBJAUX = 0
      NTJAUX = 0
      NEMPTY = 0
      NBL0VT = 0
      NTL0VT = 0
      NBRMAS = 0
      NTRMAS = 0
      NBLETA = 0
      NTLETA = 0
      NBACOL = 0
      NTACOL = 0
      DOFILT = .TRUE.
      DOTRGR = .TRUE.
      MATRIX = .TRUE.
      DOFRES = .TRUE.
      DOL2JETS = .TRUE.
      DOL2EM = .TRUE.
      DOPNUT = .TRUE.
      DOMUOT = .TRUE.
      DOL0VT = .TRUE.
      DORMAS = .TRUE.
      DOLETA = .TRUE.
      DOACOL = .TRUE.
C
C     get RCP file
C
      CALL INRCP('L2_COMPARE_RCP',IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &    'Cannot find L2_COMPARE.RCP','F')
      ENDIF
      CALL EZPICK('L2_COMPARE_RCP')
      CALL EZERR(IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &    'L2_COMPARE.RCP does not have L2_COMPARE bank!','F')
      ENDIF
      CALL EZGET_l('DO_FILT',DOFILT,IER)
      CALL EZGET_l('DO_TRGR',DOTRGR,IER)
      IF (DOTRGR) THEN
        CALL EZGET_l('L1_MATRIX',MATRIX,IER)
      ENDIF
      CALL EZGET_l('DO_FRES',DOFRES,IER)
      IF (DOFRES) THEN
        CALL EZGET_l('DO_L2JETS',DOL2JETS,IER)
        CALL EZGET_l('DO_L2EM',DOL2EM,IER)
      ENDIF
      CALL EZGET_l('DO_PNUT',DOPNUT,IER)
      CALL EZGET_l('DO_L0VT',DOL0VT,IER)
      CALL EZGET_l('DO_RMAS',DORMAS,IER)
      CALL EZGET_l('DO_LETA',DOLETA,IER)
      CALL EZGET_l('DO_ACOL',DOACOL,IER)
C
      CALL EZGET_l('PRTEMP',PRTEMP,IER)
      CALL EZGET_l('WRT_ALL',WRT_ALL,IER) 
      CALL EZGET_l('WRT_BAD',WRT_BAD,IER) 
      CALL EZGET_l('WRT_MU' ,WRT_MU,IER) 
C
      CALL EZGET_i('PRTLEV',PRTLEV,IER)
      ACTION = 'TERM'
      CALL EZGETS('LUN_OUT',1,ACTION,LEN,IER)
      IF (ACTION(1:4).EQ.'AUTO') THEN
C
C       open output file
C
   10   CONTINUE
        CALL GTUNIT(IUSER,LUN,IER)
        IF (IER.NE.0) THEN
C
C         rats, user already assigned - try again
C
          IF (IUSER.EQ.1000)
     &      CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &      'Cannot get LUN for output','F')
          IUSER = IUSER + 1
          GOTO 10
        ENDIF
C
C       good ole D0OPEN...
C
        CALL D0OPEN(LUN,'L2_COMPARE.OUT','OF',GOTIT)
        IF (.NOT.GOTIT) THEN
          LUN = 6
          CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &      'Cannot open new L2_COMPARE.OUT','I')
        ENDIF
      ELSE
        LUN = 6
      ENDIF
C
      IF (DOL2JETS) THEN
        CALL EZGET('JAUX_ET',JAUX_ET,IER)
        CALL EZGET('JAUX_ETA',JAUX_ETA,IER)
        CALL EZGET('JAUX_PHI',JAUX_PHI,IER)
        CALL EZGET('JAUX_ETAS',JAUX_ETAS,IER)
        CALL EZGET('JAUX_PHIS',JAUX_PHIS,IER)
        CALL EZGET('JAUX_EMF',JAUX_EMF,IER)
      ENDIF
C
      IF (DOL2EM) THEN
        CALL EZGET('L2EM_ET',L2EM_ET,IER)
        CALL EZGET('L2EM_SUMEM',L2EM_SUMEM,IER)
        CALL EZGET('L2EM_EM1R',L2EM_EM1R,IER)
        CALL EZGET('L2EM_EM12R',L2EM_EM12R,IER)
        CALL EZGET('L2EM_EM3R',L2EM_EM3R,IER)
        CALL EZGET('L2EM_EM4R',L2EM_EM4R,IER)
        CALL EZGET('L2EM_FH1R',L2EM_FH1R,IER)
        CALL EZGET('L2EM_SIGMA3',L2EM_SIGMA3,IER)
        CALL EZGET('L2EM_SIGMA5',L2EM_SIGMA5,IER)
        CALL EZGET('L2EM_SIG3MID',L2EM_SIG3MID,IER)
        CALL EZGET('L2EM_SH13',L2EM_SH13,IER)
        CALL EZGET('L2EM_SH24',L2EM_SH24,IER)
        CALL EZGET('L2EM_SH35',L2EM_SH35,IER)
        CALL EZGET('L2EM_SH57',L2EM_SH57,IER)
        CALL EZGET('L2EM_CONER',L2EM_CONER,IER)
        CALL EZGET('L2EM_FCONE',L2EM_FCONE,IER)
        CALL EZGET('L2EM_DETA',L2EM_DETA,IER)
        CALL EZGET('L2EM_DPHI',L2EM_DPHI,IER)
      ENDIF
C
      IF (DOPNUT) THEN
        CALL EZGET('PNUT_EX',PNUT_EX,IER)
        CALL EZGET('PNUT_EY',PNUT_EY,IER)
        CALL EZGET('PNUT_ET',PNUT_ET,IER)
        CALL EZGET('PNUT_PHI',PNUT_PHI,IER)
        CALL EZGET('PNUT_ETSCALAR',PNUT_ETSCALAR,IER)
      ENDIF
C
      IF (DOMUOT) THEN
        CALL EZGET('MUOT_XIN',MUOT_XIN,IER)
        CALL EZGET('MUOT_YIN',MUOT_YIN,IER)
        CALL EZGET('MUOT_ZIN',MUOT_ZIN,IER)
        CALL EZGET('MUOT_XOUT',MUOT_XOUT,IER)
        CALL EZGET('MUOT_YOUT',MUOT_YOUT,IER)
        CALL EZGET('MUOT_ZOUT',MUOT_ZOUT,IER)
        CALL EZGET('MUOT_XCIN',MUOT_XCIN,IER)
        CALL EZGET('MUOT_YCIN',MUOT_YCIN,IER)
        CALL EZGET('MUOT_ZCIN',MUOT_ZCIN,IER)
        CALL EZGET('MUOT_XCOUT',MUOT_XCOUT,IER)
        CALL EZGET('MUOT_YCOUT',MUOT_YCOUT,IER)
        CALL EZGET('MUOT_ZCOUT',MUOT_ZCOUT,IER)
        CALL EZGET('MUOT_CHB',MUOT_CHB,IER)
        CALL EZGET('MUOT_CHNB',MUOT_CHNB,IER)
        CALL EZGET('MUOT_BDL',MUOT_BDL,IER)
        CALL EZGET('MUOT_P',MUOT_P,IER)
        CALL EZGET('MUOT_DP',MUOT_DP,IER)
        CALL EZGET('MUOT_ECAL',MUOT_ECAL,IER)
        CALL EZGET('MUOT_EFE',MUOT_EFE,IER)
      ENDIF
c
      IF (DOL0VT) THEN
        CALL EZGET('L0VT_ZFAST',L0VT_ZFAST,IER)
        CALL EZGET('L0VT_ZSLOW',L0VT_ZSLOW,IER)
      ENDIF
C
      IF (DORMAS) THEN
        CALL EZGET('RMAS_MASS',RMAS_MASS,IER)
        CALL EZGET('RMAS_ETAB',RMAS_ETAB,IER)
        CALL EZGET('RMAS_ET',RMAS_ET,IER)
        CALL EZGET('RMAS_ETA',RMAS_ETA,IER)
        CALL EZGET('RMAS_PHI',RMAS_PHI,IER)
      ENDIF
C
      IF (DOLETA) THEN
        CALL EZGET('LETA_DETA',LETA_DETA,IER)
        CALL EZGET('LETA_ET',LETA_ET,IER)
        CALL EZGET('LETA_ETA',LETA_ETA,IER)
        CALL EZGET('LETA_PHI',LETA_PHI,IER)
      ENDIF
C
      IF (DOACOL) THEN
        CALL EZGET('ACOL_ET',ACOL_ET,IER)
        CALL EZGET('ACOL_ETA',ACOL_ETA,IER)
        CALL EZGET('ACOL_PHI',ACOL_PHI,IER)
      ENDIF
C
C     all done with RCP
C
      CALL EZRSET
C
      RETURN
C
C-------------------------------------------------------------------
      ENTRY L2_COMPARE_DIA()
C
      L2_COMPARE_DIA = .TRUE.
C
C     dump info per event?  creates lots of printout, also available
C     from within RCP file
C
      CALL GETPAR(1,'Print level (0=lots, 1=less, etc.): ','I',PRTLEV)
C
      RETURN
C
C-------------------------------------------------------------------
      ENTRY L2_COMPARE_SUM()
C
      L2_COMPARE_SUM = .TRUE.
      WRITE(TRUN,701) NRUN
 701  FORMAT(I5,'.OUT')
      TITLE=TMAIN//TRUN
C
C     program summary hook...
C
      IUSER=0
      IF (ACTION(1:4).EQ.'AUTO') THEN
C
C       open output file
C
  910   CONTINUE
        CALL GTUNIT(IUSER,LUN1,IER)
        IF (IER.NE.0) THEN
C
C         rats, user already assigned - try again
C
          IF (IUSER.EQ.1000)
     &      CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &      'Cannot get LUN1 for output','F')
          IUSER = IUSER + 1
          GOTO 910
        ENDIF
C
C       good ole D0OPEN...
C
        CALL D0OPEN(LUN1,TITLE,'OF',GOTIT)
        IF (.NOT.GOTIT) THEN
          LUN1 = 7
          CALL ERRMSG('L2_COMPARE','L2_COMPARE_INI',
     &      'Cannot open new L2_COMP_SUMM_NRUNX.OUT','I')
        ENDIF
      ELSE
        LUN1= 7
      ENDIF
C
      WRITE(LUN1,'(/,
     &  1X,75(''*''),/,
     &  16X,''L 2 _ C O M P A R E    S U M M A R Y'',/,
     &  1X,75(''*''))')
      CALL ERRSUM(LUN1)
      IF (DOFRES) THEN
        IF (DOL2JETS) THEN
          CALL L2_COMP_JETS_OUT(LUN1)
          WRITE(LUN1,'(
     &    '' L2JETS tolerances: ET   '',F7.2,''      ETA  '',F7.2,/,
     &    ''                    PHI  '',F7.2,''      EMF  '',F7.2,/,
     &    ''                    ETAS '',F7.2,''      PHIS '',F7.2,/)')
     &    JAUX_ET,JAUX_ETA,JAUX_PHI,JAUX_EMF,JAUX_ETAS,JAUX_PHIS
        ENDIF
        IF (DOL2EM) THEN
          WRITE(LUN1,'(
     &      '' L2EM tolerances: ET             '',F7.2,10X,
     &      ''SUMEM         '',F7.2,/,
     &      ''                  EM1/SUMEM      '',F7.2,10X,
     &      ''EM1+2/SUMEM   '',F7.2,/,
     &      ''                  EM3/SUMEM      '',F7.2,10X,
     &      ''EM4/SUMEM     '',F7.2,/,
     &      ''                  FH1/SUMEM      '',F7.2,10X,
     &      ''SIGMA3        '',F7.2,/,
     &      ''                  SIGMA5         '',F7.2,10X,
     &      ''SIG3+MID      '',F7.2,/,
     &      ''                  E3MAX/3X3      '',F7.2,10X,
     &      ''(4X4-2X2)/2X2 '',F7.2)')
     &      L2EM_ET,L2EM_SUMEM,L2EM_EM1R,L2EM_EM12R,L2EM_EM3R,L2EM_EM4R,
     &      L2EM_FH1R,L2EM_SIGMA3,L2EM_SIGMA5,L2EM_SIG3MID,
     &      L2EM_SH13,L2EM_SH24
          WRITE(LUN1,'(
     &      ''                  (5X5-3X3)/3X3  '',F7.2,10X,
     &      ''(5X5-7X7)/5X5 '',F7.2,/,
     &      ''                  CONE R         '',F7.2,10X
     &      ''FR. CONE ET   '',F7.2,/,
     &      ''                  ETA WIDTH      '',F7.2,10X,
     &      ''PHI WIDTH     '',F7.2,/)')
     &      L2EM_SH35,L2EM_SH57,L2EM_CONER,L2EM_FCONE,L2EM_DETA,
     &      L2EM_DPHI
        ENDIF
      ENDIF
      IF (DOPNUT) THEN
        WRITE(LUN1,'(
     &      '' PNUT tolerances: EX     '',F7.2,''   EY     '',F7.2,/,
     &      ''                  ET     '',F7.2,''   PHI    '',F7.2,/,
     &      ''                  SCALAR '',F7.2)')
     &      PNUT_EX,PNUT_EY,PNUT_ET,PNUT_PHI,PNUT_ETSCALAR
      ENDIF
      IF (DORMAS) THEN
        WRITE(LUN1,'(
     &      '' RMAS tolerances: MASS   '',F7.2,''   ETAB   '',F7.2,/,
     &      ''                  ET     '',F7.2,''   ETA    '',F7.2,/,
     &      ''                  PHI    '',F7.2)')
     &      RMAS_MASS,RMAS_ETAB,RMAS_ET,RMAS_ETA,RMAS_PHI
      ENDIF
      IF (DOLETA) THEN
        WRITE(LUN1,'(
     &      '' LETA tolerances: DEL_ETA'',F7.2,''   ET     '',F7.2,/,
     &      ''                  ETA    '',F7.2,''   PHI    '',F7.2)')
     &      LETA_DETA,LETA_ET,LETA_ETA,LETA_PHI
      ENDIF
      IF (DOACOL) THEN
        WRITE(LUN1,'(
     &      '' ACOL tolerances: ET     '',F7.2,''   ETA    '',F7.2,/,
     &      ''                  PHI    '',F7.2)')
     &      ACOL_ET,ACOL_ETA,ACOL_PHI
      ENDIF
      IF (DOFILT) THEN
        FPSET = L2_COMPARE_FILT_OUT(LUN1)
      ENDIF
      WRITE(LUN1,'(
     &  '' Total number of events compared: '',I7,/,
     &  '' Discrepancies found:'')') NEVTS
      IF (DOTRGR) THEN
        WRITE(LUN1,'(
     &    ''        TRGR:                     '',I7,
     &    '' out of '',I7)') NBTRGR,NTTRGR
      ENDIF
C
      IF (DOFRES) THEN
        IF (DOL2JETS) THEN
          WRITE(LUN1,'(
     &      ''        JAUX:                     '',I7,
     &      '' out of '',I7)') NBJAUX,NTJAUX
        ENDIF
        IF (DOL2EM) THEN
          WRITE(LUN1,'(
     &      ''        L2EM:                     '',I7,
     &      '' out of '',I7)') NBL2EM,NTL2EM
        ENDIF
      ENDIF
      IF (DOPNUT) THEN
          WRITE(LUN1,'(
     &      ''        PNUT:                     '',I7,
     &      '' out of '',I7)') NBPNUT,NTPNUT
      ENDIF
      IF (DOMUOT) THEN
          WRITE(LUN1,'(
     &      ''        MUOT:                     '',I7,
     &      '' out of '',I7)') NBMUOT,NTMUOT
      ENDIF
      IF (DOL0VT) THEN
          WRITE(LUN1,'(
     &      ''        L0VT:                     '',I7,
     &      '' out of '',I7)') NBL0VT,NTL0VT
      ENDIF
          WRITE(LUN1,'(
     &      ''       EMPTY:                     '',I7,
     &      '' out of '',I7)') NEMPTY,NEVTS
      IF (DORMAS) THEN
          WRITE(LUN1,'(
     &      ''        RMAS:                     '',I7,
     &      '' out of '',I7)') NBRMAS,NTRMAS
      ENDIF
      IF (DOLETA) THEN
          WRITE(LUN1,'(
     &      ''        LETA:                     '',I7,
     &      '' out of '',I7)') NBLETA,NTLETA
      ENDIF
      IF (DOACOL) THEN
          WRITE(LUN1,'(
     &      ''        ACOL:                     '',I7,
     &      '' out of '',I7)') NBACOL,NTACOL
      ENDIF
C
      IF (MATRIX) THEN
        WRITE(LUN1,
     &      '('' Look in the .HST4 for 2-d L1 Accept matrix'')')
        IF (HEXIST(90001).AND.HEXIST(90002)) THEN
          CALL HOPERA(90001,'-   ',90002,90003,1.0,1.0)
        ENDIF
      ELSE
        WRITE(LUN1,'(''  but L1 Matrix not enabled'')')
      ENDIF
C
      IF (ACTION(1:4).EQ.'AUTO') THEN
        CLOSE(LUN)
        CLOSE(LUN1)
      ENDIF
C
      RETURN
      END
