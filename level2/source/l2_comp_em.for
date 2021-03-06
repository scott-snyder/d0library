      SUBROUTINE L2_COMP_EM(LUN,L2EM,IB,OKOK,
     &  L2EM_ET,L2EM_SUMEM,L2EM_EM1R,L2EM_EM12R,L2EM_EM3R,L2EM_EM4R,
     &  L2EM_FH1R,L2EM_SIGMA3,L2EM_SIGMA5,L2EM_SIG3MID,L2EM_SH13,
     &  L2EM_SH24,L2EM_SH35,L2EM_SH57,L2EM_CONER,L2EM_FCONE,
     &  L2EM_DETA,L2EM_DPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        compare output banks from l2_em
C
C     grabs info on the L2EM bank - electron tool results L2EM
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-APR-1992   Drew Baden
C-   Modified  2-DEC-1993   Andrzej Zieminski
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
c
      INTEGER L2EM(*),IB,LUN
      LOGICAL OKOK
C
      INTEGER NCAND1,NCAND2,NREP1,NREP2,IP1,IP2,I,J,NRUN,NEV
      INTEGER ICAND,IETA1,IETA2,IPHI1,IPHI2,NTRAK1,NTRAK2
      INTEGER TETA1,TETA2,TPHI1,TPHI2,ILYR1,ILYR2,FAIL1,FAIL2
C
      INTEGER ILIST(400),L2EM1,L2EM2, JCAND,NZBANK,IPAR1,IPAR2,IP3,IP4
      LOGICAL LIPAR
      CHARACTER*1 CIPAR
C
      LOGICAL LTETA,LTPHI,LIETA,LIPHI,LLYR,LET,LSUMEM,LEM1R,LEM12R,LEM3R
      LOGICAL LEM4R,LFH1R,LSIGMA3,LSIGMA5,LSIG3MID,LSH13,LSH24,LSH35
      LOGICAL LSH57,LCONER,LFCONE,LDETA,LDPHI,LNTRAK,LFAIL
      LOGICAL LDUMP,L2_NXT_REAL_DIFF,L2_NXT_INT_DIFF
      REAL L2EM_ET,L2EM_SUMEM,L2EM_EM1R,L2EM_EM12R,L2EM_EM3R,L2EM_EM4R
      REAL L2EM_FH1R,L2EM_SIGMA3,L2EM_SIGMA5,L2EM_SIG3MID
      REAL L2EM_SH13,L2EM_SH24,L2EM_SH35,L2EM_SH57,L2EM_CONER
      REAL L2EM_FCONE,L2EM_DETA,L2EM_DPHI
      REAL ET1,SUMEM1,EM1R1,EM12R1,EM3R1,EM4R1
      REAL FH1R1,SIGMA31,SIGMA51,SIG3MID1
      REAL SH131,SH241,SH351,SH571,CONER1
      REAL FCONE1,DETA1,DPHI1
      REAL ET2,SUMEM2,EM1R2,EM12R2,EM3R2,EM4R2
      REAL FH1R2,SIGMA32,SIGMA52,SIG3MID2
      REAL SH132,SH242,SH352,SH572,CONER2
      REAL FCONE2,DETA2,DPHI2
      CHARACTER*1 CET,CSUMEM,CEM1R,CEM12R,CEM3R
      CHARACTER*1 CEM4R,CFH1R,CSIGMA3,CSIGMA5,CSIG3MID
      CHARACTER*1 CSH13,CSH24,CSH35,CSH57,CCONER
      CHARACTER*1 CFCONE,CDETA,CDPHI,CNTRAK
      CHARACTER*1 CTETA,CTPHI,CIETA,CIPHI,CLYR,CFAIL
C
      DATA CET/' '/,CSUMEM/' '/,CEM1R/' '/,CEM12R/' '/,CEM3R/' '/
      DATA CEM4R/' '/,CFH1R/' '/,CSIGMA3/' '/,CSIGMA5/' '/,CSIG3MID/' '/
      DATA CSH13/' '/,CSH24/' '/,CSH35/' '/,CSH57/' '/,CCONER/' '/
      DATA CFCONE/' '/,CDETA/' '/,CDPHI/' '/,CFAIL/' '/,CNTRAK/' '/
      DATA CTETA/' '/,CTPHI/' '/,CIETA/' '/,CIPHI/' '/,CLYR/' '/
C----------------------------------------------------------------------
C
      OKOK = .TRUE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_EM',
     &    'L2_COMP_EM called for IB.NE.2 ! Not allowed!!!','F')
        RETURN
      ENDIF
C
C     make sure there is L2EM - no mistakes
C
      IF (L2EM(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' has NO L2EM bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (L2EM(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' has NO L2EM bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C
C     ok, compare the two
C
      NCAND1 = NZBANK(IXCOM,L2EM(1))
      NCAND2 = NZBANK(IXCOM,L2EM(2))
      IF (NCAND1.NE.NCAND2) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' L2EM discrepancy in #Candidates (S,D): '',2I6)')
     &      NRUN,NEV,NCAND1,NCAND2
c       GOTO 999
      ENDIF
C
      IF (NCAND1.GT.400) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' Too many L2EM Candidates (S,D): '',2I6)')
     &      NRUN,NEV,NCAND1,NCAND2
        GOTO 999
      ENDIF
C
      NREP1 = IQ(L2EM(1)+2)
      NREP2 = IQ(L2EM(2)+2)
      IF (NREP1.NE.NREP2) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' L2EM discrepancy in repetition length (S,D):'',2I7)')
     &      NRUN,NEV,NREP1,NREP2
        GOTO 999
      ENDIF
C
      CALL VZERO(ILIST,400)
      L2EM1 = L2EM(1)
      ICAND = 0
50    ICAND = ICAND+1
      L2EM2 = L2EM(2)
      JCAND = 0
60    IP2 = 3 + L2EM2
      IP1 = 3 + L2EM1
      JCAND = JCAND + 1
      LTETA = L2_NXT_INT_DIFF(IP1,IP2,TETA1,TETA2,CTETA)
      LTPHI = L2_NXT_INT_DIFF(IP1,IP2,TPHI1,TPHI2,CTPHI)
      LIETA = L2_NXT_INT_DIFF(IP1,IP2,IETA1,IETA2,CIETA)
      LIPHI = L2_NXT_INT_DIFF(IP1,IP2,IPHI1,IPHI2,CIPHI)
      IP3=IP1+21
      IP4=IP2+21
      LIPAR = L2_NXT_INT_DIFF(IP3,IP4,IPAR1,IPAR2,CIPAR)
      IF(LTETA.OR.LTPHI.OR.LIETA.OR.LIPHI.OR.LIPAR) GOTO 61
      ILIST(ICAND)=JCAND
      GOTO 51
61    L2EM2 = LQ(L2EM2)
      IF(L2EM2.GT.0) GOTO 60
C
51    L2EM1 = LQ(L2EM1)
      IF(L2EM1.GT.0) GOTO 50
C
      IF (NCAND1.NE.ICAND) THEN
        WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' L2EM problem with the bank chain (S,D):'',2I7)')
     &      NRUN,NEV,NCAND1,ICAND
        GOTO 999
      ENDIF
C
      DO ICAND=1,NCAND1
        IF (ILIST(ICAND).EQ.0) THEN
          WRITE(LUN,'('' L2_COMP_EM: Run/Event '',2I7,
     &      '' L2EM: no match for an e-cand 
     &  (S,D):'',2I7)') NRUN,NEV,ICAND
c          GOTO 999
        ENDIF
      ENDDO
C
      L2EM1 = L2EM(1)
      ICAND = 0
70    ICAND = ICAND+1
      L2EM2 = L2EM(2)
      JCAND = 0
80    IP2 = 3 + L2EM2
      IP1 = 3 + L2EM1
       JCAND = JCAND + 1
      IF(JCAND.NE.ILIST(ICAND)) GOTO 81
        LTETA = L2_NXT_INT_DIFF(IP1,IP2,TETA1,TETA2,CTETA)
        LTPHI = L2_NXT_INT_DIFF(IP1,IP2,TPHI1,TPHI2,CTPHI)
        LIETA = L2_NXT_INT_DIFF(IP1,IP2,IETA1,IETA2,CIETA)
        LIPHI = L2_NXT_INT_DIFF(IP1,IP2,IPHI1,IPHI2,CIPHI)
        LLYR  = L2_NXT_INT_DIFF(IP1,IP2,ILYR1,ILYR2,CLYR)
        LET   = L2_NXT_REAL_DIFF(IP1,IP2,ET1,ET2,L2EM_ET,CET)
        LSUMEM = L2_NXT_REAL_DIFF(IP1,IP2,SUMEM1,SUMEM2,L2EM_SUMEM,
     &    CSUMEM)
        LEM1R = L2_NXT_REAL_DIFF(IP1,IP2,EM1R1,EM1R2,L2EM_EM1R,CEM1R)
        LEM12R = L2_NXT_REAL_DIFF(IP1,IP2,EM12R1,EM12R2,L2EM_EM12R,
     &    CEM12R)
        LEM3R = L2_NXT_REAL_DIFF(IP1,IP2,EM3R1,EM3R2,L2EM_EM3R,CEM3R)
        LEM4R = L2_NXT_REAL_DIFF(IP1,IP2,EM4R1,EM4R2,L2EM_EM4R,CEM4R)
        LFH1R = L2_NXT_REAL_DIFF(IP1,IP2,FH1R1,FH1R2,L2EM_FH1R,CFH1R)
        LSIGMA3 = L2_NXT_REAL_DIFF(IP1,IP2,SIGMA31,SIGMA32,L2EM_SIGMA3,
     &    CSIGMA3)
        LSIGMA5 = L2_NXT_REAL_DIFF(IP1,IP2,SIGMA51,SIGMA52,L2EM_SIGMA5,
     &    CSIGMA5)
        LSIG3MID = L2_NXT_REAL_DIFF(IP1,IP2,SIG3MID1,SIG3MID2,
     &    L2EM_SIG3MID,CSIG3MID)
        LSH13 = L2_NXT_REAL_DIFF(IP1,IP2,SH131,SH132,L2EM_SH13,CSH13)
        LSH24 = L2_NXT_REAL_DIFF(IP1,IP2,SH241,SH242,L2EM_SH24,CSH24)
        LSH35 = L2_NXT_REAL_DIFF(IP1,IP2,SH351,SH352,L2EM_SH35,CSH35)
        LSH57 = L2_NXT_REAL_DIFF(IP1,IP2,SH571,SH572,L2EM_SH57,CSH57)
        LCONER = L2_NXT_REAL_DIFF(IP1,IP2,CONER1,CONER2,L2EM_CONER,
     &    CCONER)
        LFCONE = L2_NXT_REAL_DIFF(IP1,IP2,FCONE1,FCONE2,L2EM_FCONE,
     &    CFCONE)
        LDETA = L2_NXT_REAL_DIFF(IP1,IP2,DETA1,DETA2,L2EM_DETA,CDETA)
        LDPHI = L2_NXT_REAL_DIFF(IP1,IP2,DPHI1,DPHI2,L2EM_DPHI,CDPHI)
C
C-      Temporary fix
C
        IF(DETA1.EQ.0. .OR. DETA2.EQ.0.)LDETA=.FALSE.
        IF(DPHI1.EQ.0. .OR. DPHI2.EQ.0.)LDPHI=.FALSE.

        LNTRAK = L2_NXT_INT_DIFF(IP1,IP2,NTRAK1,NTRAK2,CNTRAK)
        LFAIL = L2_NXT_INT_DIFF(IP1,IP2,FAIL1,FAIL2,CFAIL)
C
        LDUMP = LTETA.OR.LTPHI.OR.LIETA.OR.LIPHI.OR.LLYR.OR.LET.OR.
     &    LSUMEM.OR.LEM1R.OR.LEM12R.OR.LEM3R.OR.LEM4R.OR.LFH1R.OR.
     &    LSIGMA3.OR.LSIGMA5.OR.LSIG3MID.OR.LSH13.OR.LSH24.OR.LSH35.OR.
     &    LSH57.OR.LCONER.OR.LFCONE.OR.LDETA.OR.LDPHI.OR.LNTRAK.OR.LFAIL
        IF (LDUMP) THEN
          OKOK = .FALSE.
          WRITE(LUN,'(/,'' L2_COMP_EM:  RUN/EVENT '',2I7,
     &        '' L2EM entry discrepancy'',/,
     &        '' "*" denote variables whose differences is '',
     &        ''outside of tolerances'')') NRUN,NEV
          WRITE(LUN,'(
     &      ''         Eta/Phi/Layer               '',
     &      ''EM1 EM1+2   EM3   EM4   FH1 -----SIGMA-----'',/,
     &      ''          L1       L2     ET   sEM  '',
     &      ''/sEM  /sEM  /sEM  /sEM  /sEM   3    5  3+MID'',/,
     &      ''        --------------- ----- ----- ---- '',
     &      '' ----  ----  ----  ---- ---- ---- -----'')')
          WRITE(LUN,'('' SIM'',I2,I3,A1,I2,A1,I3,A1,2(I2,A1),
     &      F5.1,A1,F5.1,A1,5(F5.2,A1),2(F4.1,A1),E7.1,A1)')
     &      ICAND,TETA1,CTETA,TPHI1,CTPHI,IETA1,CIETA,IPHI1,CIPHI,
     &      ILYR1,CLYR,ET1,CET,SUMEM1,CSUMEM,EM1R1,CEM1R,
     &      EM12R1,CEM12R,EM3R1,CEM3R,EM4R1,CEM4R,FH1R1,CFH1R,
     &      SIGMA31,CSIGMA3,SIGMA51,CSIGMA5,SIG3MID1,CSIG3MID
          WRITE(LUN,'('' DAT'',I2,I3,A1,I2,A1,I3,A1,2(I2,A1),
     &      F5.1,A1,F5.1,A1,5(F5.2,A1),2(F4.1,A1),e7.1,A1)')
     &      ICAND,TETA2,CTETA,TPHI2,CTPHI,IETA2,CIETA,IPHI2,CIPHI,
     &      ILYR2,CLYR,ET2,CET,SUMEM2,CSUMEM,EM1R2,CEM1R,
     &      EM12R2,CEM12R,EM3R2,CEM3R,EM4R2,CEM4R,FH1R2,CFH1R,
     &      SIGMA32,CSIGMA3,SIGMA52,CSIGMA5,SIG3MID2,CSIG3MID
          WRITE(LUN,'(/,''                          '',
     &      ''                          Width'',/,
     &      ''          SH13   SH24   SH35   SH57  CONE  fISO '',
     &      ''   Eta  Phi   #Trks   Status'',/,
     &      ''         -----  -----  -----  -----  ----  ---- '',
     &      ''  ----------  -----   ------'')')
          WRITE(LUN,'('' SIM'',I3,1X,4(F6.1,A1),2(F5.2,A1),F6.2,A1,
     &      F5.2,A1,I6,A1,2(I8,A1))')
     &      ICAND,SH131,CSH13,SH241,CSH24,SH351,CSH35,SH571,CSH57,
     &      CONER1,CCONER,FCONE1,CFCONE,DETA1,CDETA,DPHI1,CDPHI,
     &      NTRAK1,CNTRAK,FAIL1,CFAIL
          WRITE(LUN,'('' DAT'',I3,1X,4(F6.1,A1),2(F5.2,A1),F6.2,A1,
     &      F5.2,A1,I6,A1,2(I8,A1))')
     &      ICAND,SH132,CSH13,SH242,CSH24,SH352,CSH35,SH572,CSH57,
     &      CONER2,CCONER,FCONE2,CFCONE,DETA2,CDETA,DPHI2,CDPHI,
     &      NTRAK2,CNTRAK,FAIL2,CFAIL
        ENDIF
81    L2EM2 = LQ(L2EM2)
      IF(L2EM2.GT.0) GOTO 80
C
71    L2EM1 = LQ(L2EM1)
      IF(L2EM1.GT.0) GOTO 70
C
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      OKOK = .FALSE.
      RETURN
C
      END
