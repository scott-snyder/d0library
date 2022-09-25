      SUBROUTINE PTAUFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      find TAU candidates and fill PTAU banks
C-
C-   Created  27-SEP-1990   Serban D. Protopopescu
C-   Updated  26-FEB-1993   Susan K. Blessing  Add EZRSET call.
C-    Add MIN_NTRACK, MAX_NTRACK.
C-   Updated  12-MAY-1993   Amber S. Boehnlein  Add track distance,
C-    impact parameter, Fixed bug in delta_theta calculation
C-   Updated  12-NOV-1993   Qizhong Li-Demarteau  added hottest Et1 and Et2
C-                                        and energy correction status word
C-   Updated  18-NOV-1993   Qizhong Li-Demarteau  fixed delphi calculation,
C-                         added energy for window of 1x1, 3x3, 5x5 towers
C-                         and fill tau ID into JETS bank
C-   Updated  24-NOV-1993   Qizhong Li-Demarteau  fixed the bug in ZV by
C-                                replacing ZVERTE call with VERTEX_INFO
C-   Updated  17-MAR-1994   Qizhong Li-Demarteau  added NTRKs for different
C-                                regions (10, 20 and 30 degree cone)
C-   Updated  12-DEC-1994   Qizhong Li-Demarteau  Added 4 hottest tower's
C-                                            Et, iphi, ieta into PTAU
C-   Updated  31-JAN-1995   Qizhong Li-Demarteau  added cleantau
C-   Updated  10-FEB-1995   Qizhong Li-Demarteau  added tau H-matrix
C-   Updated  11-JUL-1995   Qizhong Li-Demarteau  use ZMAX from ZTRLNK.INC
C-   Updated  25-JUL-1995   Qizhong Li-Demarteau  use NEW_RMS for PTAU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZTRLNK.INC/LIST'
C
      INTEGER GZJETS,LPTAU,LZTR(ZMAX),NZTR,NV,I,GZPTAU
C       use ZLINKC to protect links
      EQUIVALENCE(LPTAU,CSTLNK(50))
      EQUIVALENCE(LZTR,CSTLNK(51))
      INTEGER TRACK
      INTEGER MIN_NTRACK,MAX_NTRACK
      INTEGER  STATUS, IW, IPHI, IETA
C      INTEGER  ZMAX
C      PARAMETER( ZMAX = 450 )
C      INTEGER  ZLINK(ZMAX)
      INTEGER  IR, NTRK, IDC(200)
      INTEGER   HIDATA, HIETA(4), HIPHI(4), IHOT
      INTEGER*2 HIINFO(2)
      EQUIVALENCE (HIINFO(1),HIDATA)
      INTEGER USE_NEW_RMS
C
      REAL R_RMS,R_RMS_CUT,TH,PHI,DELTA_TH,DELTA_PHI,DELTA_ETA
      REAL ETA,PT,THMIN,THMAX,PHIMIN,PHIMAX,IER,ZV,ZVTX_INFO(3,1)
      REAL    TTHETA,TETA,TPHI
      REAL EM_RATIO,EM_RATIO_CUT,ETA_CUT
      REAL    MAX_DIST_ETA_PHI, MAX_XY_IMPACT
      REAL    DIST_ETA_PHI,MIN_DEP,XY_IMPACT,MIN_XYI
      REAL    HIET(4)
      REAL    DELPHI, SUME
      REAL    PETA_TO_DETA
      REAL    REGION(3), DELTAA, NEWPI
      REAL    NEW_RMS, PROB(2), CHISQ(2), FVAR
      REAL    THRESHOLD
      REAL    NEW_RMS_CUT
C
      LOGICAL FIRST,EZERR
      LOGICAL OK
      LOGICAL DST
      LOGICAL FLGVAL, VRFFLG, HST_VERIFY
      LOGICAL CANDIDATE
C
      SAVE FIRST,R_RMS_CUT,DELTA_ETA,DELTA_PHI,EM_RATIO_CUT
      SAVE ETA_CUT,MIN_NTRACK,MAX_NTRACK
      SAVE MAX_DIST_ETA_PHI, MAX_XY_IMPACT
C
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALL DHDIR('CTAUS_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER .NE. 0) THEN
        CALL ERRMSG('TAU_FIX','PTAUFL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CTAUS_RCP')       ! SELECT CTAUS RCP BANK
C
        IF (EZERR(IER)) THEN
          CALL ERRMSG('CTAUS','PTAUFL',
     &      'CTAUS RCP bank not found in PTAUFL.','W')
        ELSE
C
C            read parameters for cuts
C
          CALL EZGET('R_RMS_CUT',R_RMS_CUT,IER)
          CALL EZGET('DELTA_PHI',DELTA_PHI,IER)
          CALL EZGET('DELTA_ETA',DELTA_ETA,IER)
          CALL EZGET('EM_RATIO_CUT',EM_RATIO_CUT,IER)
          CALL EZGET('ETA_CUT',ETA_CUT,IER)
          CALL EZGET_i('MIN_NTRACK',MIN_NTRACK,IER)
          CALL EZGET_i('MAX_NTRACK',MAX_NTRACK,IER)
          CALL EZGET('MAX_DIST_ETA_PHI',MAX_DIST_ETA_PHI,IER)
          CALL EZGET('MAX_XY_IMPACT',MAX_XY_IMPACT,IER)
          CALL EZGET('NTRK_REGIONS',REGION(1),IER)
          CALL EZGET('THRESHOLD_FOR_RMS',THRESHOLD,IER)
          CALL EZGET_i('USE_NEW_RMS',USE_NEW_RMS,IER)
          CALL EZGET('NEW_RMS_CUT',NEW_RMS_CUT,IER)
          CALL EZGET_l('DST',DST,IER)
          CALL EZGET_l('HST_VERIFY',HST_VERIFY,IER)
          CALL EZRSET
          VRFFLG = FLGVAL('VERIFY')
          VRFFLG = VRFFLG .OR. HST_VERIFY
          IF (VRFFLG) THEN
            CALL HBOOK1(321,'n ztrk',10,-0.5,9.5,0.)
            CALL HBOOK1(322,'min xyi',50,0.0,5.0,0.)
            CALL HBOOK1(323,'min r',50,0.0,1.0,0.)
            CALL HBOOK1(331,'emf',50,0.0,1.0,0.)
            CALL HBOOK1(332,'eta$',50,-5.0,5.0,0.)
            CALL HBOOK1(333,'JET RMS$',50,0.0,0.3,0.)
            CALL HBOOK1(334,'NEW RMS$',50,0.0,0.3,0.)
            CALL HBOOK1(335,'JET RMS - NEW RMS$',50,-0.3,0.3,0.)
            CALL HBOOK1(336,'CHISQ(S)$',50,0.0,300.0,0.)
            CALL HBOOK1(337,'CHISQ(B)$',50,0.0,300.0,0.)
            CALL HBOOK1(338,'FISHER VAR$',50,-100.0,100.0,0.)
            CALL HBOOK2(340,'old rms vs new rms(thr=300)',
     &        50,0.0,0.3,50,0.0,0.3,0.)
            do 701 i= 0,10
              call HBOOK1(350+i,'NEW RMS',50,0.0,0.3,0.)
  701       continue
            call hbprof(380,'rms vs threshold',12,-0.1,1.1,-0.1,0.3,' ')
          ENDIF
        ENDIF
C
C Initialize link area ZLINKC.
        CALL CZLINI
C
      ENDIF
C
      LPTAU=GZPTAU()
      IF(LPTAU.GT.0) GOTO 999         ! do not refind taus without dropping
C
C        loop over all jets to find TAUS
      LJETS=GZJETS()
C
      DO WHILE (LJETS.GT.0)
        R_RMS=SQRT(Q(LJETS+12)**2+Q(LJETS+13)**2)
        EM_RATIO=Q(LJETS+14)
        ETA=Q(LJETS+9)
C
        NEW_RMS = 9999.9
        CANDIDATE = .FALSE.
        IF (USE_NEW_RMS .GT. 0) THEN
          CALL CJET_NEWRMS(LJETS,THRESHOLD,NEW_RMS,OK)
          IF (OK .AND. (NEW_RMS.LT.NEW_RMS_CUT)) THEN
            CANDIDATE = .TRUE.
          ENDIF
        ELSE
          IF ((USE_NEW_RMS .NE. 1) .AND. (R_RMS .LT. R_RMS_CUT)) THEN
            CANDIDATE = .TRUE.
          ENDIF
        ENDIF
        IF (VRFFLG) THEN
          CALL HFILL(331,EM_RATIO,0.,1.)
          CALL HFILL(332,eta,0.,1.)
          CALL HFILL(333,r_rms,0.,1.)
          IF (USE_NEW_RMS .GT. 0) THEN
            CALL HFILL(334,new_rms,0.,1.)
            CALL HFILL(335,r_rms-new_rms,0.,1.)
          ENDIF
        ENDIF
        IF (CANDIDATE .AND. EM_RATIO.LT.EM_RATIO_CUT 
     &    .AND.ABS(ETA).LT.ETA_CUT) THEN
          TH =Q(LJETS+7)
          PHI=Q(LJETS+8)
          DELTA_TH=2.*(ATAN(EXP(-ETA+DELTA_ETA))-
     &                 ATAN(EXP(-ETA)))
          DELTA_TH=ABS(DELTA_TH)
          PHIMIN=PHI-DELTA_PHI
          PHIMAX=PHI+DELTA_PHI
          THMIN=TH-DELTA_TH
          THMAX=TH+DELTA_TH
          PT=Q(LJETS+6)/3.
          CALL VERTEX_INFO(1,NV,ZVTX_INFO,OK)
C            Only consider the main primary vertex
          IF ( OK ) THEN
            ZV = ZVTX_INFO(1,1)
          ELSE
            ZV = 0.0
          ENDIF
          IF (DST) THEN
            CALL ZTRK_IN_ROAD(ZV,PHIMIN,PHIMAX,THMIN,THMAX,NZTR,LZTR)
          ELSE
            CALL ZTRAKS(ZV,PHIMIN,PHIMAX,THMIN,THMAX,PT,NZTR,LZTR)
          ENDIF
C
          IF (VRFFLG) THEN
            CALL HFILL(321,FLOAT(NZTR),0.,1.)
          ENDIF
          IF (NZTR.GE.MIN_NTRACK.AND.NZTR.LE.MAX_NTRACK) THEN
C Look for closest track in eta-phi
            MIN_DEP = 99999.
            MIN_XYI = 99999.
            DO TRACK = 1, NZTR
              LZTRK     = LZTR(TRACK)
              LZFIT     = LQ(LZTRK-1)
              IF (LZFIT.GT.0)THEN
                TTHETA    = Q(LZFIT+13)
                TETA      = -LOG(TAN(TTHETA/2.))
                TPHI      = Q(LZFIT+10)
                XY_IMPACT = Q(LZFIT+32)
C
C Distance in eta-phi where TPHI, TETA are associated with the track
C and PHI, ETA are associated with the tau jet candidate
C
                DELPHI = ABS(TPHI - PHI)
                IF (DELPHI .GT. (TWOPI - DELPHI)) THEN
                  DELPHI = TWOPI - DELPHI
                ENDIF
                DIST_ETA_PHI = SQRT(DELPHI**2 + (TETA-ETA)**2)
                IF (DIST_ETA_PHI.LT.MIN_DEP)
     &            MIN_DEP = DIST_ETA_PHI
                IF (XY_IMPACT.LT.MIN_XYI)
     &            MIN_XYI = XY_IMPACT
              ENDIF
            ENDDO
            IF (VRFFLG) THEN
              CALL HFILL(322,MIN_XYI,0.,1.)
              CALL HFILL(323,MIN_DEP,0.,1.)
            ENDIF
            IF (MIN_XYI.LE.MAX_XY_IMPACT.AND.
     &          MIN_DEP.LE.MAX_DIST_ETA_PHI) THEN
C we have a tau candidate
              CALL BKPTAU(NZTR,LPTAU)
              LQ(LPTAU-2)=LJETS           ! fill reference link to JETS
              IQ(LJETS+20) = IQ(LPTAU-5)  ! fill tau ID to JETS bank
              DO I=1,NZTR
                LQ(LPTAU-2-I)=LZTR(I)
                CALL ZTFLAG(LZTR(I),'TAU')
              ENDDO
              CALL UCOPY(Q(LJETS+2),Q(LPTAU+3),8)
              Q(LPTAU+11)=R_RMS
              CALL CTAU_HOT(LJETS,HIET,HIETA,HIPHI)
              Q(LPTAU+12) = HIET(1)
              Q(LPTAU+13) = HIET(2)
              Q(LPTAU+22) = HIET(3)
              Q(LPTAU+23) = HIET(4)
              DO 101 IHOT = 1, 4
                HIINFO(WORD1) = HIETA(IHOT)
                HIINFO(WORD2) = HIPHI(IHOT)
                IQ(LPTAU+23+IHOT) = HIDATA
  101         CONTINUE
C              CALL CTAU_POS(LJETS,TAUPOS)
C              CALL UCOPY(TAUPOS,Q(LPTAU+16),3)
              IQ(LPTAU+15) = IQ(LJETS+26)
              IPHI = 64*PHI/TWOPI
              IETA = PETA_TO_DETA(ETA,ZV)*10
              DO 100 IW = 1, 3
                CALL CSUME_WINDOW(IPHI,IETA,IW,SUME)
                Q(LPTAU+15+IW) = SUME
  100         CONTINUE
              NEWPI = PI
              DO 200 IR = 1, 3
                NTRK = 0
                DELTAA = REGION(IR) * NEWPI / 180.0
                PHIMIN = PHI - DELTAA
                PHIMAX = PHI + DELTAA
                THMIN = TH - DELTAA
                THMAX = TH + DELTAA
                CALL NCROAD(ZV,PHIMIN,PHIMAX,THMIN,THMAX,NTRK,IDC)
C                IF (NTRK .LE. 0) THEN
C                  CALL ZTRK_IN_ROAD
C     &              (ZV,PHIMIN,PHIMAX,THMIN,THMAX,NTRK,ZLINK)
C                ENDIF
                IQ(LPTAU+18+IR) = NTRK
  200         CONTINUE
              CALL CLEANTAU(LPTAU,STATUS,OK)
              IF (OK) IQ(LPTAU+14) = STATUS
              CALL GET_TAU_QUAN(LPTAU,PROB,CHISQ,FVAR)
              Q(LPTAU+28) = CHISQ(1)
              Q(LPTAU+29) = CHISQ(2)
              Q(LPTAU+30) = FVAR
              IF (NEW_RMS .GT. 99.9) THEN
                CALL CJET_NEWRMS(LJETS,THRESHOLD,NEW_RMS,OK)
                IF (OK) THEN
                  Q(LPTAU+31) = NEW_RMS
                  Q(LPTAU+32) = THRESHOLD
                ENDIF
              ELSE
                Q(LPTAU+31) = NEW_RMS
                Q(LPTAU+32) = THRESHOLD
              ENDIF

C
C       flag the bank if it is built by new_rms
C
              CALL MVBITS(USE_NEW_RMS,0,2,IQ(LPTAU),0)   
              IF (VRFFLG) THEN
                CALL HFILL(336,Q(LPTAU+28),0.,1.)
                CALL HFILL(337,Q(LPTAU+29),0.,1.)
                CALL HFILL(338,Q(LPTAU+30),0.,1.)
                CALL HFILL(340,Q(LPTAU+31),Q(LPTAU+11),1.)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        LJETS=LQ(LJETS)  ! pointer to next jet
      ENDDO
C
  999 RETURN
      END

