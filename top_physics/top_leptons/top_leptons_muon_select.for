      SUBROUTINE TOP_LEPTONS_MUON_SELECT(NOMU,NOMU_UNCUT,NOJT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over PMUO banks and flag banks with
C-                         muon candidates which we do not want to
C-                         consider further. Also Pt orders the
C-                         remaining banks
C-
C-   Inputs  : NOJT       - No of Jets banks surviving after electron and
C-                          photon selection.
C-             
C-   Outputs : NOMU       - No of PMUO candidates after cuts
C-             NOMU_UNCUT - No of PMUO banks before selection
C-
C-             Loads CLEANMU STATUS word into word 45 of PMUO.
C-               ( If bit is set => track failed this cut )
C-             Bits 1 to 22 are currently used by s/r CLEANMU. This routine
C-             sets the following additional bits :
C-
C-         Bit No.           Cut
C-           26,27    Failed at least one selection/id cut
C-           28       Minimum Rapidity
C-           29       Maximum Rapidity
C-           30       Pt min
C-           31       Pt max
C-           32       Spare
C-
C-   Controls: None
C-
C-   Created  15-JUL-1992   Stephen J. Wimpenny
C-   Modified 28-Sep-1992   Calls MUON_SELECT to do Cosmic rejection
C-                          and track validation
C-   Modified  5-Oct-1992   Min rapidity cut added (for tech. studies).
C-   Modified 28-Jan-1993   Works with both old and new PMUO Formats
C-   Modified 22-Jul-1993   Calorimeter dE/dx correction implemented for
C-                          isolated muons.
C-   Renovated 1-Apr-1994   Uses new CLEANMU package
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,CORR_MU,OK,OK_NEW
C
      INTEGER NOMU,NOMU_UNCUT,I_SET,I_RESET,PMUO_VERS
      INTEGER IER,IOFF,ICOR,LPMUO,GZPMUO,NOJT,STATUS
      INTEGER N,MUON_MASK(32),STATUS_NEW,JBIT
C
      REAL ETA_MIN,ETA_MAX,PT_MIN,PT_MAX
C
      DATA FIRST/.TRUE./
C
      OK=.TRUE.
      IF(FIRST) THEN
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Zebra Bank 'cuts'
C
        CALL EZGET('PMUO_ETAMIN',ETA_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('PMUO_ETAMAX',ETA_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('PMUO_PTMIN',PT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('PMUO_PTMAX',PT_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('MU_CORR',CORR_MU,IER)
        IF(IER.EQ.0) CALL EZGETA('MUON_MASK_1',0,0,0,N,IER)
        IF(IER.EQ.0) THEN
          IF(N.NE.32) THEN
            CALL ERRMSG('Error reading Golden Muon Mask',
     1        'TOP_LEPTONS_MUON_SELECT',' ','F')
          ENDIF
          CALL EZGETA('MUON_MASK_1',1,N,1,MUON_MASK,IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_MUON_SELECT',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      I_SET=1
      I_RESET=0
      NOMU=0
      NOMU_UNCUT=0
C
C *** Look for muon candidates
C
      LPMUO=GZPMUO(0)
      IF(LPMUO.NE.0) THEN
C
C *** Get Bank Version No.
C
        IOFF=0
        PMUO_VERS=IQ(LPMUO+1)
        IF(PMUO_VERS.GE.3) IOFF=1
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LPMUO,14)
        LPMUO=GZPMUO(0)
        CALL ZTOPSY(IXCOM,LPMUO)
        LPMUO=GZPMUO(0)
C
C *** Sort thro and look for good muons
C
        DO WHILE (LPMUO.GT.0)
C
C *** Check PMUO bank muon track candidates (s/r CLEANMU) and then do analysis
C *** orientated 'physics' selection cuts
C
          NOMU_UNCUT=NOMU_UNCUT+1
C
C *** Do muon id and Cosmic Ray cuts
C
          CALL CLEANMU(LPMUO,STATUS,OK)
C
C *** Loop over STATUS and test only requested bits
C
          STATUS_NEW=0
          OK_NEW=.TRUE.
          DO N=1,32
            IF(MUON_MASK(N).EQ.1) THEN
              IF(JBIT(STATUS,N).EQ.1) THEN
                CALL SBIT(I_SET,STATUS_NEW,N)
                OK_NEW=.FALSE.
              ENDIF
            ENDIF
          ENDDO
C
C *** Start Track selection 'Physics' Cuts
C ***   .... min rapidity value
C
          IF(ETA_MIN.GT.1.0E-4) THEN
            IF(ABS(Q(LPMUO+16)).LT.ETA_MIN) THEN
              CALL SBIT(I_SET,STATUS_NEW,28)
              OK_NEW=.FALSE.
            ENDIF
          ENDIF
C
C ***   .... max rapidity range
C
          IF(ABS(Q(LPMUO+16)).GT.ETA_MAX) THEN
            CALL SBIT(I_SET,STATUS_NEW,29)
            OK_NEW=.FALSE.
          ENDIF
C
C *** Pt min/max
C *** first do muon dE/dx correction - if appropriate
C
          IF(CORR_MU) THEN
            CALL TOP_LEPTONS_MUON_DEDX_CORR(LPMUO,NOJT,ICOR)
          ENDIF
C
C *** Now do cuts
C
          IF(Q(LPMUO+14).LT.PT_MIN) THEN
            CALL SBIT(I_SET,STATUS_NEW,30)
            OK_NEW=.FALSE.
          ENDIF
          IF(Q(LPMUO+14).GT.PT_MAX) THEN
            CALL SBIT(I_SET,STATUS_NEW,31)
            OK_NEW=.FALSE.
          ENDIF
C
C *** If track has been rejected by the selection cuts then
C *** set global reject flag (PMUO versions 1,2) or Golden and
C *** Silver Muon Accept flags (PMUO versions 3 on)
C
          IF(.NOT.OK_NEW) THEN
C
C *** Track failed id cuts
C
            IF(PMUO_VERS.LT.3) THEN
              CALL SBIT(I_SET,IQ(LPMUO+44),17)
            ELSE
              CALL SBIT(I_RESET,STATUS_NEW,26)
              CALL SBIT(I_RESET,STATUS_NEW,27)
              IQ(LPMUO+45)=STATUS_NEW
            ENDIF
            GO TO 10
          ELSE
C
C *** track passed id cuts
C
            IF(PMUO_VERS.LT.3) THEN
              CALL SBIT(I_RESET,IQ(LPMUO+44),17)
            ELSE
C              write(12,8888) OK_NEW,STATUS_NEW
              CALL SBIT(I_SET,STATUS_NEW,26)
              CALL SBIT(I_SET,STATUS_NEW,27)
              IQ(LPMUO+45)=STATUS_NEW
            ENDIF
          ENDIF
C
C *** OK we have a possible good one !
C
          NOMU=NOMU+1
   10     LPMUO=LQ(LPMUO)
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
