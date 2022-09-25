      SUBROUTINE CLEANTAU(LPTAU,STATUS,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Imposes quality cuts to identify tau
C-   
C-   Inputs  : LPTAU(I):   bank address for PTAU bank
C-   Outputs : STATUS(I):  bit pattern (0/1) indicating which cuts
C-                         were satisfied/failed
C-             OK(I):      True if tau passes all the quality cuts
C-                         ALWAYS TRUE if no user cuts specified
C-
C-             STATUS BIT   0 : RMS tighter cut
C-             STATUS BIT   1 : E_Window(5x5)/E cut
C-             STATUS BIT   2 : E_Window(3x3)/E cut
C-             STATUS BIT   3 : Isolation cut (Et(0.7) - Et(0.2))/Et(0.2)
C-             STATUS BIT   4 : Isolation cut (E(0.7) - E(0.2))/E(0.2)
C-             STATUS BIT   5 : Isolation cut (Et(0.6) - Et(0.2))/Et(0.2)
C-             STATUS BIT   6 : Isolation cut (E(0.6) - E(0.2))/E(0.2)
C-             STATUS BIT   7 : if associated with e, Chi_2 cut
C-             STATUS BIT   8 : if associated with e, Et(JNEP)/Et cut
C-             STATUS BIT   9 : cut on # of cells above 1 GeV
C-             STATUS BIT  10 : cut on # of towers has 90% jet Et
C-             STATUS BIT  11 : cut on ratio of hotest to next_hotest cell
C-             STATUS BIT  12 : cut on Fraction of ICD/MG Et 
C-             STATUS BIT  13 : cut on Fraction of CH Et 
C-             STATUS BIT  14 : cut on Fraction of EM Et (low limit)
C-             STATUS BIT  15 : cut on Fraction of EM Et (high limit)
C-             STATUS BIT  16 : (Et1+Et2)/Et cut
C-             STATUS BIT  17 : (Et1+Et2+Et3)/Et cut
C-             STATUS BIT  18 : (Et1+Et2+Et3+Et4))/Et cut
C-             STATUS BIT  19 : (Et1+...+Etn))/Et cut
C-                              (n: number of consective towers among the hotest
C-                               4 towers. 1=<n<=4)
C-             STATUS BIT  20 : Chi square cut for signal h-matrix
C-             STATUS BIT  21 : Chi square cut for background h-matrix
C-             STATUS BIT  22 : cut on Fisher variable
C-                          ...
C-             STATUS BIT  28 : # tracks cut
C-             STATUS BIT  29 : # CDC tracks in 10 degree cone
C-             STATUS BIT  30 : # CDC tracks between 10-20 degree ring
C-             STATUS BIT  31 : # CDC tracks between 10-30 degree ring
C-
C-   Created  16-OCT-1993   Qizhong Li-Demarteau
C-   Updated  21-JAN-1995   Qizhong Li-Demarteau  added more bits 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER  LPTAU, STATUS
      LOGICAL  OK, FIRST, EZERROR
      INTEGER  LRCP, IER, LJETS, LPELC, LJNEP, LHMTE
      INTEGER  NCELL, NCELL_MAX, N90, N90_MAX, NV
      INTEGER  NTRK, NTRK_MAX, NTRK10, NTRK20, NTRK30, NTRK10_20
      INTEGER  NTRK10_MAX, NTRK10_20_MAX, NTRK10_30_MAX, NTRK10_30
      INTEGER  IETAC, NVERSION
      REAL     RMS_MAX, W3RATE_MIN, W5RATE_MIN
      REAL     ISO_RAD1, ISO_RAD2, ISO_RAD3, CORE_RAD1, CORE_RAD2
      REAL     ISO_CUT1, ISO_CUT2, ISO_CUT3, ISO_CUT4
      REAL     ISO1, ISO2, ISO3, ISO4
      REAL     ET, RMS, ET1, ET2, ET3, ET4, W3RATE, W5RATE
      REAL     CONE_RAD(5), TAUETA, TAUPHI
      REAL     E_CONE(5), ET_CONE(5), EM_CONE(5)
      REAL     JNEPET, JNEPET_RATE, JNEPET_RATE_MIN
      REAL     E_CHISQ, E_CHISQ_MIN
      REAL     RHOT, RHOT_MAX, RTICD, RTICD_MAX, RTCH, RTCH_MAX
      REAL     RTEM, RTEM_MIN, RTEM_MAX, ET2RATE, ET2RATE_MIN
      REAL     ET3RATE, ET3RATE_MIN, ET4RATE, ET4RATE_MIN
      REAL     ZV, ZVTX_INFO(3,1)
      REAL     PETA_TO_DETA
      CHARACTER*(*) RCPFIL 
      PARAMETER( RCPFIL = 'CLEANTAU_RCP' )   ! Logical name of RCP file
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C  read CLEANTAU_RCP, if it has not been read yet
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC(RCPFIL,LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP(RCPFIL,IER)
          OK = IER .EQ. 0
          IF (.NOT. OK) CALL ERRMSG('CTAUS','CLEANTAU',
     &      'Reading CLEANTAU_RCP failed','F')
        ENDIF
C
C    get all the parameters from the CLEANTAU_RCP
C
        CALL EZPICK('CLEANTAU_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('CTAUS','CLEANTAU',     
     &      'Unable to find bank CLEANTAU_RCP','F')
          GOTO 999
        ENDIF
        CALL EZGET('RMS_MAX',RMS_MAX,IER)
        CALL EZGET('WINDOW_3_RATE_MIN',W3RATE_MIN,IER)
        CALL EZGET('WINDOW_5_RATE_MIN',W5RATE_MIN,IER)
        CALL EZGET('CORE_RAD1',CORE_RAD1,IER)
        CALL EZGET('CORE_RAD2',CORE_RAD2,IER)
        CALL EZGET('ISOLATION_RAD1',ISO_RAD1,IER)
        CALL EZGET('ISOLATION_RAD2',ISO_RAD2,IER)
        CALL EZGET('ISOLATION_RAD3',ISO_RAD3,IER)
        CALL EZGET('ISO_CUT1',ISO_CUT1,IER)
        CALL EZGET('ISO_CUT2',ISO_CUT2,IER)
        CALL EZGET('ISO_CUT3',ISO_CUT3,IER)
        CALL EZGET('ISO_CUT4',ISO_CUT4,IER)
        CALL EZGET('E_CHISQ_MIN',E_CHISQ_MIN,IER)
        CALL EZGET('JNEPET_RATE_MIN',JNEPET_RATE_MIN,IER)
        CALL EZGET_i('NCELL_MAX',NCELL_MAX,IER)
        CALL EZGET_i('N90_MAX',N90_MAX,IER)
        CALL EZGET('RHOT_MAX',RHOT_MAX,IER)
        CALL EZGET('RTICD_MAX',RTICD_MAX,IER)
        CALL EZGET('RTCH_MAX',RTCH_MAX,IER)
        CALL EZGET('RTEM_MAX',RTEM_MAX,IER)
        CALL EZGET('RTEM_MIN',RTEM_MIN,IER)
        CALL EZGET('ET2RATE_MIN',ET2RATE_MIN,IER)
        CALL EZGET('ET3RATE_MIN',ET3RATE_MIN,IER)
        CALL EZGET('ET4RATE_MIN',ET4RATE_MIN,IER)
        CALL EZGET_i('NTRK_MAX',NTRK_MAX,IER)
        CALL EZGET_i('NTRK10_MAX',NTRK10_MAX,IER)
        CALL EZGET_i('NTRK10_20_MAX',NTRK10_20_MAX,IER)
        CALL EZGET_i('NTRK10_30_MAX',NTRK10_30_MAX,IER)
        CALL EZRSET
      ENDIF
C
      STATUS = 0
      OK = .TRUE.
C
      IF (LPTAU .LE. 0) GOTO 999
C
C set version # (1-15) for cleantau into IQ(LPTAU) bit 0-3
C
      NVERSION = 1
      CALL MVBITS(NVERSION,0,4,IQ(LPTAU),0)
C
      ET = Q(LPTAU + 7)
      IF (ET .LE. 0.0) GOTO 999
      RMS = Q(LPTAU + 11)
      ET1 = Q(LPTAU + 12)
      ET2 = Q(LPTAU + 13)
      ET3 = Q(LPTAU + 22)
      ET4 = Q(LPTAU + 23)
      IF (Q(LPTAU+6) .LE. 0.0) GOTO 999
      W3RATE = Q(LPTAU + 17) / Q(LPTAU+6)
      W5RATE = Q(LPTAU + 18) / Q(LPTAU+6)
C
C     RMS cut
C
      IF (RMS .GT. RMS_MAX) THEN
        STATUS = IBSET(STATUS,0)
      ENDIF
      IF (W5RATE .LT. W5RATE_MIN) THEN
        STATUS = IBSET(STATUS,1)
      ENDIF
      IF (W3RATE .LT. W3RATE_MIN) THEN
        STATUS = IBSET(STATUS,2)
      ENDIF
C
C      Isolation cuts
C
      CONE_RAD(1) = CORE_RAD1
      CONE_RAD(2) = CORE_RAD2
      CONE_RAD(3) = ISO_RAD1
      CONE_RAD(4) = ISO_RAD2
      CONE_RAD(5) = ISO_RAD3
      TAUETA = Q(LPTAU+10)
      TAUPHI = Q(LPTAU+9)
      CALL VERTEX_INFO(1,NV,ZVTX_INFO,OK)
C            Only consider the main primary vertex
      IF ( OK ) THEN
        ZV = ZVTX_INFO(1,1)
      ELSE
        ZV = 0.0
      ENDIF
      IETAC = PETA_TO_DETA(TAUETA,ZV) * 10
      CALL CONE_ISO(IETAC,TAUETA,TAUPHI,CONE_RAD,
     &  E_CONE,ET_CONE,EM_CONE)
      IF (ET_CONE(1) .GT. 0.0) THEN
        ISO1 = (ET_CONE(5) - ET_CONE(1)) / ET_CONE(1)
        IF (ISO1 .GT. ISO_CUT1) THEN
          STATUS = IBSET(STATUS,3)
        ENDIF
        ISO3 = (ET_CONE(4) - ET_CONE(1)) / ET_CONE(1)
        IF (ISO3 .GT. ISO_CUT3) THEN
          STATUS = IBSET(STATUS,5)
        ENDIF
      ELSE
        STATUS = IBSET(STATUS,3)
        STATUS = IBSET(STATUS,5)
      ENDIF
      IF (E_CONE(1) .GT. 0.0) THEN
        ISO2 = (E_CONE(5) - E_CONE(1)) / E_CONE(1)
        IF (ISO2 .GT. ISO_CUT2) THEN
          STATUS = IBSET(STATUS,4)
        ENDIF
        ISO4 = (E_CONE(4) - E_CONE(1)) / E_CONE(1)
        IF (ISO4 .GT. ISO_CUT4) THEN
          STATUS = IBSET(STATUS,6)
        ENDIF
      ELSE
        STATUS = IBSET(STATUS,4)
        STATUS = IBSET(STATUS,6)
      ENDIF
C
C     If this PTAU is associated with PELC
C
      LJETS = LQ(LPTAU - 2)
      IF (LJETS .GT. 0) THEN
        LPELC = LQ(LJETS - 3)
        LJNEP = LQ(LJETS - 2)
        IF (LPELC .GT. 0) THEN
          LHMTE = LQ(LPELC - 1)
          IF (LHMTE .GT. 0) E_CHISQ = Q(LHMTE + 7)
          IF (E_CHISQ .LT. E_CHISQ_MIN) THEN
            STATUS = IBSET(STATUS,7)
          ENDIF
        ENDIF
        IF (LJNEP .GT. 0) THEN
          JNEPET = Q(LJNEP + 6)
          JNEPET_RATE = JNEPET / ET
          IF (JNEPET_RATE .LT. JNEPET_RATE_MIN) THEN
            STATUS = IBSET(STATUS,8)
          ENDIF
        ENDIF
C
C      cut on jet variables
C
        NCELL = IQ(LJETS+16)
        IF (NCELL .GT. NCELL_MAX) THEN
          STATUS = IBSET(STATUS,9)
        ENDIF
        N90 = IQ(LJETS+21)
        IF (N90 .GT. N90_MAX) THEN
          STATUS = IBSET(STATUS,10)
        ENDIF
        RHOT = Q(LJETS+19)
        IF (RHOT .GT. RHOT_MAX) THEN
          STATUS = IBSET(STATUS,11)
        ENDIF
        RTICD = Q(LJETS+17)
        IF (RTICD .GT. RTICD_MAX) THEN
          STATUS = IBSET(STATUS,12)
        ENDIF
        RTCH = Q(LJETS+18)
        IF (RTCH .GT. RTCH_MAX) THEN
          STATUS = IBSET(STATUS,13)
        ENDIF
        RTEM = Q(LJETS+14)
        IF (RTEM .LT. RTEM_MIN) THEN
          STATUS = IBSET(STATUS,14)
        ENDIF
        IF (RTEM .GT. RTEM_MAX) THEN
          STATUS = IBSET(STATUS,15)
        ENDIF
      ENDIF
C
C     ET fraction cuts
C
      ET2RATE = (ET1+ET2)/ET
      IF (ET2RATE .LT. ET2RATE_MIN) THEN
        STATUS = IBSET(STATUS,16)
      ENDIF
      ET3RATE = (ET1+ET2+ET3)/ET
      IF (ET3RATE .LT. ET3RATE_MIN) THEN
        STATUS = IBSET(STATUS,17)
      ENDIF
      ET4RATE = (ET1+ET2+ET3+ET4)/ET
      IF (ET4RATE .LT. ET4RATE_MIN) THEN
        STATUS = IBSET(STATUS,18)
      ENDIF
C
C      track information
C
      NTRK = IQ(LPTAU-3) - 2
      IF (NTRK .GT. NTRK_MAX) THEN
        STATUS = IBSET(STATUS,28)
      ENDIF
      NTRK10 = IQ(LPTAU+19)
      NTRK20 = IQ(LPTAU+20)
      NTRK30 = IQ(LPTAU+21)
      IF (NTRK10 .GT. NTRK10_MAX) THEN
        STATUS = IBSET(STATUS,29)
      ENDIF
      NTRK10_20 = NTRK20 - NTRK10
      IF (NTRK10_20 .GT. NTRK10_20_MAX) THEN
        STATUS = IBSET(STATUS,30)
      ENDIF
      NTRK10_30 = NTRK30 - NTRK10
      IF (NTRK10_30 .GT. NTRK10_30_MAX) THEN
        STATUS = IBSET(STATUS,31)
      ENDIF
C
  999 RETURN
      END
