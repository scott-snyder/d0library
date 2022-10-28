      SUBROUTINE TOP_LEPTONS_NTUPLE_JETFIN(JET,HT,SPHER,PLAN,GSPHER,
     1  GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To find 5 highest Et jets and construct JET
C-
C-   Inputs  : none
C-   Outputs : jet vector JET
C-   Controls: 
C-
C-   Created  16-SEP-1991   Jim Cochran
C-   modified  1-SEP-1992   to be used in TOP_LEPTONS package - jc
C-   Modified 18-Sep-1992   Modified to use Jet logical
C-   Modified 16-Nov-1992   HT Calculation added + eta-phi swop fixed
C-   Modified 15-Mar-1993   Name changes for Good_Jet logical
C-                          and routine name
C-   Modified 29-Apr-1993   Jet corrections added
C-   Modified 4-Dec-1993    Re-order Jets after correction
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL TOP_LEPTONS_GOOD_JET,FIRST,CORR_JETS
C
      INTEGER I,J,LJETS,GZJETS,NJETS,IER,IERR,LJETS_VEC(10),ETTAG
C
      REAL JET(0:5,8),ETAJT_MAX,PTJT_MIN
      REAL EX,EY,EZ,ET,E,ETA,PHI
      REAL HT,HT_JETPT_MIN,HT_JETETA_MAX
      REAL SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH
C
      DATA FIRST/.TRUE./
C
C *** Read cut parameters for HT from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('JETS_ETAMAX',ETAJT_MAX,IER)
        IF(IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',HT_JETPT_MIN,IER)
        IF(IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',HT_JETETA_MAX,IER)
        IF(IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        CALL EZRSET
        IF(IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     1    'TOP_LEPTONS_NTUPLE_JETFIN',' ','F')
C
C *** Check consistency of HT selection and Jet preselection
C *** - reset limits if necessary
C
        IF(HT_JETPT_MIN.LT.PTJT_MIN) HT_JETPT_MIN=PTJT_MIN
        IF(HT_JETETA_MAX.GT.ETAJT_MAX) HT_JETETA_MAX=ETAJT_MAX
        FIRST=.FALSE.
      ENDIF
C
C *** Initialize jet entries
C
      DO J=0,5
        DO I=1,8
          JET(J,I)= -9.
        ENDDO
      ENDDO
C
      LJETS=GZJETS()
C
C *** If no jets found, set flags accordingly
C
      IF (LJETS .EQ. 0) THEN            ! No JETS banks (-6)
        DO J=0,5
          DO I=1,8
            JET(J,I) = -6.
          ENDDO
        ENDDO
C
      ELSEIF(LJETS.NE.0) THEN
C
C *** loop over all jets
C *** and look for Banks flagged as good
C
        NJETS=0
        HT=0.
        DO WHILE (LJETS.GT.0)
          IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
            NJETS=NJETS+1  
            IF(NJETS.LT.11)THEN
              LJETS_VEC(NJETS) = LJETS
            ENDIF
            IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS,E,ET,EX,EY,EZ,
     1          PHI,ETA,IER)
              IF(IER.LT.0) THEN
                DO I=1,6
                  JET(0,I)=Q(LJETS+I+1)
                ENDDO
                JET(0,7)=Q(LJETS+9)
                JET(0,8)=Q(LJETS+8)
                IF(Q(LJETS+6).GE.HT_JETPT_MIN.AND.
     1           ABS(Q(LJETS+9)).LT.HT_JETETA_MAX) THEN
                  HT=HT+Q(LJETS+6)
                ENDIF
              ELSE
                JET(0,1)=EX
                JET(0,2)=EY
                JET(0,3)=EZ
                JET(0,4)=E
                JET(0,5)=ET
                JET(0,6)=Q(LJETS+7)
                JET(0,7)=ETA
                JET(0,8)=PHI
                IF(ET.GT.HT_JETPT_MIN .AND. ETA.LT.HT_JETETA_MAX) THEN
                  HT = HT + ET
                ENDIF
              ENDIF
            ELSE
              DO I=1,6
                JET(0,I)=Q(LJETS+I+1)
              ENDDO
              JET(0,7)=Q(LJETS+9)
              JET(0,8)=Q(LJETS+8)
              IF(Q(LJETS+6).GE.HT_JETPT_MIN.AND.
     1         ABS(Q(LJETS+9)).LT.HT_JETETA_MAX) THEN
                HT=HT+Q(LJETS+6)
              ENDIF
            ENDIF
            ETTAG = 5
            CALL TOP_LEPTONS_NTUPLE_MAX5(JET,ETTAG)
          ENDIF
          LJETS=LQ(LJETS)  ! pointer to next jet
        ENDDO
      ENDIF
C
C *** Order jets in NTUPLE; current orderiong may be incorrect
C *** because of jet energy corrections
C
C      CALL TOP_LEPTONS_REAL_MATRIX_SORT(JET,8,NJETS,5)
C
      CALL TOP_LEPTONS_EVENT_SHAPE(NJETS,LJETS_VEC,SPHER,PLAN,
     1                 GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH,IERR)
C
      IF (LJETS.GT.0 .AND. NJETS.LT.5) THEN
        DO J=NJETS+1,5
          DO I=1,8
            JET(J,I)= -3.
          ENDDO
        ENDDO
      ENDIF
C
  999 RETURN
      END
