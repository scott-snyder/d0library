      SUBROUTINE TOP_LEPTONS_JET_SELECT(NOEL,NOPH,NOJT,NOJT_UNCUT,
     1  MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loop over JETS banks and drop banks with
C-                         jet candidates which we do not want to
C-                         consider further. Also Et orders the
C-                         remaining banks and drops 'electron' and
C-                         'photon' jets
C-
C-   Inputs  : NOEL      - no. of electron candidates found by the
C-                         electron selection
C-             NOPH      - no. of photon candidates found by the
C-                         photon selection
C-   RCP var : ETA_MAX   - max eta
C-             PT_MIN    - min Pt
C-             PT_MAX    - max Pt
C-             ETMIN_ALG - min Et for jet algorithm
C-             JET_ALG   - Jet algorithm
C-
C-   Outputs : NOJT          - No of JETS candidates after cuts
C-             NOJT_UNCUT    - No of JETS banks before selection
C-             CONE_SIZE_ALG - Cone size of selected jet algorithm
C-                             (-1. if NN is being used)
C-   Controls:
C-
C-   Created  15-JUL-1992   Stephen J. Wimpenny
C-   Updated  14-SEP-1992   Meenakshi Narain  CLEAN UP RCP variable
C-                                            handling mechanism
C-   Modified 24-Sep-1992   Flags rejected Banks using upper end bits of
C-                          word 15 (ie. bits 17-32)
C-   Modified 29-Sep-1992   Fix bug in but setting logic
C-   Modified 15-Mar-1993   Name changes in Good_Electron and Good_Photon
C-                          logicals
C-   Modified 29-Apr-1993   Jet Energy Corrections and Hot Cell Cuts
C-                          Added
C-   Modified 16-Jun-1993   Missing Et correction implemented
C-   Modified 11-Jul-1993   Electron/Photon match moved to s/r 
C-                          TOP_LEPTONS_EM_JET_MATCH
C-   Modified 29-Jul-1993   Format of calls to SET CAPH modified
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
C
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      LOGICAL FIRST,CORR_JETS,DO_HOTCELL_CUTS
      LOGICAL TOP_LEPTONS_GOOD_PHOTON
      LOGICAL TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER JET_ALG,IER,I_SET,I_RESET
      INTEGER I,ITEMP,IMATCH,IKILL(5),IOBJ
      INTEGER NOJT,NOJT_UNCUT,NOEL,NOPH
      INTEGER GZJETS,LJETS
C
      REAL NEW_E,NEW_PX,NEW_PY,NEW_PZ
      REAL ETA_MAX,PT_MIN,PT_MAX,JET_PHI,JET_ET,JET_ETA
      REAL CONE_TEMPLATE_7(3),CONE_TEMPLATE_5(3)
      REAL CONE_TEMPLATE_3(3),NN_TEMPLATE(5)
      REAL CONE_SIZE_ALG,CONE_SIZE(3),MET_VEC(3)
C
      DATA CONE_SIZE/ 0.7, 0.5, 0.3/
      DATA CONE_TEMPLATE_7/ 1., 6., 0.7/
      DATA CONE_TEMPLATE_5/ 1., 6., 0.5/
      DATA CONE_TEMPLATE_3/ 1., 6., 0.3/
      DATA NN_TEMPLATE    / 2., 7., 2., 8., 2./
      DATA FIRST /.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        IER = 0
C
C *** Get all latest parameter/Options Values
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Jet Algorithm 
C
        CALL EZGET('JETS_ALGORITHM',JET_ALG,IER)
C
C *** Jets --- JETS
C
        IF (IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETA_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_PTMIN',PT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_PTMAX',PT_MAX,IER)
C
C *** Jet energy corrections
C
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
C
C *** Hot Cell cuts
C
        IF (IER.EQ.0) CALL EZGET('JETS_KILL_HOT_CELLS',DO_HOTCELL_CUTS,
     1    IER)
C
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_JET_SELECT',' ','F')
        CALL EZRSET
      ENDIF
C
C *** setup appropriate JETS pointers
C
      IF(JET_ALG.EQ.1) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_7,IER)
        CONE_SIZE_ALG=CONE_SIZE(1)
      ELSEIF(JET_ALG.EQ.2) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_5,IER)
        CONE_SIZE_ALG=CONE_SIZE(2)
      ELSEIF(JET_ALG.EQ.3) THEN
        CALL SET_CAPH('CONE_JET',CONE_TEMPLATE_3,IER)
        CONE_SIZE_ALG=CONE_SIZE(3)
      ELSEIF(JET_ALG.EQ.4) THEN
        CALL SET_CAPH('NN_JET',NN_TEMPLATE,IER)
        CONE_SIZE_ALG=-1.
      ELSE 
        WRITE(12,1000) JET_ALG
        CALL ERRMSG('Illegal Jet Algorithm',
     1    'TOP_LEPTONS_JET_SELECT',' ','F')
      ENDIF
C
      I_SET=1
      I_RESET=0
      NOJT=0
      NOJT_UNCUT=0
      IOBJ=1
C
C *** Look for jet candidates
C
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
C
C *** Order Banks in Decreasing Pt
C
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
C
C *** Sort thro and look for good jets
C
        DO WHILE (LJETS.GT.0)
          ITEMP=0
          NOJT_UNCUT=NOJT_UNCUT+1
C
C *** Re-set selection bits in case they have been set in a
C *** previous pass of the code
C
          DO I=17,32
            CALL SBIT(I_RESET,IQ(LJETS+15),I)
          ENDDO
C
C *** Reject Jet-Photon and Jet-ELectron matches
C
          CALL TOP_LEPTONS_EM_JET_MATCH(NOEL,NOPH,LJETS,IMATCH)
          IF(IMATCH.GE.0) THEN
            CALL SBIT(I_SET,IQ(LJETS+15),19)
            ITEMP=ITEMP+1
          ENDIF  
C
C *** Hot Cell Cuts - set bit 22 if jet is flagged as bad
C
          IF(DO_HOTCELL_CUTS) THEN
            CALL TOP_LEPTONS_HOT_CELL(LJETS,IKILL,IER)
            IF(IER.NE.0) THEN
              CALL SBIT(I_SET,IQ(LJETS+15),22)
              ITEMP=ITEMP+1
              CALL VSUB(MET_VEC(1),Q(LJETS+2),MET_VEC(1),3)
            ENDIF
          ENDIF
C
C *** Jet Energy Corrections 
C
          IF(CORR_JETS) THEN
C
C *** skip energy correction calculation for photons
C *** and electrons
C
            IF(ITEMP.NE.0) THEN
              JET_ETA=Q(LJETS+9)
              JET_ET=Q(LJETS+6)
            ELSE
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS,NEW_E,JET_ET,NEW_PX,
     1          NEW_PY,NEW_PZ,JET_PHI,JET_ETA,IER)
              IF(IER.LT.0) THEN
                JET_ETA=Q(LJETS+9)
                JET_ET=Q(LJETS+6)
              ENDIF
            ENDIF
          ELSE
            JET_ETA=Q(LJETS+9)
            JET_ET=Q(LJETS+6)
          ENDIF
C
C *** Flag JETS banks with candidates which fail jet selection cuts
C *** start with max eta cut. Bit 20 of word 15.
C
          IF(ABS(JET_ETA).GT.ETA_MAX) THEN
            CALL SBIT(I_SET,IQ(LJETS+15),20)
            ITEMP=ITEMP+1
          ENDIF
C
C *** Pt cuts (Bit 21 of word 15)
C
          IF(JET_ET.LT.PT_MIN.OR.Q(LJETS+6).GT.PT_MAX) THEN
            CALL SBIT(I_SET,IQ(LJETS+15),21)
            ITEMP=ITEMP+1
          ENDIF
C
C *** Check that we have passed all cuts
C *** set bit 17 if jets bank fails any cut
C
          IF(ITEMP.GT.0) THEN
            CALL SBIT(I_SET,IQ(LJETS+15),17)
            GO TO 10
          ENDIF
C
C *** OK we have a possible good one !
C
          NOJT=NOJT+1
   10     LJETS=LQ(LJETS)
        ENDDO
      ENDIF
C
C *** Reset CAPH algorithm
C
      CALL RESET_CAPH
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(//,' ==> TOP_LEPTONS_JET_SELECT - Illegal Jet Algorithm',
     1 ' Requested : Type = ',I2,' <==',//)
      END
