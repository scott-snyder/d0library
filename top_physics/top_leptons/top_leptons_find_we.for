      SUBROUTINE TOP_LEPTONS_FIND_WE(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for top->e e 
C-                                  event candidates
C-
C-   Inputs  : 
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-             MISSET_MIN_CALO - minimum missing Et (Calo+ICD+MG)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isnt a candidate
C-
C-   Controls: None
C-
C-   Created  29-JUL-1992   Stephen J. Wimpenny
C-   Modified 17-Sep-1992   Read cuts directly from RCP file
C-   Modified 16-Mar-1993   Name changes for Good_Electron and Good_Photon
C-                          routines
C-   Modified 17-Jul-1993   Uses new isolated electron logic
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,IFGOOD,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER NOMU,NOEL,NOPH,NOJT,IER
      INTEGER LPELC,LPPHO,LPNUT,LJETS,GZPELC,GZPPHO,GZPNUT,GZJETS
      INTEGER LPELC_VEC(5),LPPHO_VEC(5),LJETS_VEC(10),I_EL,I_PH,I_JT
      INTEGER LPELC_VEC_ISOL(5),LPPHO_VEC_ISOL(5),I_EL_ISOL,I_PH_ISOL
C
      REAL MISSET
      REAL MISSET_MIN_CALO,MET_VEC(3)
C
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('PNUT2_ETMIN_W',MISSET_MIN_CALO,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_WE',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_EL=0
      I_PH=0
      I_JT=0
C
C *** Get PNUT Bank pointer
C
      MISSET=0.
      LPNUT=GZPNUT(4)
      IF(LPNUT.LE.0) LPNUT=GZPNUT(2)
      IF(LPNUT.NE.0) THEN
        MISSET=(Q(LPNUT+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT+4)+MET_VEC(2))**2
        MISSET=SQRT(MISSET)
      ENDIF
C
C *** Get PELC Bank pointers
C
      IF(NOEL.LT.1) GO TO 50
C
      IF(NOEL.GT.5) CALL ERRMSG('Electron Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_EE',' ','W')
C
C *** Loop over banks and extract pointers
C
      LPELC=GZPELC()
      DO WHILE(LPELC.NE.0)
        IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
          I_EL=I_EL+1
          IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
C
C *** Get PPHO Bank pointers
C
   50 IF(NOPH.LT.1) GO TO 100
C
      IF(NOPH.GT.5) CALL ERRMSG('Photon Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_EE',' ','W')
C
C *** Loop over banks and extract pointers
C
      LPPHO=GZPPHO()
      DO WHILE(LPPHO.NE.0)
        IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
          I_PH=I_PH+1
          IF(I_PH.LT.6) LPPHO_VEC(I_PH)=LPPHO
        ENDIF
        LPPHO=LQ(LPPHO)
      ENDDO
  100 CONTINUE
C
C *** JETS
C
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.GT.10) I_JT=10
C-----------------------------------------------------------------------
C
C *** W ->e nuselection
C ***  i.) require at least 1 'good' PELC banks
C
      IF(I_EL.LT.1) GO TO 200
C
C *** Look for an isolated electron
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.1) GO TO 200
C
C *** Cut on missing energy
C
      IF(MISSET.LT.MISSET_MIN_CALO) GO TO 999
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
  200 CONTINUE
C
C *** W ->photon nu selection 
C ***  ii.) backup selection -> require at least 1 'good' PPHO banks
C
      IF(I_PH.LT.1) GO TO 999
C
C *** Look for an isolated photon
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** Cut on missing energy
C
      IF(MISSET.LT.MISSET_MIN_CALO) GO TO 999
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
