      SUBROUTINE TOP_LEPTONS_FIND_ZEE(IFGOOD,NOMU,NOEL,NOPH,NOJT,
     1  MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for Z0->e e 
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
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isnt a candidate
C-
C-   Controls: None
C-
C-   Created  29-Jan-1993   Stephen J. Wimpenny
C-   Modified 11-Feb-1993   Skeletal finder implemented
C-   Modified 15-Mar-1993   Name changes for Good_Electron and Good_Photon
C-                          logicals
C-   Modified 17-Jul-1993   Use new isolated electron logic   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL FIRST,IFGOOD,CORR_JETS,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER NOMU,NOEL,NOPH,NOJT,IER
      INTEGER LPELC,LPPHO,LJETS,GZPELC,GZPPHO,GZJETS
      INTEGER LPELC_VEC(5),LPPHO_VEC(5),LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_JT
      INTEGER I_EL_ISOL,I_PH_ISOL,LPELC_VEC_ISOL(5),LPPHO_VEC_ISOL(5)
C
      REAL MET_VEC(3)
C
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_ZEE',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_EL=0
      I_PH=0
      I_JT=0
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
C
C *** Get PELC Bank pointers
C
      IF(NOEL.LT.1) GO TO 50
C
      IF(NOEL.GT.5) CALL ERRMSG('Electron Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_ZEE',' ','W')
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
     1 'TOP_LEPTONS_FIND_ZEE',' ','W')
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
C *** Look for isolated electrons above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.2) GO TO 200
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C
  200 CONTINUE
C
C *** electron+photon selection -> require at least 1 'good' PELC
C *** bank + 1 'good' PPHO bank
C
      IF(I_EL.LT.1) GO TO 300
      IF(I_PH.LT.1) GO TO 999
C
C *** Look for an isolated electron above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.1) GO TO 300
C
C *** and an isolated photon
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C
  300 CONTINUE
C
C *** di-photon selection
C
      IF(I_PH.LT.2) GO TO 999
C
C *** Look for two isolated photons above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** Good candidate
C
      IFGOOD=.TRUE.
C
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN

      END
