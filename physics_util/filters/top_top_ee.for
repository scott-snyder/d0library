      LOGICAL FUNCTION TOP_TOP_EE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Purpose and Methods : Filter ttbar-->ee events from ELF stream
C-                         according to the following criteria
C-     - PELC  with pt > ELEC_MIN_ET
C-     - PPHO  with pt > PHOT_MIN_ET
C-     - both em clusters within abs(physics eta) <= em_max_eta
C-     - if SELECT_EM_FILTERS true then use only filters with names
C-       specified by the array EM_FILT_NAMES
C-
C-   Controls: TOP_TOP_EE_RCP
C-   Returned value  : true for events that pass
C-
C-   Created  17-DEC-1992   Meenakshi Narain
C-   Updated  14-JAN-1993   Meenakshi Narain  Add selection on EM filters
C-   Modified 15-JUL-1993   Pushpa C. Bhat  Remove Missing Et cut 
C-   Updated  24-JUL-1993   Pushpa C. Bhat  Fix EZGET of FILTNAMES 
C-   Updated  Oct-17-1994   Bob Kehoe -- add cut on maximum eta of em clusters
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,IER
      INTEGER LPELC,LPPHO
      INTEGER GZVERH
      INTEGER GZPELC,GZPPHO
      INTEGER NELEC,NPHOT,NELPH
      REAL    PHOT_ET,ELC_ET,ET
      REAL    ELETMIN, PHETMIN
      real pho_eta,ele_eta,em_max_eta
      LOGICAL FIRST,FLAG_ELEC,FLAG_PHOT
      LOGICAL PASSED,SELECT_EM_FILTERS
      LOGICAL OK, PASS_L2, MATCH_WILD
      INTEGER NTRIGON,NFILTON,LEN1,TBIT_ON(32),FBIT_ON(128)
      INTEGER NFSTRING_REQ,STATUS
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 TSTRING_REQ(32),FSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING

      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_TOP_EE_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_TOP_EE_RCP')
          IF (IER.EQ.0) CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('em_max_eta',em_max_eta,IER)
          IF (IER.EQ.0) CALL
     &      EZGET('SELECT_EM_FILTERS',SELECT_EM_FILTERS,IER)
          IF (SELECT_EM_FILTERS) THEN
            CALL EZ_GET_CHARS('EM_FILTNAMES',NFSTRING_REQ,FSTRING_REQ,
     &        IER)
          ENDIF
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_TOP_EE','TOP_TOP_EE_RCP',
     &        'ERROR GETTING TOP_TOP_EE RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('TOP_TOP_EE','TOP_TOP_EE_RCP',
     &      'ERROR READING TOP_TOP_EE RCP FILE','W')
        ENDIF
      ENDIF
C
C ****  intializations
C
      OK        = .FALSE.
      PASS_L2   = .FALSE.
      PASSED    = .FALSE.
      FLAG_ELEC = .FALSE.
      FLAG_PHOT = .FALSE.
C
C ****  Check whether the event passes an EM filter
C
      IF (SELECT_EM_FILTERS) THEN
C   get names of triggers/filters fired for this event
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C   check whether the event passes an EM filter
        DO I = 1,NFSTRING_REQ
          SEARCH_STRING = FSTRING_REQ(I)
          DO J = 1,NFILTON
            OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
            PASS_L2 = PASS_L2 .OR. OK
          ENDDO
        ENDDO
        IF (.NOT.PASS_L2) GO TO 900
      ENDIF
C
C ****  electrons
C
      NELEC=0
      LPELC=GZPELC()
      ELC_ET=ELETMIN
      IF(LPELC.NE.0) THEN
        DO WHILE (LPELC.GT.0)
          ET=Q(LPELC+7)
          if (et.ge.elc_et) then
            ele_eta = q(lpelc + 19)
            if (abs(ele_eta).le.em_max_eta) then
              nelec = nelec + 1
              flag_elec = .true.
            endif
          endif
          LPELC=LQ(LPELC)
        ENDDO
      ENDIF
C
C ****  photons
C
      NPHOT=0
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          if (et.ge.phot_et) then
            pho_eta = q(lppho + 19)
            if (abs(pho_eta).le.em_max_eta) then
              nphot = nphot + 1
              flag_phot = .true.
            endif
          endif
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
      ENDIF
C
C ****  Write out event?
C
      NELPH = NELEC + NPHOT
      IF (NELPH.GE.2) PASSED = .TRUE.
C
  900 CONTINUE
C
      TOP_TOP_EE = PASSED
C
  999 RETURN
      END
