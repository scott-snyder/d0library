      LOGICAL FUNCTION STREAM_EJETS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Purpose and Methods : Filter ttbar-->ee events from ELF stream
C-                         according to the following criteria
C-     - PELC  with pt > ELEC_MIN_ET
C-     - PPHO  with pt > PHOT_MIN_ET
C-     - if SELECT_EM_FILTERS true then use only filters with names
C-       specified by the array EM_FILT_NAMES
C-
C-   Controls: STREAM_EJETS_RCP
C-   Returned value  : true for events that pass
C-
C-   Created  17-DEC-1992   Meenakshi Narain
C-   Updated  14-JAN-1993   Meenakshi Narain  Add selection on EM filters
C-   Updated  20-JUL-1995   Meenakshi Narain  Loop over vertices etc. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,IER,K
      INTEGER LPELC,LPPHO
      INTEGER GZPELC,GZPPHO
      INTEGER NELEC,NPHOT,NGELEC,NGPHOT
      INTEGER NPASS(10),NPASS_L2EM,NPASS_L2BKG
      INTEGER IUNIT,SSUNIT,NVERT
      REAL    PHOT_ET,ELC_ET,ET
      REAL    ELETMIN, PHETMIN, ETMISS, ETMISSMIN
      REAL    ZVTX_INFO(3, 1)
      REAL    CQUAN(50),CHISQR,EMF,EISOL,ET_NEW(10)
      LOGICAL FIRST,FLAG_MET,OKZ,ETPASS,FLAG_EMFILT,FLAG_BKGFILT
      LOGICAL SELECT_EM_FILTERS,STREAM_EJETS_EOJ
      LOGICAL SELECT_EJETS,SELECT_EJETS_SIG,SELECT_EJETS_BKG
      LOGICAL SELECT_GJETS,SELECT_GJETS_SIG,SELECT_GJETS_BKG
      LOGICAL OK, PASS_L2, MATCH_WILD,STATUS,SELECT_BKG_FILTERS
      INTEGER LPNUT,GZPNUT
      INTEGER NTRIGON,NFILTON,TBIT_ON(32),FBIT_ON(128)
      INTEGER NFSTRING_REQ,NV,NBKGFSTRING_REQ
      INTEGER ISTAT,NVAR
      INTEGER NZTRAKS_PPHO,LZTRK_PPHO,VERTEX_ID
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 FSTRING_REQ(128),FBKGSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('STREAM_EJETS_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('STREAM_EJETS_RCP')
          IF (IER.EQ.0) CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('MISS_MIN_ET',ETMISSMIN,IER)
          IF (IER.EQ.0) CALL
     &      EZGET_l('SELECT_EM_FILTERS',SELECT_EM_FILTERS,IER)
          IF (SELECT_EM_FILTERS) THEN
            CALL EZ_GET_CHARS('EM_FILTNAMES',NFSTRING_REQ,FSTRING_REQ,
     &        IER)
          ENDIF
          IF (IER.EQ.0) CALL
     &      EZGET_l('SELECT_BKG_FILTERS',SELECT_BKG_FILTERS,IER)
          IF (SELECT_BKG_FILTERS) THEN
            CALL EZ_GET_CHARS('BKG_FILTNAMES',
     &        NBKGFSTRING_REQ,FBKGSTRING_REQ,IER)
          ENDIF
          IF (IER.NE.0) THEN
            CALL ERRMSG('SELECT_EVENTS','STREAM_EJETS_RCP',
     &        'ERROR GETTING SELECT_EVENTS RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('SELECT_EVENTS','STREAM_EJETS_RCP',
     &      'ERROR READING SELECT_EVENTS RCP FILE','W')
        ENDIF
        IF (.NOT.SELECT_EM_FILTERS) NPASS_L2EM = -1
        IF (.NOT.SELECT_BKG_FILTERS) NPASS_L2BKG = -1
      ENDIF
C
C ****  intializations
C
      OK        = .FALSE.
      FLAG_MET  = .FALSE.
C set filter flag to true initially to force pass if filter selection is OFF
      FLAG_BKGFILT   = .TRUE.
      FLAG_EMFILT    = .TRUE.
C
C ****  Get Zvertex
C
      CALL VERTEX_INFO(1,NV,ZVTX_INFO,OKZ)
C
C ****  Check whether the event passes an EM OR BKG filter
C
      IF (SELECT_EM_FILTERS.OR.SELECT_BKG_FILTERS) THEN
C
C   get names of triggers/filters fired for this event
C
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C   check whether the event passes an EM filter
        IF (SELECT_EM_FILTERS) THEN
          PASS_L2   = .FALSE.
          DO I = 1,NFSTRING_REQ
            SEARCH_STRING = FSTRING_REQ(I)
            DO J = 1,NFILTON
              OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
              PASS_L2 = PASS_L2 .OR. OK
            ENDDO
          ENDDO
          FLAG_EMFILT  = PASS_L2
          if (FLAG_EMFILT)  NPASS_L2EM = NPASS_L2EM + 1
        ENDIF
        IF (SELECT_BKG_FILTERS) THEN
C   check whether the event passes a BKG filter
          PASS_L2   = .FALSE.
          DO I = 1,NBKGFSTRING_REQ
            SEARCH_STRING = FBKGSTRING_REQ(I)
            DO J = 1,NFILTON
              OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
              PASS_L2 = PASS_L2 .OR. OK
            ENDDO
          ENDDO
          FLAG_BKGFILT = PASS_L2
          if (FLAG_BKGFILT) NPASS_L2BKG = NPASS_L2BKG + 1
        ENDIF
      ENDIF
C
C ****  electrons
C
      NELEC=0
      NGELEC=0
      LPELC=GZPELC()
      ELC_ET=ELETMIN
      IF(LPELC.NE.0) THEN
        DO WHILE (LPELC.GT.0)
          ETPASS= .FALSE.
          ET=Q(LPELC+7)
          IF(ET.GE.ELC_ET) etpass = .true.
          if (etpass) GOTO 50
          if (nv.gt.1) then
            call em_etzv(lpelc,nvert,et_new)
            do k = 1, nvert
              if (et_new(k).ge.ELC_ET) then
                etpass = .true.
                IF(ETPASS) GOTO 50
              endif
            enddo
          endif
          IF(.NOT.ETPASS) GOTO 60
   50     continue
          NELEC=NELEC+1
*          STATUS = GOOD_ELECTRON(LPELC,'LOOSE_NOTRK')
          call cleanem(lpelc,0,status,istat)
          call cleanem_cquans(nvar,cquan)
          chisqr = cquan(4)
          emf = cquan(9)
          eisol = cquan(13)
          status = (chisqr.lt.300.and.eisol.lt.0.3)
          IF (.NOT. STATUS) GOTO 60
          NGELEC= NGELEC+1
   60     CONTINUE
          LPELC=LQ(LPELC)
        ENDDO
      ENDIF
C
C ****  photons
C
      NPHOT=0
      NGPHOT=0
C-
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          ETPASS= .FALSE.
          CALL GET_PPHO_TRACKS(LPPHO,NZTRAKS_PPHO,LZTRK_PPHO,VERTEX_ID)
          IF (NZTRAKS_PPHO.EQ.0) GOTO 80
          IF(ET.GE.PHOT_ET) etpass=.true.
          if (etpass) GOTO 70
          if (nv.gt.1) then
            call em_etzv(lppho,nvert,et_new)
            do k = 1, nvert
              if (et_new(k).ge.PHOT_ET) then
                etpass = .true.
                IF(ETPASS) GOTO 70
              endif
            enddo
          endif
          IF(.NOT.ETPASS) GOTO 80
   70     continue
          NPHOT=NPHOT+1
*          STATUS = GOOD_PHOTON(LPPHO,'LOOSE')
          call cleanem(lppho,0,status,istat)
          call cleanem_cquans(nvar,cquan)
          chisqr = cquan(4)
          emf = cquan(9)
          eisol = cquan(13)
          status = (chisqr.lt.300.and.eisol.lt.0.3)
          IF (.NOT. STATUS) GOTO 80
          NGPHOT= NGPHOT+1
   80     CONTINUE
          LPPHO=LQ(LPPHO)          ! pointer to next photon
        ENDDO
      ENDIF
C-
C
C ****  missing ET
C
  100 CONTINUE
C-
      ETMISS=ETMISSMIN
      LPNUT=GZPNUT(2)
      IF(LPNUT.GT.0) THEN
        ET=Q(LPNUT+7)
        IF(ET.GE.ETMISS) FLAG_MET = .TRUE.
      ENDIF
c-
c-check the corrected missing et also...
c-
      LPNUT=GZPNUT(4)
      IF(LPNUT.GT.0) THEN
        ET=Q(LPNUT+7)
        IF(ET.GE.ETMISS) FLAG_MET = .TRUE.
      ENDIF
C
C ****  Write out event?
C
  900 CONTINUE
C
      STREAM_EJETS = .TRUE.
C
  999 RETURN
C#######################################################################
      ENTRY SELECT_EJETS_SIG

      SELECT_EJETS_SIG = .FALSE.
      IF (NGELEC.GE.1.AND.FLAG_MET.AND.FLAG_EMFILT)  THEN
        SELECT_EJETS_SIG = .TRUE. 
        NPASS(1) = NPASS(1) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY SELECT_EJETS

      SELECT_EJETS = .FALSE.
      IF (NELEC.GE.1.AND.FLAG_MET.AND.FLAG_EMFILT)  THEN
        SELECT_EJETS = .TRUE. 
        NPASS(2) = NPASS(2) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY SELECT_GJETS_SIG

      SELECT_GJETS_SIG = .FALSE.
      IF (NGPHOT.GE.1.AND.FLAG_MET.AND.FLAG_EMFILT)  THEN
        SELECT_GJETS_SIG = .TRUE. 
        NPASS(3) = NPASS(3) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY SELECT_GJETS

      SELECT_GJETS = .FALSE.
      IF (NPHOT.GE.1.AND.FLAG_MET.AND.FLAG_EMFILT) THEN
        SELECT_GJETS = .TRUE. 
        NPASS(4) = NPASS(4) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY SELECT_EJETS_BKG

      SELECT_EJETS_BKG = .FALSE.
      IF (NELEC.GE.1.AND.FLAG_BKGFILT)  THEN
        SELECT_EJETS_BKG = .TRUE. 
        NPASS(5) = NPASS(5) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY SELECT_GJETS_BKG

      SELECT_GJETS_BKG = .FALSE.
      IF (NPHOT.GE.1.AND.FLAG_BKGFILT)  THEN
        SELECT_GJETS_BKG = .TRUE. 
        NPASS(6) = NPASS(6) + 1
      ENDIF
      RETURN
C#######################################################################
      ENTRY STREAM_EJETS_EOJ

      STREAM_EJETS_EOJ = .TRUE.
      IUNIT=SSUNIT()
      WRITE(IUNIT,10)
      WRITE(IUNIT,11) NPASS_L2EM
      WRITE(IUNIT,20) (NPASS(I),I=1,4)
      WRITE(IUNIT,12)
      WRITE(IUNIT,11) NPASS_L2BKG
      WRITE(IUNIT,21) (NPASS(I),I=5,6)
   10 FORMAT(//' Summary of  e+jets streaming '/)
   11 FORMAT(5X,I10,' events pass L2 filter selection '//)
   20 FORMAT(5X,I10,' events pass e+mEt    signal filter ',/
     &       5X,I10,' events pass PELC+mEt signal filter ',/
     &       5X,I10,' events pass g+mEt    signal filter ',/
     &       5X,I10,' events pass PPHO+mET signal filter ',///)
   12 FORMAT(//' Summary of  e+jets background streaming '/)
   21 FORMAT(5X,I10,' events pass PELC selection  ',/
     &       5X,I10,' events pass PPHO selection  ',///)
      RETURN
      END
