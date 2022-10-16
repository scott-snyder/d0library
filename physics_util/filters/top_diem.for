      LOGICAL FUNCTION TOP_DIEM
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
C-   Controls: TOP_DIEM_RCP
C-   Returned value  : true for events that pass
C-
C-   Created  31-OCT-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,IER,K
      INTEGER LPELC,LPPHO
      INTEGER GZPELC,GZPPHO
      INTEGER NELEC,NPHOT
      INTEGER NGELEC,NGPHOT,NGELPH,NVERT
      INTEGER ISTAT,NVAR,NPASS(5),IUNIT,SSUNIT
      REAL    CQUAN(50),CHISQR,EMF,EISOL
      REAL    PHOT_ET,ELC_ET,ET,ET_NEW(10)
      REAL    ELETMIN, PHETMIN
      LOGICAL FIRST,OKZ,TOP_DIEM_EOJ,ETPASS
      LOGICAL PASSED,SELECT_EM_FILTERS
      LOGICAL OK, PASS_L2, MATCH_WILD,STATUS
      INTEGER NTRIGON,NFILTON,TBIT_ON(32),FBIT_ON(128)
      INTEGER NFSTRING_REQ,NV
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 FSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING
      REAL    ZVTX_INFO(3, 1)
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_DIEM_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('TOP_DIEM_RCP')
          IF (IER.EQ.0) CALL EZGET('ELEC_MIN_ET',ELETMIN,IER)
          IF (IER.EQ.0) CALL EZGET('PHOT_MIN_ET',PHETMIN,IER)
          IF (IER.EQ.0) CALL
     &      EZGET_l('SELECT_EM_FILTERS',SELECT_EM_FILTERS,IER)
          IF (SELECT_EM_FILTERS) THEN
            CALL EZ_GET_CHARS('EM_FILTNAMES',NFSTRING_REQ,FSTRING_REQ,
     &        IER)
          ENDIF
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_DIEM','TOP_DIEM_RCP',
     &        'ERROR GETTING TOP_DIEM RCP VALUES','W')
          ENDIF
          CALL EZRSET
        ELSE
          CALL ERRMSG('TOP_DIEM','TOP_DIEM_RCP',
     &      'ERROR READING TOP_DIEM RCP FILE','W')
        ENDIF
      ENDIF
C
C ****  intializations
C
      OK        = .FALSE.
      PASS_L2   = .FALSE.
      PASSED    = .FALSE.
C
C ****  Get Zvertex
C
      CALL VERTEX_INFO(1,NV,ZVTX_INFO,OKZ)
C            Only consider the main primary vertex

C
C ****  Check whether the event passes an EM filter
C
      IF (SELECT_EM_FILTERS) THEN
C
C   get names of triggers/filters fired for this event
C
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C   check whether the event passes an EM filter
        DO I = 1,NFSTRING_REQ
          SEARCH_STRING = FSTRING_REQ(I)
          DO J = 1,NFILTON
            OK = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
            PASS_L2 = PASS_L2 .OR. OK
          ENDDO
        ENDDO
C
        IF (.NOT.PASS_L2) GO TO 900
C
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
          IF(ET.GE.ELC_ET) etpass=.true.
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
C          STATUS = GOOD_ELECTRON(LPELC,'LOOSE_NOTRK')
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
C-
      LPPHO=GZPPHO()
      PHOT_ET=PHETMIN
      IF(LPPHO.NE.0) THEN
        DO WHILE (LPPHO.GT.0)
          ET=Q(LPPHO+7)
          ETPASS= .FALSE.
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
C          STATUS = GOOD_PHOTON(LPPHO,'LOOSE')
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
C
C
C ****  Write out event?
C
CCC      NGELPH = NGELEC+NGPHOT
      NGELPH = NELEC+NPHOT
      IF (NGELPH.GE.2) THEN
        PASSED = .TRUE.
        NPASS(1) = NPASS(1) + 1
CCC        IF (NGELEC.GE.2) THEN
        IF (NELEC.GE.2) THEN
          NPASS(2) = NPASS(2)+1
CC        ELSE IF (NGPHOT.GE.2) THEN
        ELSE IF (NPHOT.GE.2) THEN
          NPASS(3) = NPASS(3)+1
        ELSE
          NPASS(4) = NPASS(4)+1
        ENDIF
      ENDIF
C
  900 CONTINUE
C
      TOP_DIEM = PASSED
C
  999 RETURN
C#######################################################################
      ENTRY TOP_DIEM_EOJ

      TOP_DIEM_EOJ = .TRUE.
      IUNIT=SSUNIT()
      WRITE(IUNIT,10)
      WRITE(IUNIT,20) (NPASS(I),I=1,4)
   10 FORMAT(//' Summary of  dielectron streaming '/)
   20 FORMAT(5X,I10,' events pass filter selection ',/
     &       5X,I10,' events pass ee filter ',/
     &       5X,I10,' events pass gg filter ',/
     &       5X,I10,' events pass eg filter ',///)
      RETURN
      END
