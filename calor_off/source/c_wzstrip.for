      SUBROUTINE C_WZSTRIP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STRIP OFF W AND Z EVENTS
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-SEP-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
      INTEGER IER
      REAL    MIN_EL_ET,MIN_PH_ET,MIN_MS_ET
      INTEGER GZPELC,GZPPHO,GZPNUT
      INTEGER IELEC,IPPHO,INEUT
      REAL    ET_ELECTRON,ET_PHOTON,ET_NEUTRINO
      INTEGER NUM_W_EL,NUM_W_PH,NUM_Z_EL_EL,NUM_Z_EL_PH,NUM_Z_PH_PH
      INTEGER SSUNIT
      LOGICAL OK,PBD_SET_FLAG,FLGVAL
      LOGICAL DO_C_RERUN_HMATRIX
      INTEGER IWRITE
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('CDST_RCP')
        CALL EZGET('MINIMUM_ELECTRON_ET',MIN_EL_ET,IER)
        CALL EZGET('MINIMUM_PHOTON_ET',MIN_PH_ET,IER)
        CALL EZGET('MINIMUM_MISSING_ET',MIN_MS_ET,IER)
        CALL EZRSET
        NUM_W_EL = 0
        NUM_W_PH = 0
        NUM_Z_EL_EL = 0
        NUM_Z_EL_PH = 0
        NUM_Z_PH_PH = 0
        IWRITE = 0
        CALL EZPICK('CALFRAME_RCP')
        CALL EZGET_l('DO_C_RERUN_HMATRIX',DO_C_RERUN_HMATRIX,IER)
        CALL EZRSET
      ENDIF
C
      CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
C
      IELEC = 0
      LPELC = GZPELC()
      DO WHILE (LPELC.NE.0)
        ET_ELECTRON = Q(LPELC+7)
        IF ( ET_ELECTRON.GT.MIN_EL_ET ) THEN
          IELEC = IELEC + 1
        ENDIF
        LPELC = LQ(LPELC)
      ENDDO
C
      IPPHO = 0
      LPPHO = GZPPHO()
      DO WHILE (LPPHO.NE.0)
        ET_PHOTON = Q(LPPHO+7)
        IF ( ET_PHOTON.GT.MIN_PH_ET ) THEN
          IPPHO = IPPHO + 1
        ENDIF
        LPPHO = LQ(LPPHO)
      ENDDO
C
      INEUT = 0
      LPNUT = GZPNUT(0)
      IF ( LPNUT.GT.0 ) THEN
        ET_NEUTRINO = Q(LPNUT+7)
        IF(ET_NEUTRINO.GT.MIN_MS_ET )THEN
          INEUT = INEUT + 1
        ENDIF
      ENDIF
C
      IF ( IELEC.GE.2 ) THEN
C
C ****  WRITE EVENT OUT IF ATLEAST EITHER TWO ELECTRONS
C ****  OR TWO PHOTONS OR ONE OF EACH
C
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        NUM_Z_EL_EL = NUM_Z_EL_EL + 1
      ELSEIF ( IELEC.EQ.1.AND.IPPHO.EQ.1 ) THEN
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        NUM_Z_EL_PH = NUM_Z_EL_PH + 1
      ELSEIF ( IPPHO.GE.2 ) THEN
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        NUM_Z_PH_PH = NUM_Z_PH_PH + 1
      ENDIF
C
      IF (IELEC.GE.1.AND.INEUT.EQ.1 ) THEN
C
C ****  WRITE THIS EVENT OUT IF 1 EM AND 1 NEUTRINO
C
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        NUM_W_EL = NUM_W_EL + 1
      ELSEIF ( IPPHO.GE.1.AND.INEUT.EQ.1 ) THEN
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        NUM_W_PH = NUM_W_PH + 1
      ENDIF
C
C
C ****  NOW TO SET IT TO RUN THE H MATRIX PACKAGE IF EVENT HAS BEEN SELECTED.
C
      IF ( FLGVAL('WRITE_THIS_EVENT') ) THEN
        IF ( DO_C_RERUN_HMATRIX ) THEN  !ONLY IF RERUNNING H MATRIX
          OK = PBD_SET_FLAG('C_RERUN_HMATRIX',.TRUE.)
        ENDIF
        IWRITE = IWRITE + 1
      ELSE
        OK = PBD_SET_FLAG('C_RERUN_HMATRIX',.FALSE.)
      ENDIF
C
  999 RETURN
C
      ENTRY C_WZ_SUMMARY
      WRITE(SSUNIT(),1)IWRITE,NUM_W_EL,NUM_W_PH,
     &  NUM_Z_EL_EL,NUM_Z_EL_PH,NUM_Z_PH_PH
    1 FORMAT(' Total number of events written out ',I6,/,
     &       ' Number of events in W electron channel ',I6,/,
     &       ' Number of events in W photon   channel ',I6,/,
     &       ' Number of events in Z electron-electron channel ',I6,/,
     &       ' Number of events in Z electron-photon channel ',I6,/,
     &       ' Number of events in Z photon-photon channel ',I6)

      RETURN
      END
