      FUNCTION TOP_FIT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize TOP_FIT package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Updated   6-JAN-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:EVENT_QUAN_2C.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      INCLUDE 'D0$INC:LSQ_PARS.INC'
      INCLUDE 'D0$INC:LSQ_MATRIX.INC'
      INCLUDE 'D0$INC:KINE_FIT.INC'
C
      INTEGER I
      LOGICAL TOP_FIT_INI
      LOGICAL FIRST
      INTEGER IER,NWARN
      REAL    WMASS_S,ELMASS_S,MUMASS_S,NUMASS_S,BMASS_S,HADMASS_S
C
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        TOP_FIT_INI=.FALSE.
C       read in files
        CALL INRCP('TOP_FIT_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('TOP_FIT_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &    CALL ERRMSG('TOP_FIT','TOP_FIT_INI',
     &    ' Default TOP_FIT_RCP modified','W')
C
        CALL EZPICK('TOP_FIT_RCP')              ! select TOP_FIT bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CALL EZGET('NUMBER_OF_WARNINGS',NWARN,IER)
          CALL ERRMAX ('TOP_FIT',-1,NWARN)
          TOP_FIT_INI= .TRUE.
        ELSE
          CALL ERRMSG('TOP_FIT','TOP_FIT_INI',
     &      ' TOP_FIT_RCP file does not have a TOP_FIT bank.','W')
          TOP_FIT_INI = .FALSE.
        ENDIF
        TOP_FIT_INI=.TRUE.
      ENDIF
      ITOT_EV = 0  !INITIALIZING
      IACC_EV = 0
      IFIT_EV = 0
C
C
C ****  MAKE ZLSQ into a link area
C
      CALL LSQ_MAKE_LINK_AREA
      NMAT = 0
      DO I = 1 , LSQ_MAX
        M_NAME(I) = ' '
        M_DELETE(I) = 0
      ENDDO
C
      CALL EZPICK('TOP_FIT_RCP')
      CALL EZGET('WMASS',WMASS_S,IER)
      CALL EZGET('ELECTRON_MASS',ELMASS_S,IER)
      CALL EZGET('MUON_MASS',MUMASS_S,IER)
      CALL EZGET('NEUTRINO_MASS',NUMASS_S,IER)
      CALL EZGET('BQUARK_MASS',BMASS_S,IER)
      CALL EZGET('HADRONIC_JET_MASS',HADMASS_S,IER)
      CALL EZGET('LEPTON_TYPE',LEPTON_TYPE,IER)
      CALL EZRSET
C
      WMASS = WMASS_S
      ELMASS = ELMASS_S
      MUMASS = MUMASS_S
      NUMASS = NUMASS_S
      BMASS = BMASS_S
      HADMASS = HADMASS_S
C
  999 RETURN
      END

