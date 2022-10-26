      LOGICAL FUNCTION MULINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize MUONLIBRARY using RCP
C-   files. Based on SHLINT
C-
C-   Returned value  : TRUE IF OK.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-MAY-1993 Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:MULCON.INC'
      INCLUDE 'D0$INC:MULDAT.INC'
      INTEGER IER
C
      INTEGER ICYCLE
      CHARACTER*80 FILNAM,FILNAM_AUX
      INTEGER L
      LOGICAL NEW_LIBRARY
      INTEGER NCHAR
      INTEGER NWARN
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        MULINI=.FALSE.
C
C       read in files
        CALL INRCP('MUONLIBRARY_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('MUONLIBRARY_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &    CALL ERRMSG('MUONLIBRARY','MULINI',
     &    ' Default MUONLIBRARY_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('MUONLIBRARY_RCP')              ! select MUONLIB bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          MULINI= .TRUE.
C
          CALL EZGET_i('NUMBER_OF_WARNINGS',NWARN,IER)
          IF ( NWARN .EQ. 0 ) NWARN = 1
          CALL ERRMAX ('MUONLIBRARY',-1,NWARN)
C
          CALL EZGETA('MOMENTUM_BINS',0,0,0,NMBIN,IER)
          NMBIN = NMBIN -1
          IF(NMBIN.NE.NMOM) CALL ERRMSG('MUONLIBRARY','MULINI',
     &      'ERROR IN GETTING BIN PARAMETERS','W')
          CALL EZGET('MOMENTUM_BINS',MOMBIN,IER)
          IF(IER.NE.0)CALL ERRMSG('MUONLIBRARY','MULINI',
     &      'ERROR IN GETTING BIN PARAMETERS','W')
        ELSE
          CALL ERRMSG('MUONLIBRARY','MULINI',
     &      'MUONLIBRARY_RCP file is not well made.','W')
          MULINI = .FALSE.
        ENDIF
C
C ****  DO THE FIRST STAGE CREATION OF THE MUON LIBRARY IF IT DOESN'T
C       YET EXIST
C       ...OTHERWISE OPEN THE EXISTING FILE
C
        CALL EZGET_l('NEW_LIBRARY',NEW_LIBRARY,IER)
        CALL MULIB_OPEN('MUON_LIBRARY_NAME','MUON_LIBRARY_AUX',
     &    'MUON_LIBRARY',NEW_LIBRARY,ISUNIT,ISUNIT_AUX,FILNAM,
     &    FILNAM_AUX,CYCLES,NO_CYCLES,NCYCLES,IER)
C
        CALL EZRSET
C
      ELSE
C
C ****  NOT FIRST ENTRY INTO MULINI - SET MULINI .TRUE.
C
        MULINI=.TRUE.
      ENDIF
  999 RETURN
C
      END
