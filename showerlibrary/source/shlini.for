      LOGICAL FUNCTION SHLINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize showerlibrary using RCP
C-   files. Based on SHLINT
C-
C-   Returned value  : TRUE IF OK.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-DEC-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C
      INCLUDE 'D0$INC:QUEST.INC'
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:SHLCON.INC'
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
        SHLINI=.FALSE.
C
C       read in files
        CALL INRCP('SHOWERLIBRARY_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('SHOWERLIBRARY_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG('SHOWERLIBRARY','SHLINI',
     &  ' Default SHOWERLIBRARY_RCP modified','W')
C
C DECLARE LINK AREA
C
        CALL EZPICK('SHOWERLIBRARY_RCP')              ! select SHOWERLIB bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          SHLINI= .TRUE.
C
          CALL EZGET_i('NUMBER_OF_WARNINGS',NWARN,IER)
          IF ( NWARN .EQ. 0 ) NWARN = 1
          CALL ERRMAX ('SHOWERLIBRARY',-1,NWARN)
C
          CALL EZGETA('MOMENTUM_BINS',0,0,0,NMBIN,IER)
          NMBIN = NMBIN -1
          CALL EZGETA('VERTEX_BINS',0,0,0,NVBIN,IER)
          NVBIN = NVBIN -1                ! BIN BOUNDARIES IN ARRAY
          NEBIN = NETAL
          CALL EZGET('MOMENTUM_BINS',MOMBIN,IER)
          CALL EZGET('VERTEX_BINS',VTXBIN,IER)
          IF(IER.NE.0)CALL ERRMSG('SHOWERLIBRARY','SHLINI',
     &    'ERROR IN GETTING BIN PARAMETERS','W')
        ELSE
          CALL ERRMSG('SHOWERLIBRARY','SHLINI',
     &      'SHOWERLIBRARY_RCP file is not well made.','W')
          SHLINI = .FALSE.
        ENDIF
C
C ****  DO THE FIRST STAGE CREATION OF THE SHOWER LIBRARY IF IT DOESN'T
C       YET EXIST
C       ...OTHERWISE OPEN THE EXISTING FILE
C
        CALL EZGET_l('NEW_LIBRARY',NEW_LIBRARY,IER)
        CALL SHLIB_OPEN('SHOWER_LIBRARY_NAME','SHOWER_LIBRARY_AUX',
     &    'SHOWER_LIBRARY',NEW_LIBRARY,ISUNIT,ISUNIT_AUX,FILNAM,
     &    FILNAM_AUX,CYCLES,NCYCLES,IER)
C
        CALL EZRSET
C
      ELSE
C
C ****  NOT FIRST ENTRY INTO SHLINI - SET SHLINI .TRUE.
C
        SHLINI=.TRUE.
      ENDIF
  999 RETURN
C
C
       END
