      FUNCTION HMATRIX_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Hmatrix package
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-   Updated  22-JAN-1991   Harrison B. Prosper
C-      Added call to pick up sub_directory in accumulate mode
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:USE_HMATRIX.INC'
      INCLUDE 'D0$LINKS:IZHRCP.LINK'
      LOGICAL HMATRIX_INI
      LOGICAL FIRST,READONLY
      SAVE FIRST
      DATA FIRST/.TRUE./
      INTEGER IER,NWARN
      LOGICAL OK
      INTEGER SSUNIT,I
      INTEGER NCHARS
      INTEGER STATUS
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        HMATRIX_INI=.FALSE.
C
C       read in files
        CALL INRCP('HMATRIX_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        CALL INRCPE('HMATRIX_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &    CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &    ' Default HMATRIX_RCP modified','W')
C
        CALL EZPICK('HMATRIX_RCP')              ! select HMATRIX bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CALL EZGET('NUMBER_OF_WARNINGS',NWARN,IER)
          CALL ERRMAX ('HMATRIX',-1,NWARN)
          HMATRIX_INI= .TRUE.
        ELSE
          CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &      ' HMATRIX_RCP file does not have a HMATRIX bank.','W')
          HMATRIX_INI = .FALSE.
        ENDIF
C
C ****  MAKE ZHMATRIX into a link area
C
        CALL HMATRIX_MAKE_LINK_AREA
C
        CALL EZGET('ACCUMULATE_HMATRIX',ACCUMULATE,IER)
        IF ( ACCUMULATE ) THEN
C MAKE HEADER BANKS ETC. COPY RCP TO CONTROL RCP.
          CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &      'Program will accumulate H matrix ','W')
          WRITE(SSUNIT(),1)
    1     FORMAT(' ACCUMULATING HMATRIX')
          CALL EZGET('NEW_RZ',NEW_RZ,IER)
          IF ( NEW_RZ ) THEN
            READONLY = .FALSE.
            CALL HMATRIX_RZ_MAKE(.TRUE.,READONLY,RZ_UNIT,OK)
          ELSE
            READONLY = .FALSE.
            CALL HMATRIX_RZ_MAKE(.FALSE.,READONLY,RZ_UNIT,OK)
          ENDIF
          CALL EZCOPY('HMATRIX_RCP','HMATRIX_RZ_RCP',
     &      LCRCP,0,IER)                ! STAND ALONE
          IF ( IER.NE.0 ) THEN
            CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &        'ERROR COPYING RCP BANK TO STRUCTURE','W')
          ENDIF
C
          CALL EZ_GET_CHARS('SUB_DIRECTORY',NCHARS,SUB_DIRECTORY,IER)
          CALL HMATRIX_SET_SIZES        ! SETUP ARRAY DIMENSIONS ETC
          CALL HMATRIX_BOOK_BANKS       ! BOOK ALL STRUCTURES NEEDED
          CALL EZMOVE('HMATRIX_RZ_RCP',LHMTR,IZHRCP) !TAKES CARE OF EXPORT
        ELSE
C READ HMATRIX FROM RZ FILE
          CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &      'Program will read H matrix from file','W')
          WRITE(SSUNIT(),2)
    2     FORMAT(' Reading H matrix from File ')
          READONLY = .TRUE.
          CALL HMATRIX_RZ_MAKE(.FALSE.,READONLY,RZ_UNIT,OK)      ! OPEN EXISTING
                                                                 ! RZ FILE
C
          CALL EZ_GET_CHARS('USE_HMATRIX',NUSE,USE_HMATRIX_LIST,IER)
          DO I = 1 , NUSE
            SUB_DIRECTORY = USE_HMATRIX_LIST(I)
            CALL HMATRIX_RZ_GET_MATRIX(IER)
            IF ( IER.NE.0 ) THEN
              CALL ERRMSG('HMATRIX','HMATRIX_INI',
     &          ' ERROR GETTING HMATRIX FOR USAGE ','W')
            ENDIF
          ENDDO
C
C
          CALL EZPICK('HMATRIX_RCP') !TO OVERRIDE SET TO HMATRIX_RZ_RCP
                                     ! IN HMATRIX_RZ_GET_MATRIX
          CALL EZ_GET_CHARS('SUB_DIRECTORY',NCHARS,SUB_DIRECTORY,IER)
          CALL HMATRIX_SET(SUB_DIRECTORY,IER)   ! SET AS DEFAULT
        ENDIF
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        IF ( STATUS.NE.0 ) THEN
          CALL ERRMSG('HMATRIX_INI','DO_HBOOK_OPEN',
     &      ' ERROR OPENING HBOOK FILE','W')
        ENDIF
        CALL EZRSET
      ELSE
C
C ****  Not first entry into HMATRIX_INI - set HMATRIX_INI .TRUE.
C
        HMATRIX_INI=.TRUE.
      ENDIF
  999 RETURN
      END

