      SUBROUTINE MU_L15_TAB_INIT
C-------------------------------------------------------------------------------
C-
C-   Purpose and Methods : Open and allocate logical unit numbers for 
C-				the level 1.5 muon trigger tables.
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified  1-12-1994   L. Markosky.  Set up for run 1B L1.5 tables. 
C-------------------------------------------------------------------------------
	IMPLICIT NONE
	CHARACTER*72 CR21TAB(5), CR31TAB(6), CR41TAB(6)
	CHARACTER*72 CR51TAB(8), CR61TAB(8)
	CHARACTER*72 SAMCEN_NTAB, SAMCEN_STAB
	CHARACTER*72 CHSEL_NTAB, CHSEL_STAB	
	CHARACTER*72 CONSEC_TAB1, CONSEC_TAB2
	INTEGER CR21_LENGTH(5), CR31_LENGTH(6), CR41_LENGTH(6)
	INTEGER CR51_LENGTH(8), CR61_LENGTH(8)
	INTEGER CNSC1_LENGTH, CNSC2_LENGTH
	INTEGER CHN_LENGTH, CHS_LENGTH
	INTEGER LUN_CR21(5), LUN_CR31(6), LUN_CR41(6)
	INTEGER LUN_CR51(8), LUN_CR61(8), LUN_CCTMAT(5)
	INTEGER LUN_SCN, LUN_SCS, LUN_CONSEC1, LUN_CONSEC2

	INTEGER IER, I
	INTEGER OTC_FILE_LENGTH

	CHARACTER*72 OTC_FILE
	CHARACTER*72 STRING
	INTEGER GT_USER
	PARAMETER (GT_USER = 1) ! Default User Unit Number

C------------------------------------------------------------------------------
C  Open MUSIM.RCP, which contains the name of the OTC table .RCP.

      CALL EZPICK('MUSIM_RCP')
      CALL EZERR(IER)     ! Check if error
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' EZPICK ERR','MU_L15_TAB_INIT',STRING,'F')
        GOTO 999
      ENDIF

C  Get the name of the OTC tables' .RCP file.

      CALL EZGETS('OTC_TAB_FILE',1,OTC_FILE,OTC_FILE_LENGTH,IER)
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' OTC_TAB_FILE','MU_L15_TAB_INIT',STRING,'F')
        GOTO 999
      ENDIF

C-- Read in the OTC tables' RCP file.

      CALL INRCP(OTC_FILE,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG(' INRCP','MU_L15_TAB_INIT','Could not read: '
     &  // 'OTC_FILE_RCP','F')
        GOTO 999
      ENDIF

      CALL EZRSET()

C  Open the OTC .RCP.

      CALL EZPICK('MU_L15_TABLES_1B_RCP')
      CALL EZERR(IER)     ! Check if error

      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' EZPICK ERR','MU_L15_TAB_INIT',STRING,'F')
        GOTO 999
      ENDIF

C Start getting the table names, crate by crate.

      DO I = 1, 8

C  Crate 21 - CF Kinematic tables.

        IF(I.LT.6)THEN
          CALL EZGETS('OTC_CRATE21_TABLES',I,CR21TAB(I),
     &				CR21_LENGTH(I),IER)
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG(' OTC_CRATE21_TABLES',
     &			'MU_L15_TAB_INIT',STRING,'F')
            GOTO 999
          ENDIf
        ENDIF

C  Crate 31 - North EF and Overlap Kinematic tables.

	IF(I.LT.7)THEN
           CALL EZGETS('OTC_CRATE31_TABLES',I,CR31TAB(I),
     &			CR31_LENGTH(I),IER)
           IF(IER.NE.0) THEN
             CALL EZGET_ERROR_TEXT(IER,STRING)
             CALL ERRMSG(' OTC_CRATE31_TABLES','MU_L15_TAB_INIT',
     &			STRING,'F')
             GOTO 999
           ENDIF

C  Crate 41 - South EF and Overlap Kinematic tables.

           CALL EZGETS('OTC_CRATE41_TABLES',I,CR41TAB(I),
     &			CR41_LENGTH(I),IER)
      	   IF(IER.NE.0) THEN
              CALL EZGET_ERROR_TEXT(IER,STRING)
              CALL ERRMSG(' OTC_CRATE41_TABLES','MU_L15_TAB_INIT',
     &			STRING,'F')
              GOTO 999
           ENDIF
	ENDIF

C  Crate 51 - North SAMUS Kin, SAMUS triplet and road find, Overlap triplet
C    find tables.

        CALL EZGETS('OTC_CRATE51_TABLES',I,CR51TAB(I),CR51_LENGTH(I),
     &			IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' OTC_CRATE51_TABLES','MU_L15_TAB_INIT',
     &			STRING,'F')
          GOTO 999
        ENDIF

C  Crate 61 - South SAMUS Kin, SAMUS triplet and road find, Overlap triplet
C    find tables.

        CALL EZGETS('OTC_CRATE61_TABLES',I,CR61TAB(I),CR61_LENGTH(I),
     &			IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' OTC_CRATE61_TABLES','MU_L15_TAB_INIT',
     &			STRING,'F')
          GOTO 999
        ENDIF

      ENDDO

C North and South SAMCEN tables (for samus centroid clustering).

      CALL EZGETS('SAMCEN_CONSEC_1',1,CONSEC_TAB1,CNSC1_LENGTH,IER)
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' SAMCEN_CONSEC_1','MU_L15_TAB_INIT',STRING,'F')
        GOTO 999
      ENDIF

      CALL EZGETS('SAMCEN_CONSEC_2',1,CONSEC_TAB2,CNSC2_LENGTH,IER)
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' SAMCEN_CONSEC_2','MU_L15_TAB_INIT',STRING,'F')
        GOTO 999
      ENDIF
	
      CALL EZGETS('SAMCEN_CHSEL_NORTH',1,CHSEL_NTAB,CHN_LENGTH,IER)
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' SAMCEN_CHSEL_NORTH','MU_L15_TAB_INIT',
     &		STRING,'F')
        GOTO 999
      ENDIF

      CALL EZGETS('SAMCEN_CHSEL_SOUTH',1,CHSEL_STAB,CHS_LENGTH,IER)
      IF(IER.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' SAMCEN_CHSEL_SOUTH','MU_L15_TAB_INIT',
     &		STRING,'F')
        GOTO 999
      ENDIF

      CALL EZRSET()

C  Allocate logical units for all tables.

        DO I = 1, 8
	  IF(I.LT.6)THEN
             CALL GTUNIT(GT_USER,LUN_CR21(I),IER)
             IF(IER.NE.0) THEN
                CALL ERRMSG('LUN_CR21(I)','GTUNIT',
     &     		' Could not allocate logical unit','F')
             ENDIF
	  ENDIF
	  IF(I.LT.7)THEN
             CALL GTUNIT(GT_USER,LUN_CR31(I),IER)
             IF(IER.NE.0) THEN
               CALL ERRMSG('LUN_CR31(I)','GTUNIT',
     &         		' Could not allocate logical unit','F')
               ENDIF
             CALL GTUNIT(GT_USER,LUN_CR41(I),IER)
             IF(IER.NE.0) THEN
               CALL ERRMSG('LUN_CR41(I)','GTUNIT',
     &         		' Could not allocate logical unit','F')
             ENDIF
	  ENDIF
          CALL GTUNIT(GT_USER,LUN_CR51(I),IER)
          IF(IER.NE.0) THEN
             CALL ERRMSG('LUN_CR51(I)','GTUNIT',
     &         		' Could not allocate logical unit','F')
          ENDIF
          CALL GTUNIT(GT_USER,LUN_CR61(I),IER)
          IF(IER.NE.0) THEN
             CALL ERRMSG('LUN_CR61(I)','GTUNIT',
     &         		' Could not allocate logical unit','F')
          ENDIF
        ENDDO

        CALL GTUNIT(GT_USER,LUN_SCN,IER)
        IF(IER.NE.0) THEN
           CALL ERRMSG('LUN_SCN','GTUNIT',
     &     		' Could not allocate logical unit','F')
        ENDIF

        CALL GTUNIT(GT_USER,LUN_SCS,IER)
        IF(IER.NE.0) THEN
           CALL ERRMSG('LUN_SCS','GTUNIT',
     &     		' Could not allocate logical unit','F')
        ENDIF

	CALL GTUNIT(GT_USER,LUN_CONSEC1,IER)
        IF(IER.NE.0) THEN
           CALL ERRMSG('LUN_CONSEC1','GTUNIT',
     &     		' Could not allocate logical unit','F')
        ENDIF

	CALL GTUNIT(GT_USER,LUN_CONSEC2,IER)
        IF(IER.NE.0) THEN
           CALL ERRMSG('LUN_CONSEC2','GTUNIT',
     &     		' Could not allocate logical unit','F')
        ENDIF

C  Set the unit numbers in MU_TRIG_CRATE for the level 1.5 trigger calculation.

        CALL MU_OTC_LUN_SET(LUN_CR21,LUN_CR31,LUN_CR41,LUN_CR51,
     &				LUN_CR61)
        CALL SAMCEN_CHSEL_LUN_SET(LUN_SCN,LUN_SCS)
        CALL SAMCEN_CONSEC_LUN_SET(LUN_CONSEC1,	LUN_CONSEC2)

	LUN_CCTMAT(1) = LUN_CR21(5)
	LUN_CCTMAT(2) = LUN_CR31(6)
	LUN_CCTMAT(3) = LUN_CR41(6)
	LUN_CCTMAT(4) = LUN_CR51(8)
	LUN_CCTMAT(5) = LUN_CR61(8)

	CALL MU_OTC_CCTMAT_SET(LUN_CCTMAT)

C  Open OTC tables.

	DO I = 1, 8
	   IF(I.LT.6)THEN
             OPEN(LUN_CR21(I),FILE=CR21TAB(I),READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
	   ENDIF
	   IF(I.LT.7)THEN
             OPEN(LUN_CR31(I),FILE=CR31TAB(I),READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
             OPEN(LUN_CR41(I),FILE=CR41TAB(I),READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
	   ENDIF
           OPEN(LUN_CR51(I),FILE=CR51TAB(I),READONLY,
     &       FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
           OPEN(LUN_CR61(I),FILE=CR61TAB(I),READONLY,
     &       FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
	ENDDO

	OPEN(LUN_SCN,FILE=CHSEL_NTAB,READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
	OPEN(LUN_SCS,FILE=CHSEL_STAB,READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)

	OPEN(LUN_CONSEC1,FILE=CONSEC_TAB1,READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)
	OPEN(LUN_CONSEC2,FILE=CONSEC_TAB2,READONLY,
     &         FORM='UNFORMATTED',ACCESS='DIRECT',STATUS='OLD',RECL=1)

C-------------------------------------------------------------------------------
  999 RETURN
      END
