      SUBROUTINE DB_MUGEOM( GEO, TY, I, VALID1,VALID2, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get MUON geometory STP from database
C-                         Before call this rotuine, SMUO bank
C_                         has to be constructed.
C-                         Currently MSGH part is off.
C-
C-   Inputs  : GEO  : Type of bank 'MGEO', 'MSGH', 'MMAH'
C-             TY   : Type of geometory 0:MC, 1:Surevy, -1:Survey
C-             I    : if TY= 0  version number for MC
C-                    if TY= 1  run number     for Survey
C-                    if TY=-1  version number for Survey
C-   Outputs : VALID1 : Lowest run number for real data
C-                         Header bank version number for MC
C-             VALID2 : highest run number
C-             IER    error code: 0 no error
C-                              1 specified STP not found
C-                              2 DBL3 initialize error
C-                              3 INPUT 'TY' not avairable
C-                              4 INPUT 'GEO' not avairable
C-                              5 No STP on 'GEO'
C-   Controls: Database logical = 'DBSRVY_MUO'
C-
C-   Created   3-DEC-1991   Atsushi Taketani
C-   Updated   07-Apr-1992  Atsushi Taketani compatiblity to UNIX
C-   Updated   12-MAR-1993  Atsushi Taketani open and close on each time
C-                                           modify TOP dir name
C-                                           if fail, program crash
C_   Updated   29-MAR-1993  AT Use new database logical
C-   Updated   24-MAY-1993  AT bug fix
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) GEO
      INTEGER  TY, I, IER, VALID1, VALID2, IGEO
      LOGICAL  FIRST
      LOGICAL  INIT
      INTEGER  DUNIT, LTDIR
      CHARACTER*40   PATH(3)
      INTEGER  LBK, LBD, ITIME
      INTEGER  KEY(9)
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      CHARACTER*132 MSG
      INTEGER      GZMGEH, LDN, GZMMAH, GZMSGH
C
      INTEGER      K1,K2,K3,NKEY,IKEY,IKEYL,INSTL
C
      INTEGER      KEYS(999), NKEYS, NWKEYS
      INTEGER      KEYV(8,111), KEYVA(3,8,111), NKEYSA(3)
      EQUIVALENCE  (KEYS(1), KEYV(1,1))
C
      INTEGER GZSMUO, MODULE
      INCLUDE 'D0$LINKS:IZMGEH.LINK'
      INCLUDE 'D0$LINKS:IZMMAH.LINK'
C      INCLUDE '[TAKETANI.SCINT]IZMSGH.LINK'
      INTEGER NWRITE, RUNNO
C
      CHARACTER*80 ERRMG
      INTEGER      ELEN
      LOGICAL OK
C
C data statment
      DATA           PATH / '//WMUGEO/SMUO/MGEH','//WMUGEO/SMUO/MMAH',
     &                      '//WMUGEO/SMUO/MSGH' /
      DATA     FIRST/.TRUE./
      DATA     INIT/.FALSE./
      DATA    NWRITE/0/
CC----------------------------------------------------------------------
C  INIT DATABASE
C
      IF ( FIRST ) THEN
C        FIRST = .FALSE.
        CALL GTUNIT (121,DUNIT,IER)         ! get logical unit
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('Error in GTUNIT', 'DB_MUGEOM',
     &                'can not get logical unit number for DBL3', 'W' )
          CALL D0_ABORT( 'ABORT in DB_MUGEOM' )
          IER = 2
          GOTO 999
        END IF                              ! open data base file
        CALL D0RZOPEN( DUNIT, 'DBL3$WMG:DBTEST_MAR12.DAT', 
     1                 'ISU', 4096, OK )
        IF ( .NOT.OK ) THEN
          CALL ERRMSG( 'Error on OPEN', 'DB_MUGEOM',
     &                 'can not open DBL3 file', 'W' )
          CALL D0_ABORT( 'ABORT in DB_MUGEOM' )
          IER = 2
          GOTO 999
        ELSE 
          IER = 0
        END IF     
C DBL3 init
        CALL DBINIT(IDVSTP,DUNIT,'WMUGEO',LTDIR,     0,'S')
        IF(IQUEST(1) .NE. 0) THEN
          CALL ERRMSG( 'Error in DBINIT', 'DB_MUGEOM',
     &                 'DBL3 initialize fail', 'W' )
          CALL D0_ABORT( 'ABORT in DB_MUGEOM' )
          IER = 2
          GOTO 999
        ENDIF
        DO 200 K1=1,3                       ! get KEY information
          CALL DUKEYD( PATH(K1), KEYS, NKEYS, NWKEYS )
          NKEYSA(K1) = NKEYS
          DO 200 K2=1,NKEYS
            DO 200 K3=1,NWKEYS
              KEYVA(K1,K3,k2) = KEYV(K3,K2)
  200   CONTINUE
        INIT = .TRUE.
      END IF
C
      IF ( .NOT.INIT ) THEN                 ! INIT ok?
        CALL ERRMSG( 'Error in MUGEO DBL3 INIT', 'DB_MUGEOM',
     &               'DBL3 initialize fail', 'W' )
        CALL D0_ABORT( 'ABORT in DB_MUGEOM' )
        IER = 2
        GOTO 999
      END IF
C
      IF ( GEO.EQ.'MGEH' ) THEN             ! Is 'GEO' avairable?
        IGEO = 1
      ELSE IF ( GEO.EQ.'MMAH' ) THEN
        IGEO = 2
      ELSE IF ( GEO.EQ.'MSGH' ) THEN
        IGEO = 3
      ELSE
        IER = 4
        GOTO 999
      END IF
C
      NKEYS = NKEYSA( IGEO )
      IF ( NKEYS.LE.0 ) THEN         ! Is STP on 'GEO'?
        IER = 5
        GOTO 999
      END IF
C
      IF ( TY.EQ.0 ) THEN         ! MC type geometory
        KEY(8) = -ABS(I)
        DO IKEY=1,NKEYS
          IF ( KEYVA(IGEO,8,IKEY).EQ.KEY(8) ) THEN
            ITIME = KEYVA(IGEO,3,IKEY)
            VALID1 = KEYVA(IGEO,8,IKEY)
            VALID2 = KEYVA(IGEO,4,IKEY)
            GOTO 100
          END IF
        END DO
        IER = 3
        GOTO 990
  100   CALL DBUSE( PATH(IGEO), LBK, LBD, ITIME, KEY, '8' )
      ELSE IF ( TY.EQ.1 ) THEN    ! survey type geometory
        INSTL = 0
        DO IKEY=1,NKEYSA(IGEO)
          IF ((I.GE.KEYVA(IGEO,3,IKEY)).AND.(I.LE.KEYVA(IGEO,4,IKEY))
     &                           ) THEN
            IF ( KEYVA(IGEO,7,IKEY).GE.INSTL ) THEN
              INSTL = KEYVA(IGEO,7,IKEY)
              IKEYL = IKEY
            END IF
          END IF
        END DO
        ITIME = I
        KEY(8) = KEYVA(IGEO,8,IKEYL)
        CALL DBUSE( PATH(IGEO), LBK, LBD, ITIME, KEY, '8' )
        VALID1 = KEYVA(IGEO,3,IKEYL)
        VALID2 = KEYVA(IGEO,4,IKEYL)
      ELSE IF ( TY.EQ.-1 ) THEN
        KEY(8) = I
        DO IKEY=1,NKEYSA(IGEO)
          IF ( KEYVA(IGEO,8,IKEY).EQ.KEY(8) ) THEN
            ITIME = KEYVA(IGEO,3,IKEY)
            VALID1 = KEYVA(IGEO,3,IKEY)
            VALID2 = KEYVA(IGEO,4,IKEY)
            GOTO 110
          END IF
        END DO
        IER = 3
        GOTO 990
  110   CALL DBUSE( PATH(IGEO), LBK, LBD, ITIME, KEY, '8' )
      ELSE
        IER = 3
        GOTO 990
      END IF
C
      IF ( IQUEST(1).NE.0 ) THEN
        CALL ADDSTR( 'specified STP not exist for ', GEO, ERRMG, ELEN )
        CALL ERRMSG( 'Error on DBUSE', 'DB_MUGEOM', ERRMG(1:ELEN )
     &               , 'W' )
        CALL D0_ABORT( 'ABORT in DB_MUGEOM' )
        IER = 1
        GOTO 999
      ELSE
        IER = 0
      END IF
C
      IF ( IGEO.EQ.1 ) THEN
        LDN = GZMGEH()
        CALL MZDROP( IXSTP, LDN, ' ' )
        LSMUO = GZSMUO( 'STPC', MODULE )
        CALL ZSHUNT( IXSTP, LBD, LSMUO, -IZMGEH, 1 )
        LDN = GZMGEH()
        IF ( TY.EQ.1 ) THEN
          IC(LDN+4) =  VALID1
          IC(LDN+5) =  VALID2
        END IF
      ELSE IF ( IGEO.EQ.2 ) THEN
        LDN = GZMMAH()
        CALL MZDROP( IXSTP, LDN, ' ' )
        LSMUO = GZSMUO( 'STPC', MODULE )
        CALL ZSHUNT( IXSTP, LBD, LSMUO, -IZMMAH, 1 )
        LDN = GZMMAH()
        IF ( TY.EQ.1 ) THEN
          IC(LDN+4) =  VALID1
          IC(LDN+5) =  VALID2
        END IF
C      ELSE IF ( IGEO.EQ.3 ) THEN
C        LDN = GZMSGH()
C        CALL MZDROP( IXSTP, LDN, ' ' )
C        LSMUO = GZSMUO( 'STPC', MODULE )
C        CALL ZSHUNT( IXSTP, LBD, LSMUO, -IZMSGH, 1 )
C        LDN = GZMSGH()
C        IF ( TY.EQ.1 ) THEN
C          IC(LDN+4) =  VALID1
C          IC(LDN+5) =  VALID2
C        END IF
      END IF
C
      IF ( NWRITE.LE.10 ) THEN
        WRITE( MSG,800) GEO, KEY(8), RUNNO()
  800   FORMAT( ' Load new ',A4, ' from DB, VSN=', I4, ' RUN = ', I8 )
        CALL INTMSG( MSG )
        NWRITE = NWRITE + 1
      END IF

  990 IF ( (IER.EQ.3).AND.(NWRITE.LE.10) ) THEN
        WRITE( MSG,810) GEO
  810   FORMAT( ' Fail to load new ',A4, ' from DB' )
        CALL INTMSG( MSG )
        NWRITE = NWRITE + 1
      END IF
C
  999 CLOSE(DUNIT)
      CALL DBENDF('WMUGEO')
C
      RETURN
      END
