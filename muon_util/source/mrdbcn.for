      SUBROUTINE MRDBCN ( OK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read muon electronic constants from DBL3
C-
C-   Outputs : OK     true if no problems
C-   Controls:
C-
C-   Created  19-DEC-1989   J. Green
C-   Modified   June-1991   J. Green  Use new permanant lind for MDTM
C-                                    write record of constants read
C-                                    save validity to reduce fetches
C-                                    error recovery from fetch
C    DH 1/92 change to MUNMOD2
C    DH 3/92 use D0OPEN
C-   Modified  17-APR-1992   SHAHRIAR ABACHI    call to DBCLB_FETCH_OFFLINE
C-                                              used.
C-                May-92     J.Green   add MDFT banks
C    DH 5/92 add MUCALIB switches
C    DH 6/92 turn off read of pedestals
C    Modified   MAR 93       J.Green   make failure to init DB fatal
C    Modified   JAN 94       J.Green   allow forcing database version number
C    Modified   JAN 94       J.Green   add SCINTS caltype
C    Modified   MAR 95   R.Markeloff   Replace MU_SCINT_MOD with MNACTIVE
C    DH 4/95 only call dbclb_init when needed
C    Modified   MAY 95   R.Markeloff   Fixed bug for fetching new constants
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
C      INCLUDE      'D0$LINKS:IZMDTH.LINK'
      INCLUDE      'D0$INC:DBSTP.INC'
      INCLUDE      'D0$INC:MU_ZEBSTP.INC'
      LOGICAL       OK
      INTEGER       LSTPC !, LMDTH        ! specific addresses
      INTEGER       LSUP, LTARG         ! generic addresses
      EQUIVALENCE   (LSUP,LMU(10))      ! make LSUP a declared link
      INTEGER       BSIZE               ! size of bank read in
      INTEGER       MOD,  NMODS, MODULE,INITDB
      CHARACTER*12  FILE                !   DBL3 file for muon consts.
      CHARACTER*64  MSGSTR
      CHARACTER*16  CALTYPE(6)           ! type modified for DBL3
      INTEGER       RUNNO                ! current run number
      INTEGER       RUNSTA, RUNEND       ! start and end validity
      INTEGER       RUNCLB               ! calib run number
      CHARACTER*25  PATH                 ! DBL3 path string
      INTEGER       I                    ! do index
      INTEGER       MUNMOD2              ! function for module numbers
C                                        ! booking functions
      INTEGER       BKSMUO, BKMGNH, BKMTMH, BKMDTH, BKMDFH
      INTEGER       BKMSTH
      INTEGER       GZMGNH,GZMTMH,GZMDTH,GZMDFH,GZMSTH
      INTEGER       DATABASE_VSN        ! which version of CALIB database
      INTEGER       LUNIT               ! unit to write log of fetches
      INTEGER       IERR                ! error return
      INTEGER       NFAIL               ! number of fetch failures
      LOGICAL       MNACTIVE        ! function tells if scint is on mod
      LOGICAL       FIRST, FETCH
      INTEGER       SMUO_LINKS, KSMUO
      INTEGER       MUCALIB,MUCALIB_DB,IER,MUNMOD3,MUCALIB_RUN
      INTEGER       LDAT,LKY
      DATA          FIRST /.TRUE./
      DATA          CALTYPE   / 'PEDESTAL','GAINS','TIMES','DTIMES',
     &                         'DRIFT', 'SCINTS' /
      DATA          FILE/'DBCALIB$MUO'/
C----------------------------------------------------------------------
C
C               insure that links area is initialized
C
      INITDB=0
      IF (FIRST) THEN
        FIRST = .FALSE.
        MUCALIB=-1
        MUCALIB_DB=0
        MUCALIB_RUN=0
        CALL EZGET('MUCALIB',MUCALIB,IER)
        CALL EZGET('MUCALIB_DB',MUCALIB_DB,IER)
        CALL EZGET('MUCALIB_RUN',MUCALIB_RUN,IER)
        CALL EZGET('DATABASE_VSN',DATABASE_VSN,IER)
        IF (IER.LT.0) DATABASE_VSN = -1
        LSTPC = LC(LSTPH-IZSTPC)
        LSMUO = LC(LSTPC-IZSMUO)
        IF ( LSMUO .EQ. 0 )   THEN
          LSMUO = BKSMUO ( 'STPC', MODULE )
        ELSE
          SMUO_LINKS = IC(LSMUO-3)
          IF (SMUO_LINKS.LT.12) THEN
            KSMUO = LSMUO       ! mzpush call can't use a link
            CALL MZPUSH(IXSTP,KSMUO,12 - SMUO_LINKS, 0, ' ')
          ENDIF
        ENDIF
C        LMPDH=GZMPDH(0)
        LMGNH=GZMGNH(0)
        LMTMH=GZMTMH(0)
        LMDTH=GZMDTH(0)
        LMDFH=GZMDFH(0)
        LMSTH=GZMSTH(0)
C        IF ( LMPDH .EQ. 0 )   LMPDH = BKMPDH ( 'STPC', MODULE )
        IF ( LMGNH .EQ. 0 )   LMGNH = BKMGNH ( 'STPC', MODULE )
        IF ( LMTMH .EQ. 0 )   LMTMH = BKMTMH ( 'STPC', MODULE )
        IF ( LMDTH .EQ. 0 )   LMDTH = BKMDTH ( 'STPC', MODULE )
        IF ( LMDFH .EQ. 0 )   LMDFH = BKMDFH ( 'STPC', MODULE )
        IF ( LMSTH .EQ. 0 )   LMSTH = BKMSTH ( 'STPC', MODULE )
C        CALL CLBLNK
        CALL GTUNIT(234,LUNIT,IERR)
        CALL D0OPEN (LUNIT,'CALIB_CONST.OUT','O',OK)
        CALL DBCLB_INITIALIZE_FORCE_VSN (DATABASE_VSN)
        CALL DBCLB_INITIALIZE ( FILE, ' ', OK )
        IF (.NOT. OK ) THEN
          WRITE ( MSGSTR, 1000 ) FILE
 1000     FORMAT (' error initializing ',A)
          CALL ERRMSG ( 'Error in DBCLB_INITIALIZE', 
     A                  'MRDBCN',MSGSTR,'F')
          GO TO 999
        ENDIF
        INITDB=1
      ENDIF
C
      NFAIL = 0                       ! count failures
C
      RUNNO = IQ(LHEAD+6)
      IF (RUNNO.EQ.0) RUNNO = 999999
      IF(MUCALIB.GE.0.AND.MUCALIB_RUN.GT.0) THEN
        RUNNO=MUCALIB_RUN   ! force run number for mucalib
      ENDIF
C
      NMODS = MUNMOD2(0,MOD)
CC   ALLOW PARTIAL READ IN MUCALIB PACKAGE
      IF(MUCALIB.GE.0.AND.MUCALIB_DB.GE.1) NMODS = MUNMOD3(0,MOD)
      WRITE (LUNIT,1004) NMODS, RUNNO
 1004 FORMAT ( I5, ' modules enabled, run = ', I10 )
      DO 533 I=2,6        ! SKIP PEDESTALS   add scints
        IF(MUCALIB.GE.0.AND.MUCALIB_DB.GE.1) THEN ! ALLOW PARTIAL READ OF DB
          IF(MUCALIB.EQ.1.AND.I.NE.3) GO TO 533   ! TIMES
          IF(MUCALIB.EQ.2.AND.I.NE.4) GO TO 533   ! DTIMES
          IF(MUCALIB.EQ.3.AND.I.GE.3) GO TO 533   ! PADS
        ENDIF
        WRITE (LUNIT,1002) CALTYPE(I)
 1002   FORMAT ( ' now getting ',A10,
     &           ' module  calib run  start run  end run')
        CALL DBCLB_PATH ( CALTYPE(I), 'MUO', PATH )
          IF     (I.EQ.1) THEN
C            LSUP = LMPDH
          ELSEIF (I.EQ.2) THEN
            LSUP = LMGNH
          ELSEIF (I.EQ.3) THEN
            LSUP = LMTMH
          ELSEIF (I.EQ.4) THEN
            LSUP = LMDTH
          ELSEIF (I.EQ.5) THEN
            LSUP = LMDFH
          ELSEIF (I.EQ.6) THEN
            LSUP = LMSTH
          ENDIF
C
        DO MOD=1,NMODS
          IF(MUCALIB_DB.GE.1) THEN
            MODULE = MUNMOD3(1,MOD)
          ELSE
            MODULE = MUNMOD2(1,MOD)
          ENDIF
          FETCH = .TRUE.                ! assume we need to fetch
          LTARG = LC(LSUP-MODULE)
          IF (LTARG.NE.0) THEN
            RUNSTA = IC(LTARG+4)
            RUNEND = IC(LTARG+5)
            IF (RUNNO .GE. RUNSTA .AND. RUNNO .LE. RUNEND ) THEN
              FETCH = .FALSE.
            ENDIF
          ENDIF
          IF(MUCALIB_DB.GE.1) FETCH=.TRUE.    ! ALWAYS FETCH
          IF (FETCH .AND. I.EQ.6) THEN ! is there scintillator for this module?
            FETCH = MNACTIVE(MODULE)
          ENDIF
          IF ( FETCH ) THEN
            IF(INITDB.EQ.0) THEN
              CALL DBCLB_INITIALIZE_FORCE_VSN (DATABASE_VSN)
              CALL DBCLB_INITIALIZE ( FILE, ' ', OK )
              IF (.NOT. OK ) THEN
                 WRITE ( MSGSTR, 1000 ) FILE
                 CALL ERRMSG ( 'Error in DBCLB_INITIALIZE',
     A                          'MRDBCN',MSGSTR,'F')
                 GO TO 999
              ENDIF
              INITDB=1
            ENDIF
            CALL DBCLB_FETCH_OFFLINE ( PATH, RUNNO, MODULE, LDAT, LKY )
            IF ( .NOT. CALL_DBEND ) THEN        ! fetch failed
              WRITE (LUNIT,1005) MODULE
 1005         FORMAT(' fetch failed     ', I11)
              NFAIL = NFAIL + 1
              IF ( NFAIL .LT. 2) THEN           ! try recovery
                CALL DBCLB_INITIALIZE_FORCE_VSN (DATABASE_VSN)
                CALL DBCLB_INITIALIZE ( FILE, ' ', OK )
                IF (.NOT. OK ) THEN
                  WRITE ( MSGSTR, 1000 ) FILE
                  CALL ERRMSG ( 'Error in DBCBL_INITIALIZE', 'MRDBCN',
     &                           MSGSTR, 'F')
                  GO TO 999
                ENDIF
                INITDB=1
              ELSE
                OK = .FALSE.
                WRITE (MSGSTR,1006) MODULE, CALTYPE(I)
 1006           FORMAT ( ' 2 failures to read from DB, Mod ',I3,' ',A)
                CALL ERRMSG ( 'Error in DBCLB_FETCH', 'MRDBCN', MSGSTR,
     &                        'F')
                GO TO 999
              ENDIF                     ! NFAIL
            ELSE
              NFAIL = 0                 ! count consecutive failures
C                                       find corrected validity range
              RUNSTA = IC(LKY + 3)
              IC(LDAT + 4) = RUNSTA
              RUNEND = IC(LKY + 4)
              IC(LDAT + 5) = RUNEND
              RUNCLB = IC(LDAT + 6)
              BSIZE  = IC(LDAT - 1)
              IF (BSIZE .GT. 17 ) THEN
                LTARG = LC(LSUP-MODULE)
                IF (LTARG.NE.0) THEN
                  CALL MZDROP(IXSTP,LTARG,' ')
                ENDIF
                CALL ZSHUNT(IXSTP,LDAT,LSUP,-MODULE,1)
                WRITE (LUNIT,1001) MODULE,RUNCLB,RUNSTA,RUNEND
 1001           FORMAT (18X,4I11)
              ELSE
                CALL MZDROP(IXSTP,LDAT,' ')
                WRITE (LUNIT,1003) MODULE
 1003           FORMAT ( ' dummy            ',I11 )
              ENDIF                                   ! BSIZE
            ENDIF                                   ! CALL_DBEND
          ENDIF                                     ! FETCH
        ENDDO                                       ! MOD
  533   CONTINUE                                         ! I=1,4
C
  999 CONTINUE
      IF(INITDB.EQ.1) CALL DBCLB_FINISH
      RETURN
      END
