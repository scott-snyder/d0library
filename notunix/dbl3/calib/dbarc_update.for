      SUBROUTINE DBARC_UPDATE(DECT,CALTYPE,CRATE,NRUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used in updating a second database
C-
C-   Inputs  : DECT - detector type
C-             CALTYPE - calibration type
C-             CRATE   crate number (if=0 will ask for one)
C-             NRUN    run number (if=0 will ask for one)
C-   Outputs : none
C-   Controls: none
C-
C-   Created  06-MAR-1991   S. Abachi  made by modifying DBCLB_UPDATE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      CHARACTER*48 DBFILE,DBFIL2
      CHARACTER*3 DECT
      CHARACTER*(*) CALTYPE
      INTEGER NRUN,CRATE
      CHARACTER*25 PATH
      CHARACTER*80 MSG
C
      INTEGER LUN,LUN2
      INTEGER LOGLEV,LBANK,L
      INTEGER ERR,IOS
      LOGICAL IOK
      EQUIVALENCE (CALIB_LNK(1),LBANK)
C----------------------------------------------------------------------
C
      DBFILE = 'DBCALIB$'//DECT//'_S'
      IOK = .TRUE.
      CALL DBCLB_INITIALIZE(DBFILE,' ',IOK)
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error in initialization [when updating]')
        GO TO 498
      ENDIF
C
      IF (NRUN.GT.999999990) NRUN = 999999990
C
      CALL DBCLB_PATH(CALTYPE,DECT,PATH)
      CALL DBCLB_FETCH(PATH,NRUN,CRATE)
C
      IC(LBANK+5) = IC(LKEYS(CRATE) + 4)
C
C  open an output file, and initialize
C
      CALL GTUNIT(12,LUN,ERR)
      OPEN(UNIT=LUN,STATUS='NEW',FILE='DBTEST.PEDS',
     &  FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        IQUEST(1) = IOS
        CALL ERRDB(' ERROR IN OPENING FILE PEDS FILE')
        GO TO 498
      ENDIF
C
      LOGLEV=-2
      CALL FZFILE(LUN,0,'O')
      CALL FZLOGL(LUN,LOGLEV)
      CALL FZOUT(LUN,IDVSTP,LBANK,1,' ',1,0,0)
      CALL FZENDO(LUN,'T')
      CLOSE(LUN)                        ! close the temp file
      CALL RLUNIT(2,LUN,ERR)
  498 CALL DBCLB_FINISH                        ! End DB main
      IF (LBANK.NE.0) CALL MZDROP(IXSTP,LBANK,' ')      ! Drop Bank
      LBANK = 0
C
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error during database Read, Aborting ')
        RETURN
      ENDIF
  998 CONTINUE
C----------------------------------------------------------------------
C
      DBFILE = 'DBCALIB$'//DECT//'_R'
      CALL DBCLB_INITIALIZE(DBFILE,'SU',IOK)
      IF (.NOT.IOK) THEN
        CALL INTMSG(' Error during Local database initialization ')
        RETURN
      ENDIF
C
      CALL GTUNIT(12,LUN2,ERR)
C
      OPEN (UNIT=LUN2,STATUS='OLD',FILE='DBTEST.PEDS',
     &      FORM='UNFORMATTED',READONLY,IOSTAT=IOS)

      CALL FZFILE(LUN2,0,'I')
      CALL FZLOGL(LUN2,LOGLEV)
      CALL FZIN (LUN2,IDVSTP,LBANK,2,' ',0,0)
      CALL FZENDI(LUN2,'T')
      CLOSE(LUN2)
      CALL RLUNIT(12,LUN2,ERR)
C
      CALL DBARC_INSERT(PATH,IOK)
CC      CALL DBCLB_INSERT(PATH,IOK)
      IF (.NOT.IOK) CALL INTMSG(' Error in DB - Insertion ')
C
  997 CALL DBCLB_FINISH
      IF (LBANK.NE.0) CALL MZDROP(IXSTP,LBANK,' ')      ! Drop Bank
      LBANK = 0
      WRITE(MSG,900)NRUN
  900 FORMAT(' Updated Database with Run number = ',I9.9)
      IF (IOK) CALL INTMSG(MSG)
C
      CALL GTUNIT(12,LUN2,ERR)
      OPEN (UNIT=LUN2,STATUS='OLD',FILE='DBTEST.PEDS',
     &      FORM='UNFORMATTED',DISPOSE='DELETE',IOSTAT=IOS)
      CLOSE(LUN2)
      CALL RLUNIT(12,LUN2,ERR)
C
  999 CONTINUE
      NRUN = 0
      CRATE = 0
      RETURN
      END
