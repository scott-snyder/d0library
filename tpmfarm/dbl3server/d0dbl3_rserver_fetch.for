      SUBROUTINE D0DBL3_RSERVER_FETCH(RUNIT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills IBUF in ZEBDBL.INC with calibration data
C-
C-   Inputs  : RUNIT        Unit number for printout
C-
C-         It picks up the following from common block in ZEBDBL.INC
C-             IBUF(1-10)    Path name to data
C-             IBUF(11)      Length of character PATH
C-             IBUF(12)      Run nubmer
C-             IBUF(13)      Crate or Module number
C-             IBUF(14-18)   Available
C-
C-   Outputs :
C-         It fills up following in common block in ZEBDBL.INC
C-             IBUF(19)      Address of the key bank
C-             IBUF(20)      Address of the data bank
C-             IBUF(21)      Link offset from STPC or DBLC header bank
C-             IBUF(22-49)   Reserved for other links
C-             IBUF(50)      For problem diagnostics, 0=everything OK
C-             IBUF(51-79)   Available
C-             IBUF(80-100)  Part of the ZEBDBL store. Not to be overwritten.
C-             IBUF(101-*)   Contents of database store
C-             IER           <=0 then OK,
C-                  otherwise trouble, then ntework server
C-                  do not have to send IBUF, and can instead set its
C-                  IERROR flag to some non zero value.
C-
C-   Controls:
C-
C-   Created   7-JAN-1994   SHAHRIAR ABACHI
C-   Modified  7-JAN-1994   SHAHRIAR ABACHI   Added runit to argument
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUNIT,IER
      INCLUDE 'D0$INC:ZEBDBL.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      CHARACTER*(40) PATH
      INTEGER IRUN,ICRT,LENC,TRULEN
C
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LKY, LDAT,I
      INTEGER KEY(NKYS), KEYD(NKYS)
      INTEGER LBANK,UNITC,IC1
      LOGICAL IOK,WIPE(NDET)
      EQUIVALENCE (CALIB_LNK(1),LBANK)
      CHARACTER*200 MSGSTR
      CHARACTER*2 CHOP
      CHARACTER*3 DETYP
      CHARACTER*4 CALTYP,HBNK
      CHARACTER*5 TOPDN
      CHARACTER*80 DBFILE
      INTEGER LINKC,LSUP,LENTRY,LDBLC,JBIAS,LINKD,NC,NCMIN
      INTEGER SVALID(10),EVALID(10),DETSEQ,IDOT,MFREE,IXWIPE
      DATA SVALID,EVALID /10*0.,10*0./
      DATA NC,NCMIN /0,4/
      DATA WIPE /NDET * .FALSE./
C
      CHOP = ' '
      MSGSTR = ' '
      NC = NC + 1
      IF(IDVSTP .GT. 0 .AND. NC . GT. NCMIN) THEN
        NC = 0
        CALL MZNEED(IDVSTP,0,'G')
        MFREE = IQUEST(11)
        IF(MFREE .LT. 3*NNDD) THEN
          DO I=1,NDET
            IF(CDBEND(I)) THEN
              CALL DBCLB_SEQDET(I,DETYP)
              IC1 = 0
              CALL DBCLB_RFINISH(DETYP,IC1)
              WIPE(I) = .TRUE.
            ENDIF
          ENDDO
          CALL CONSTP
        ENDIF
      ENDIF
C
      IER = 0
      PATH = ' '
      LENC = IBUF(11)
      CALL UHTOC(IBUF,4,PATH,LENC)
      CALL UPCASE(PATH, PATH)
      CALL DBCLB_PATH_INFO(PATH(1:LENC),DETYP,DETSEQ,CALTYP,HBNK,LINKC)
      DBFILE = 'DBCALIB$'//DETYP(1:3)
      IRUN = IBUF(12)
      ICRT = IBUF(13)
      PATH(4:4) = PATH(9:9)
      TOPDN = PATH(3:7)
      IC1 = 1
      CHOP(1:1) = 'S'
      CALL DBCLB_INITIALIZE_FORCE_TOPNAME (TOPDN)
      IF(IRUN .LT. SVALID(DETSEQ) .OR. IRUN .GT. EVALID(DETSEQ) .OR.
     &    WIPE(DETSEQ) ) THEN
        IF(WIPE(DETSEQ)) THEN
          WIPE(DETSEQ) = .FALSE.
          IC1 = 0
          CHOP = 'SN'
        ENDIF
        IF(CDBEND(DETSEQ)) THEN
          CALL DBCLB_RFINISH(DETYP,IC1)
          CALL DBCLB_INITIALIZE_FORCE_TOPNAME (TOPDN)
        ENDIF
        CALL DBCLB_INITIALIZE_FORCE_RUN(IRUN)
        CALL DBCLB_INITIALIZE(DBFILE,CHOP,IOK)
        CALL GT_DBFILE_VALIDITY(SVALID(DETSEQ),EVALID(DETSEQ))
      ENDIF
C
      KEY(3) = IRUN
      KEY(4) = IRUN
      KEY(8) = ICRT
C
      CALL RZCDIR(PATH(1:LENC), ' ')
      IF(IQUEST(1) .NE. 0) THEN
        WRITE (MSGSTR,10) IQUEST(1),IRUN,TOPN,PATH
   10   FORMAT(' D0DBL3_RSERVER_FETCH: Error from RZCDIR.
     &      IQUEST(1),IRUN,TOPN,PATH=', I5,I12,A10,5X,A40 )
        WRITE(RUNIT,*) MSGSTR
      ENDIF
C
      CALL DBUSE(PATH(1:LENC),LKEYS(ICRT),LDATA(ICRT),IRUN,KEY,'S348')
      IF(IQUEST(1). NE. 0) THEN
        WRITE (MSGSTR,11) IQUEST(1),IRUN,TOPN,PATH
   11   FORMAT(' D0DBL3_RSERVER_FETCH: Error from DBUSE.
     &      IQUEST(1),IRUN,TOPN,PATH=', I5,I12,A10,5X,A40 )
        WRITE(RUNIT,*) MSGSTR
        LBANK = 0
        IER = 1
        GOTO 999
      ENDIF
      KEYD(3) = ID(LKEYS(ICRT)+3)
      IF ( KEYD(3) .EQ. 1 ) THEN
        KEYD(4) = ID(LKEYS(ICRT)+4)
        IF ( KEYD(4) .EQ. 999999999 ) THEN
          WRITE (MSGSTR,12) ICRT,KEYD(3),KEYD(4),IRUN,PATH
   12     FORMAT(' D0DBL3_RSERVER_FETCH: no valid data for crate,',
     &      ' svalid, evalid, run, path = ', I3.3,3I10,5X,A40 )
          WRITE(RUNIT,*) MSGSTR
          IBUF(50) = 2
          IER = 2
          GOTO 999
        ELSE
          KEY(3) = KEYD(4) + 1           ! get first real data
          KEY(4) = KEYD(4) + 1
          WRITE (MSGSTR,14) IRUN,KEY(3),PATH
   14     FORMAT(' D0DBL3_RSERVER_FETCH:  No valid data for run ',I9,
     &        ' using run ',I9, 'path= ', A40)
          WRITE(RUNIT,*) MSGSTR
          CALL DBUSE
     &          (PATH(1:LENC),LKEYS(ICRT),LDATA(ICRT),IRUN,KEY,'S348')
        ENDIF
      ENDIF
C
      IF (IQUEST(1).NE.0) THEN
        WRITE (MSGSTR,15) IQUEST(1),IRUN,TOPN,PATH
   15   FORMAT(' D0DBL3_RSERVER_FETCH: Error from DBUSE second call.
     &      IQUEST(1),IRUN,TOPN,PATH=', I5,I10,A10,5X,A40 )
        WRITE(RUNIT,*) MSGSTR
        LBANK = 0
        IER = 1
        GOTO 999
      ELSE
        IF(IDVDBL .EQ. 0) THEN
          CALL INZDBL
        ELSE
          CALL CONDBL
        ENDIF
        LBANK = LDATA(ICRT)
        LDAT = LDATA(ICRT)
        LKY = LKEYS(ICRT)
        LENTRY = LKY
        LDBLC = LD(JDBLH - IZSTPC)
        LSUP = LDBLC
        JBIAS = - LINKC
        LINKD = LD(LSUP+JBIAS)
C- copy zebdbl to zebstp
        CALL MZCOPY(IDVSTP,LENTRY,IDVDBL,LSUP,JBIAS,' ')
      ENDIF
C
      IBUF(19) = LD(LSUP - LINKC)
      IBUF(20) = LD(IBUF(19) -1)
      IBUF(21) = LINKC + 10
      IBUF(50) = 0
      IF(LSUP .LE. 0) IBUF(50) = 999
      IF(IQUEST(1) .GT. 0) IBUF(50) = IQUEST(1)
      IF(IBUF(50) .NE. 0) THEN
        IER = 1
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
