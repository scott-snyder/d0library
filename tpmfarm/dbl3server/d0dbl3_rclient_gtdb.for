      SUBROUTINE D0DBL3_RCLIENT_GTDB(PATH,PEDRUN,CRATE,LDAT,LKY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called within dbclb_fetch_offline when reading
C-                         dbl3 server is activated.
C-
C-   Inputs  : PATH     Path name
C-             PEDRUN   Pedestal run number to read
C-             CRATE    Crate number to read
C-   Outputs :
C-              LDAT    Data bank address = 0 if error
C-              LKY     Key bank address = 0 if error
C-
C-   Controls:
C-
C-   Created  26-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBDBL.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INTEGER LKY, LDAT
C
      CHARACTER*(*) PATH
      INTEGER KEY(NKYS), KEYD(NKYS)
      INTEGER LBANK,PEDRUN,CRATE
      EQUIVALENCE (CALIB_LNK(1),LBANK)
      CHARACTER*72 MSGSTR
      INTEGER LENC,TRULEN,LSTPC,DETSEQ
      INTEGER LENTRY,JBIAS,LSUP,LINKC,NSEND,IER
      INTEGER IXDBL2,IDVDBL2,IDVSUMDB2
      CHARACTER*3 DETYP
      CHARACTER*4 CALTYP,HBNK
C-
C----------------------------------------------------------------------
C-
      LBANK = 0
      LDAT = 0
      LKY = 0
      CALL DBCLB_PATH_INFO(PATH,DETYP,DETSEQ,CALTYP,HBNK,LINKC)
      IF(DETYP .EQ. 'DBM') GOTO 999
      IF(IDVDBL .EQ. 0) THEN
        CALL INZDBL
      ELSE
        CALL MZWIPE(IDVDBL)
      ENDIF
      LENC = TRULEN(PATH)
      CALL UCTOH(PATH,IBUF,4,LENC)
      IBUF(11) = LENC
      IBUF(12) = PEDRUN
      IBUF(13) = CRATE
      NSEND = 18
C
      IXDBL2 = IXDBL
      IDVDBL2 = IDVDBL
      IDVSUMDB2 = IDVSUMDB
C
      CALL D0DBL3_RCLIENT_FETCH(NSEND,IBUF,IER)
C
      IXDBL = IXDBL2
      IDVDBL = IDVDBL2
      IDVSUMDB = IDVSUMDB2
C
      IF(IER .EQ. 0) THEN
        LENTRY = IBUF(19)
        LSTPC = LC(LSTPH - IZSTPC)
        LSUP = LSTPC
        JBIAS = - IBUF(21)
        CALL MZCOPY(IDVDBL,LENTRY,IDVSTP,LSUP,JBIAS,' ')
        LKY = LC(LSUP - IBUF(21))
        LDAT = LC(LKY - 1)
        LBANK = LDAT
      ELSE
        CALL ERRDB
     &      ('DBCLB_FETCH_OFFLINE: PROBLEM WITH FETCHING VIA SERVER')
      ENDIF
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
