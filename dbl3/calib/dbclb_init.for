      SUBROUTINE DBCLB_INIT(DBFILE1,CHOPT,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 initialization
C-
C-   Inputs  :
C-  DBFILE1 - DBL3 data base name.
C-  CHOPT = ' ' Read only. Changes by other processes not visible
C-        = 'S' Read only. Changes by other processes visible to the user
C-        = 'U' for Exclusive Update privileges
C-        = 'SU' for Shared Update pivileges
C-        = 'O' For Owner pivileges (Used by server only)
C-        = 'P' For Public privileges (Used by clients to update via server)
C-        = 'J' Journaling activated
C-        = 'X' or 'A' used with Journaling means exchange or alpha mode.
C-            default is local mode.
C-        = 'D' Create D0 FZ journal files to be used for database update.
C- *NOTE:: Update Privileges must be used only by CALIB/ENDTSK.
C-
C-   Outputs :  IOK    - .TRUE. if no error
C-   Controls:
C-
C-   Created  17-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Updated  01-FEB-1991   S. Abachi  backup and journaling options added
C-   Updated  12-AUG-1991   S. Abachi  Client-Server features implemented
C-   updated     Mar-91     J.Green    added SAMUS
C-   Updated  04-MAR-1992   S. Abachi  Use GTUNIT for units
C-   Updated  05-MAR-1992   S. Abachi  Non-logical filename protction added
C-   Updated  06-MAY-1992   S. Abachi  Modified to conform to new dbstp.inc
C-   Updated  07-MAY-1992   S. Abachi  Machine blocks added
C-   Updated  01-JUN-1992   S. Abachi  Journaling can be directly activated.
C-                                     logical DBL3_JFILE can be defined to be
C-                                     the file name.
C-   Updated  03-JUN-1992   H. Greenlee Added machine block for opening
C-                                     databases on Silicon Graphics.
C-   Updated  12-JUN-1992   S. Abachi  Option D added.
C-   Updated  16-DEC-1992   H.XU       Added Level0
C-   Updated  16-JAN-1993   S. Abachi  DBLUN treatment modified
C-   Updated  24-JAN-1994   S. Abachi  Reading server feature added
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DBFILE1
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:DB_SRVR_UNITS.INC'
      CHARACTER*80 MSG_STRING
      CHARACTER*80 DBFILE
      CHARACTER*80 DBFILE2, LOCAL_DBFILE
      CHARACTER*(*) CHOPT
      CHARACTER*3 CHOP
      CHARACTER*4 CPT
      CHARACTER*1 COPT
      CHARACTER*10 JFILE
      INTEGER L,IOS,I,DOLL,IENT,J,ERR
      LOGICAL OPTZ,OPTS,OPTU,OPTO,OPTP,OPTA,OPTX,OPTD,OPTN
      LOGICAL IOK,FIRST,OK,FTMN
      INTEGER NIOS,CLEN,IRECL,IDB
      DATA IENT,FIRST /0,.TRUE./
C----------------------------------------------------------------------
C
      DBFILE = DBFILE1
      CALL UPCASE(DBFILE, DBFILE)
      DOPT = 0
      CPT = ' '
      FTMN = .FALSE.
      IOS = 0
C
      IDB = 0
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL_DBEND = .FALSE.
        OPTJ = .FALSE.
        OPTB = .FALSE.
        BUNIT = -1
        JUNIT = -1
        DO I=1,NDET
          CDBEND(I) = .FALSE.
          LUNDB(I) = 0
        ENDDO
      ENDIF
      SERVER = .FALSE.
      NIOS = 0
      OPTZ = .FALSE.
      OPTS = .FALSE.
      OPTU = .FALSE.
      OPTO = .FALSE.
      OPTP = .FALSE.
      OPTD = .FALSE.
      OPTN = .FALSE.
      CHOP = ' '
      J = 0
      IF(OPTJ) THEN
        WRITE(MSG_STRING,80)
   80   FORMAT(' Journal file open. Will close.')
        CALL INTMSG(MSG_STRING)
        CALL DBCLB_END_JOURNAL
        OPTJ = .FALSE.
      ENDIF
C
      L = LEN(CHOPT)
      DO I=1,MIN(5,L)
        IF(CHOPT(I:I) .EQ. 'Z') OPTZ = .TRUE.
        IF(CHOPT(I:I) .EQ. 'S') OPTS = .TRUE.
        IF(CHOPT(I:I) .EQ. 'U') OPTU = .TRUE.
        IF(CHOPT(I:I) .EQ. 'O') OPTO = .TRUE.
        IF(CHOPT(I:I) .EQ. 'P') OPTP = .TRUE.
        IF(CHOPT(I:I) .EQ. 'J') OPTJ = .TRUE.
        IF(CHOPT(I:I) .EQ. 'X') OPTX = .TRUE.
        IF(CHOPT(I:I) .EQ. 'A') OPTA = .TRUE.
        IF(CHOPT(I:I) .EQ. 'D') OPTD = .TRUE.
        IF(CHOPT(I:I) .EQ. 'N') OPTN = .TRUE.
        IF(CHOPT(I:I) .EQ. 'Z' .OR. CHOPT(I:I) .EQ. 'S' .OR.
     &     CHOPT(I:I) .EQ. 'U' .OR. CHOPT(I:I) .EQ. 'O' .OR.
     &     CHOPT(I:I) .EQ. 'P') THEN
          J = J + 1
          CHOP(J:J) = CHOPT(I:I)
        ENDIF
      ENDDO
C
      DOPT = 0
      IF(OPTD) DOPT = 1
      IF(OPTD .AND. (OPTZ .OR. OPTS .OR. OPTU)) DOPT = 2
C
      IF(DOPT .EQ. 1) THEN
        MSG_STRING =
     &'DOPT=1 : Only FZ files will be produced. No effect on database.'
        CALL INTMSG(MSG_STRING)
      ELSEIF(DOPT .EQ. 2) THEN
        MSG_STRING =
     &'DOPT=2 : FZ files will be produced & databse will be modified.'
        CALL INTMSG(MSG_STRING)
      ENDIF
C
      DBFILE2 = DBFILE
      DOLL = INDEX(DBFILE2,'DBCALIB$')
      IF(DOLL .EQ. 0)  THEN
        GOTO 5
      ELSE
        DBFILE2(1:) = DBFILE2(DOLL+8:)
        GOTO 6
      ENDIF
C
    5 CONTINUE
C
      DOLL = INDEX(DBFILE2,'//FNAL/D0/DBL3/CALIB/')
      IF(DOLL .EQ. 0)  THEN
        WRITE(MSG_STRING,90)
   90   FORMAT(' DBCLB_INIT: Improper database file name ')
        CALL INTMSG(MSG_STRING)
        IOK = .FALSE.
        GOTO 999
      ELSE
        FTMN = .TRUE.
        DBFILE2(1:) = DBFILE2(DOLL+21:)
      ENDIF
C
    6 CONTINUE
C
      CALL DBCLB_DETSEQ(DBFILE2(1:3), IDB)
      IF(IDB .GT. 0) THEN
        CALL DBL3INIT_RSRVR(DBFILE2(1:3), ' ')
        IF(LUNDB(IDB) .EQ. 0) THEN
          CALL GTUNIT(171,DBLUN,ERR)
          LUNDB(IDB) = DBLUN
        ENDIF
        DBLUN = LUNDB(IDB)
        LUNRZ = DBLUN
      ELSE
        WRITE(MSG_STRING,90)
        CALL INTMSG(MSG_STRING)
        GOTO 999
      ENDIF
C
   50 CONTINUE
      IF(RSERVER) THEN
        TOPN(2:2) = DBFILE2(1:3)
        IF(DBFILE2(1:3) .EQ. 'CDC') TOPN(2:2) = 'D'
      ENDIF
      IF(IDVSTP.EQ.0)THEN
        CALL INZBRA
        CALL INZSTP
      ENDIF
C
      CALL MZBOOK(IDVSTP,L,L,2,'$RZ$',0,0,25000,2,-1)
      CALL MZDROP(IDVSTP,L,' ')
C
      IF (IENT.EQ.0) THEN
        CALL MZLINK(IDVSTP,'/DBSTP/',LTDIR,LKEYS(0),LDATA(255))
        CALL CLBLNK
        IENT = 1
      ENDIF
C
   20 CONTINUE
      LOCAL_DBFILE = DBFILE
      IOS = 0
      IF(.NOT. OPTN) THEN
        IF (OPTO) THEN
          CPT = 'IOSU'
          IRECL = 4096
          CALL D0RZOPEN(DBLUN,LOCAL_DBFILE,CPT,IRECL,OK)
          IF(.NOT. OK) IOS = -30
          GOTO 900
        ELSEIF (OPTP) THEN
          SERVER = .TRUE.
          CPT = 'ISU'
          IRECL = 4096
          CALL D0RZOPEN(DBLUN,LOCAL_DBFILE,CPT,IRECL,OK)
          IF(.NOT. OK) IOS = -30
          GOTO 900
        ELSEIF (OPTZ) THEN
          CPT = 'IOSU'
          IRECL = 4096
          CALL D0RZOPEN(DBLUN,LOCAL_DBFILE,CPT,IRECL,OK)
          IF(.NOT. OK) IOS = -30
          GOTO 900
        ELSEIF (OPTU) THEN
          CPT = 'IOSU'
          IRECL = 4096
          CALL D0RZOPEN(DBLUN,LOCAL_DBFILE,CPT,IRECL,OK)
          IF(.NOT. OK) IOS = -30
          GOTO 900
        ELSE
          CPT = 'ISU'
          IRECL = 4096
          CALL D0RZOPEN(DBLUN,LOCAL_DBFILE,CPT,IRECL,OK)
          IF(.NOT. OK) IOS = -30
        ENDIF
      ENDIF
C
  900 CONTINUE
C
      IF(IOS.NE.0)THEN
        IF(IOS .EQ. 30) THEN
          NIOS = NIOS + 1
          IF(NIOS .LT. 2) THEN
            WRITE(MSG_STRING,94) DBFILE,IOS
   94       FORMAT(' File ',A20, ' could not be accessed.',
     &                        ' Will try again. IOSTAT = ',I3)
            CALL INTMSG(MSG_STRING)
            GOTO 20
          ENDIF
        ENDIF
        WRITE(MSG_STRING,95)DBFILE,IOS
   95   FORMAT(' Error opening file ',A20,'  IOSTAT = ',I3)
        CALL ERRDB(MSG_STRING)
        CALL INTMSG(MSG_STRING)
        IOK = .FALSE.
        GOTO 999
      ENDIF
      IF (OPTZ) THEN
        CALL DBINIT(IDVSTP,DBLUN,TOPN,LTDIR,50000,CHOP)
      ELSE
        CALL DBINIT(IDVSTP,DBLUN,TOPN,LTDIR,0,CHOP)
      ENDIF
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBINIT')
        IOK = .FALSE.
        CALL DBCLB_END
        CLOSE(UNIT=DBLUN)
        GO TO 999
      ENDIF
C
      CALL RZLDIR('//',' ')
      CALL DBLOGL(DBLUN,0)
C
      CALL_DBEND = .TRUE.              ! Must call DBEND and set true
      CDBEND(IDB) = .TRUE.
      IOK = .TRUE.
C
   97 CONTINUE
C
      IF(OPTJ) THEN
C&IF VAXVMS
        COPT = ' '
        IF(OPTX) COPT = 'X'
        IF(OPTA) COPT = 'A'
        JFILE = 'DBL3_JFILE'
        CALL DBCLB_SET_JOURNAL(TOPN,.FALSE.,JFILE,COPT)
C&ELSE
C&        WRITE(MSG_STRING,96)
C&   96   FORMAT(' No J option available on unix ')
C&        CALL ERRDB(MSG_STRING)
C&ENDIF
      ENDIF
C
  999 RETURN
      END
