      SUBROUTINE DBCLB_INITIALIZE(DBFILE1,CHOPT,IOK)
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
C-          default is local mode.
C-        = 'D' Create D0 FZ journal files to be used for database update.
C- *NOTE:: Update Privileges must be used only by CALIB/ENDTSK.
C-
C-   Outputs :  IOK    - .TRUE. if no error
C-   Controls:
C-
C-   Created  17-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Updated   2-SEP-1992   Chip Stewart  moved code to DBCLB_INIT
C-   Updated  15-DEC-1992   Haowei XU  Added Level 0
C-   Updated  10-JAN-1994   S. Abachi  LVSN added and LFORCE handling modified
C-   Updated  21-JAN-1994   S. Abachi  choice of new TOPN added (New entry).
C-   Updated  24-JAN-1994   S. Abachi  Reading server choice added
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DBSTP.INC'
      CHARACTER*(*) DBFILE1
      CHARACTER*80 DBFILE, DBFILE2(2), LOCAL_DBFILE
      CHARACTER*(*) CHOPT
      LOGICAL FIRST,OK
      INTEGER I,J,K,L,IOS,IOK
      INTEGER IUF,IV,RUNNO,IRUN,DBFLG
      INTEGER FORCE_RUN,JRUN,IVSN,FORCE_VSN
      CHARACTER*5 TOPDN
      CHARACTER  DET*3, CHOP*3, MSG*80
      INTEGER IER,NITEM,ITEM(6),IDB
C----------------------------------------------------------------------
      CHOP = ' '
      DBFILE = DBFILE1
      CALL UPCASE(DBFILE, DBFILE)
      IRUN = RUNNO()
      IF(LFORCE) IRUN = FORCE_RUN
      IF (IRUN.GT.0) THEN
        I = INDEX(DBFILE,'DBCALIB$')
        DET = DBFILE(I+8:I+10)
        IDB = 0
        CALL DBCLB_DETSEQ(DET,IDB)
        IF(IDB .GT. 0) GOTO 20
        CALL SWORDS(DBFILE,I,J,K)
        MSG = 'ARGUMENT '//DBFILE(I:J)//' MISSING DET AFTER DBCALIB$'
        CALL ERRMSG(' FAIL DBL3 ACCESS ','DBCLB_INITIALIZE',MSG,'W')
        IOK = .FALSE.
        GOTO 999
C
C ****  THESE VALUES ARE TEMPORARY DEFAULTS
C
   20   CONTINUE
        CALL DBL3INIT_RSRVR(DET, ' ')
        IUF = 4
        IV = -1
        IF(LVSN) IV = FORCE_VSN
        CHOP = CHOPT
C
        IF(RCSERVER) THEN
          NITEM = 2
          ITEM(1) = IUF
          ITEM(2) = IV
C&IF VAXVMS,VAXELN
          IER = 1
          CALL INTMSG
     &     ('DBCLB_INITIALIZE: RCSERVER NOT AVAILABLE ON THIS MACHINE.')
C&ELSE
C&          CALL D0DBL3_RCLIENT_INIT(NITEM,ITEM,IER)
C&ENDIF
          IF(IER.EQ.0) THEN
            IOK = .TRUE.
          ELSE
            IOK = .FALSE.
          ENDIF
          GOTO 999
        ENDIF
C
        CALL GT_DBFILE(IRUN,DET,IUF,IV,CHOP,DBFILE2,DBFLG)
        IF (DBFLG.GT.1) THEN
          CALL SWORDS(DBFILE2(1),I,J,K)
          IF(K.EQ.0) THEN
            MSG = 'BLANK DBFILE FOR '//DET
            CALL ERRMSG('ERROR IN GT_DBFILE','DBCLB_INITIALIZE',MSG,'W')
            IOK = .FALSE.
            GOTO 999
          END IF
          MSG = ' OPENED DBL3 FILE '//DBFILE2(1)(I:J)
          CALL INTMSG(MSG)
          IOK = .TRUE.
        ELSE
          IOK = .FALSE.
          MSG = 'GT_DBFILE ERROR - '//DET
          CALL ERRMSG(' FAIL DBL3 ACCESS ','DBCLB_INITIALIZE',MSG,'W')
        END IF
      ELSE
        CALL DBCLB_INIT(DBFILE1,CHOPT,IOK)
      END IF
  999 RETURN
C#######################################################################
      ENTRY DBCLB_INITIALIZE_FORCE_RUN(JRUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       FORCE DBCLB_INITIALIZE TO USE JRUN RATHER THAN RUNNO()
C-
C-   Inputs  : JRUN [I] - RUN NUMBER FORCED TO USE RATHER THAN RUNNO()
C-
C-   Created   2-SEP-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      CALL DBL3INIT_RSRVR('DUM', 'A')
      LFORCE = .TRUE.
      FORCE_RUN = JRUN
C----------------------------------------------------------------------
 1999 RETURN
C -
C - %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C -
      ENTRY DBCLB_INITIALIZE_FORCE_VSN (IVSN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Force RECO to use IVSN of the database rather
C-                         than lates version
C-
C-   Inputs  : IVSN  Database version to be forced
C-
C-   Created  07-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      CALL DBL3INIT_RSRVR('DUM', 'A')
      LVSN = .TRUE.
      FORCE_VSN = IVSN
C----------------------------------------------------------------------
 2999 RETURN
C -
C - %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C -
      ENTRY DBCLB_INITIALIZE_FORCE_TOPNAME (TOPDN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Force RECO to use IVSN of the database rather
C-                         than lates version
C-
C-   Inputs  : TOPDN   Top directory name (should be C*5)
C-
C-   Created  22-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      CALL DBL3INIT_RSRVR('DUM', 'A')
      TOPN = TOPDN
C----------------------------------------------------------------------
 3999 RETURN
C
      END
