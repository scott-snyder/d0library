      SUBROUTINE D0CSOPEN(LUN,FILNAM,CHOPT,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open a data exchange mode file (32760 byte 
C-                         records) remotely.  No other format is allowed.
C-                         Filename argument must contain a node name 
C-                         terminated by a double colon (::) and a filename
C-                         acceptable to the remote node.  FZFILE is called
C-                         internally.
C-
C-   Inputs  :
C-   LUN   = unit number
C-   FILNAM= file name
C-   CHOPT = character option
C-           'I' Input
C-           'O' Output
C-           'S' Separated data sets.
C-   Outputs :
C-   OK    = set to false if there is a problem opening file
C-           true otherwise
C-
C-   Created 21-Dec-1993   Herbert B. Greenlee
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:CZSOCK.INC'
      INCLUDE 'D0$INC:QUEST.INC/LIST'
      CHARACTER*(*) FILNAM,CHOPT
      CHARACTER*1 CH
      INTEGER LUN
      LOGICAL OK
      CHARACTER*80 CTEMP
      CHARACTER*12 NODE, ONODE, OUSER, XCHOPT
      CHARACTER*80 PATH,OPATH(99), CWD
      CHARACTER*255 LOCAL_FILENAME, REMOTE_FILE
      INTEGER OSKIN, OSKOUT
      INTEGER I, K, L, IOPT
      INTEGER LR,LN,LP,IRC
      LOGICAL VMS, OPTI, OPTO, OPTD, OPTS
      INTEGER TRULEN
      INTEGER XZRL
      EXTERNAL D0CSBUF, D0CSBUFD
      DATA XZRL/8190/
C----------------------------------------------------------------------
C
      IF(LUN.LT.1.OR.LUN.GT.99)THEN
        CALL ERRMSG('D0CSOPEN','D0CSOPEN',
     &    'Unit number out of range (1-99)','W')
        OK = .FALSE.
        GO TO 999
      ENDIF
      CALL D0CSINIT(LUN)
C
C       find options
C
      L = INDEX(CHOPT, '/') - 1
      IF(L.LT.0)L = LEN(CHOPT)
      OPTI = .FALSE.
      OPTO = .FALSE.
      OPTS = .FALSE.
      DO 1 I=1,L
        CALL UPCASE(CHOPT(I:I),CH)
        IF(CH.EQ.'I')OPTI = .TRUE.
        IF(CH.EQ.'O')OPTO = .TRUE.
        IF(CH.EQ.'S')OPTS = .TRUE.
    1 CONTINUE
      IOPT = 1
C
C- Look for 'I' option (open for input)
C
      IF(OPTI)THEN
        IOPT=1
      ENDIF
C
C- Look for 'O' option (open for output)
C
      IF(OPTO)THEN
        IOPT=2
      ENDIF
C-
C- Parse filename into node, path and filename.
C-
      LOCAL_FILENAME = FILNAM
      K = INDEX(LOCAL_FILENAME, '::')
      IF(K.NE.0)THEN
        NODE = LOCAL_FILENAME(1:K-1)
        REMOTE_FILE = LOCAL_FILENAME(K+2:)
      ELSE
        CTEMP = 'Missing node name in '//LOCAL_FILENAME
        CALL ERRMSG('D0CSOPEN','D0CSOPEN',CTEMP,'W')
      ENDIF
      PATH = ' '
  300 CONTINUE
      K = INDEX(REMOTE_FILE, '/')
      IF(K.EQ.0)K = INDEX(REMOTE_FILE, ']')
      IF(K.EQ.0)GO TO 301
      PATH = PATH(1:TRULEN(PATH))//REMOTE_FILE(1:K)
      REMOTE_FILE = REMOTE_FILE(K+1:)
      GO TO 300
  301 CONTINUE
      LN = TRULEN(NODE)
      LP = TRULEN(PATH)
      LR = TRULEN(REMOTE_FILE)
C-
C- Open communication with the remote node.  Decide if we are talking to a 
C- VMS or UNIX node so we can work around a bug in the VMS server.
C-
      IF(NODE.NE.CHNODE(LUN))THEN
        CALL CZOPEN('zserv', NODE(1:LN), IRC)
        IF(IRC.NE.0)THEN
          CTEMP = 'CZOPEN failed for node '//NODE(1:LN)
          CALL ERRMSG('CZOPEN failed','D0CSOPEN',CTEMP,'W')
          OK = .FALSE.
          GO TO 999
        ENDIF
        CHNODE(LUN) = NODE
        JSKIN(LUN) = ISKIN
        JSKOUT(LUN) = ISKOUT
        OPATH(LUN) = ' '
      ENDIF
C- VMS node?
      CALL XZPWD(CWD, IRC)
      IF(IRC.NE.0)THEN
        CTEMP = 'XZPWD failed'
        CALL ERRMSG('XZCD failed','D0CSOPEN',CTEMP,'W')
        OK = .FALSE.
        GO TO 999
      ENDIF
      VMS = CWD(1:1).NE.'/'
C- Set remote path.
      IF(PATH.NE.OPATH(LUN))THEN
        CALL XZCD(PATH(1:LP),IRC)
        OPATH(LUN) = PATH
      ENDIF
      IF(IRC.NE.0)THEN
        CTEMP = 'XZCD failed for path '//PATH(1:LP)
        CALL ERRMSG('XZCD failed','D0CSOPEN',CTEMP,'W')
        OK = .FALSE.
        GO TO 999
      ENDIF
C
C- Now open file using XZOPEN.
C
      IF(IOPT.LT.2) THEN
C
C- Open existing file for read
C
        IF(VMS)THEN
          CALL XZOPEN(LUN,REMOTE_FILE(1:LR),
     &      NODE(1:LN),4*XZRL,'I',IRC)
        ELSE
          CALL XZOPEN(LUN,REMOTE_FILE(1:LR),
     &      NODE(1:LN),4*XZRL,'ID',IRC)
        ENDIF
        IF(IRC.NE.0)THEN
          CTEMP = 'XZOPEN failed for file '//REMOTE_FILE(1:LR)
          CALL ERRMSG('XZOPEN failed','D0CSOPEN',CTEMP,'W')
          OK = .FALSE.
          GO TO 999
        ENDIF
        IF(OPTS)THEN
          XCHOPT = 'SCXI'
        ELSE
          XCHOPT = 'CXI'
        ENDIF
        CALL FZFILE(LUN,XZRL,XCHOPT)
        IF(VMS)THEN
          CALL FZHOOK(LUN,D0CSBUF,0)
        ELSE
          CALL FZHOOK(LUN,D0CSBUFD,0)
        ENDIF
C
      ELSEIF(IOPT.EQ.2) THEN
C
C- Open new file for write
C
        CALL XZOPEN(LUN,REMOTE_FILE(1:LR),
     &    NODE(1:LN),4*XZRL,'OD',IRC)
        IF(IRC.NE.0)THEN
          CTEMP = 'XZOPEN failed for file '//REMOTE_FILE(1:LR)
          CALL ERRMSG('XZOPEN failed','D0CSOPEN',CTEMP,'W')
          OK = .FALSE.
          GO TO 999
        ENDIF
        IF(OPTS)THEN
          XCHOPT = 'SCXO'
        ELSE
          XCHOPT = 'CXO'
        ENDIF
        CALL FZFILE(LUN,XZRL,XCHOPT)
        CALL FZHOOK(LUN,D0CSBUFD,0)
      ENDIF
C
      GO TO 999
C
      ENTRY D0CSCLOSE(LUN, CHOPT, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close a file opened by D0CSOPEN.
C-
C-   Inputs  : LUN   = unit number
C-             CHOPT = Character options:
C-                     'D' - Delete file (Only for Fortran CLOSE).
C-   Outputs : OK    = .TRUE. if operation succeeded
C-
C-   Created 30-Dec-1992    Herbert B. Greenlee
C-
C----------------------------------------------------------------------
      L = INDEX(CHOPT, '/') - 1
      IF(L.LT.0)L = LEN(CHOPT)
      OK=.TRUE.
C-
C- Scan options.
C-
      OPTD = .FALSE.
      DO I=1,L
        CALL UPCASE(CHOPT(I:I),CH)
        IF(CH.EQ.'D')OPTD = .TRUE.
      ENDDO
C
      ONODE = CHNODE(LUN)
      OUSER = CHUSER(LUN)
      OSKIN = JSKIN(LUN)
      OSKOUT = JSKOUT(LUN)
      IF(OPTD)THEN
        CALL XZCLOS(LUN, 'D', IRC)
      ELSE
        CALL XZCLOS(LUN, ' ', IRC)
      ENDIF
      CHNODE(LUN) = ONODE
      CHUSER(LUN) = OUSER
      JSKIN(LUN) = OSKIN
      JSKOUT(LUN) = OSKOUT
      IF(IRC.NE.0)OK = .FALSE.
C
  999 RETURN
      END
