      SUBROUTINE GT_DBFILE(IRUNO,DETYP,IUF,IV,CHOP,DBFILE,NFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given the detector type and run number finds
C-                  the appropriate dbl3 database and initializes it. Disk
C-                  files are to be named such as DBCALIB$XXX_SRUN_ERUN_IV.DAT,
C-                  with fatmen names; //FNAL/D0/DBL3/CALIB/XXX/SRUN_ERUN_IV
C-                  (XXX subdetector short names), where SRUN and ERUN
C-                  (start & end runs) are maximum eight digit numbers
C-                  and IV (version number)is at most 2 digit number.
C-
C-   Inputs  : IRUNO     Run number
C-             DETYP     Detector type
C-             IUF       User flag:
C-                  1  Use FATMEN to access.
C-                  2  Use FATMEN to access, if failed access DBL3$XXX area.
C-                  3  Access DBL3$XXX area. If failed Use FATMEN.
C-                  4  Do not use FATMEN.
C-                  5  Do not search. Use DBL3$XXX:DBCALIB$XXX.DAT. (Very risky
C-                     since run number and version are ignored).
C-                 11,12,13 Same as 1,2,3, except FATMEN part searches catalog
C-                     instead of disk area.
C-                 <0  Database will not be initialized (for any IUF).
C-              CHOP     Character option for DBINIT
C-             *IV*      File version number:
C-                 -1  Get the latest version. Otherwise get version IV
C-
C-   Outputs : DBFILE  Database name selected (array of 2)
C-                  DBFILE(1)  Disk file name
C-                  DBFILE(2)  Fatmen generic name
C-            *IV*     File version picked
C-             NFL     Output Flag:
C-              0  No files found on disk or catalog
C-              1  No files found for the given run number and/or version
C-              2  File accessed in DBL3$XXX area & initilaized
C-                    (if -2 initialization failed or not asked for)
C-              3  File accessed via FATMEN and initialized
C-                    (if -3 initialization failed or not asked for)
C-
C-   Controls:
C-
C-   Created   04-AUG-1992   SHAHRIAR ABACHI
C-   Modified  10-AUG-1992   SHAHRIAR ABACHI  RCP file name added to argument.
C-   Modified  23-AUG-1992   SHAHRIAR ABACHI  Fatmen & version number added.
C-                                            Arguments changed.
C-   Modified  31-AUG-1992   SHAHRIAR ABACHI  RCP removed and disk is searched.
C-   Modified  13-DEC-1993   SHAHRIAR ABACHI  Made compatible with IBM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRUNO,IUF,IV,NFL
      CHARACTER*(*) DETYP,DBFILE(2),CHOP
C
      INTEGER IER,NF,US1,US2,US3,DOT,I,CTOI,NR
      PARAMETER (NR=999)
      CHARACTER*(80) LDBFILES(300),CDBFILES(300),CTEMP,FILN
      CHARACTER*8 DBAREA
      CHARACTER*9 RUNS,RUNE
      INTEGER IRUNS(300),IRUNE(300),CONT,IFILE,MRK,MTCH(300)
      INTEGER SL,TRULEN,ID,LBANK,IR,IFOUND,VB
      INTEGER LD,SRUN(NR),ERUN(NR),FIV(NR),FVL(NR)
      INTEGER J,NLST,K,IS,IE
      INTEGER IC,EL,VL,IVN(300),CLN,CL,CHL,LUN,OKK
      LOGICAL STATUS,LIB$FIND_FILE,LIB$FIND_FILE_END,IOK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      CHARACTER*3 CHOPT
      CHARACTER*2 VSN,FCV(NR)
      CHARACTER*20 DATATYP
      CHARACTER*80 DFOUND_FILE,FOUND_FILE,FILEDB(NR),FTOFIND
      CHARACTER*80 FILNAM,GENAM
      CHARACTER*9 SRUNC(NR),ERUNC(NR)
      INTEGER MAXFIL,LKEYFA,JCONT,IREC
      INTEGER DBSVAL,DBEVAL,SVALID,EVALID
      PARAMETER (MAXFIL=1000,LKEYFA=10)
      INTEGER KEYS(LKEYFA,MAXFIL),IRC
      CHARACTER*225 DBFILES(MAXFIL)
C
C - Misc.
C
      NFL = 0
      CONT = 0
      JCONT = 0
      CTEMP = ' '
      DATATYP = ' '
      DBFILE(1) = ' '
      DBFILE(2) = ' '
      RUNS = ' '
      RUNE = ' '
      VSN = ' '
      DBSVAL = 0
      DBEVAL = 0
      CHOPT = ' '
      CALL UPCASE(DETYP, DETYP)
      CL = LEN(DETYP)
      IF(CHOP .NE. ' ') CALL UPCASE(CHOP, CHOP)
      CHL = LEN(CHOP)
      DBAREA = 'DBL3$'//DETYP(1:3)
      IF(DETYP(1:3) .EQ. 'DBM' .OR. DETYP(1:3) .EQ. 'HVM' .OR.
     &      DETYP(1:3) .EQ. 'LUM') THEN
        DATATYP = 'MON'
        LD = 3
      ELSE
        DATATYP = 'CALIB'
        LD = 5
      ENDIF
C
C - Search
C
      IF ( IABS(IUF) .EQ. 5 ) THEN
        IV = 0
        DBFILE(1) = DBAREA//':DBCALIB$'//DETYP(1:3)//'.DAT'
        STATUS =
     &      LIB$FIND_FILE(DBFILE(1), FOUND_FILE, CONT)
        IF(STATUS) THEN
          STATUS = LIB$FIND_FILE_END(CONT)
          CONT = 0
          NFL = -2
        ELSE
          NFL = 0
        ENDIF
        GOTO 998
      ENDIF
C
C - Get all names from list file
C
      FILN = 'DBL3$'//DETYP(1:3)//':'//DETYP(1:3)//'_DBFILES.DAT'
      CL = TRULEN(FILN)
      CALL GTUNIT(170,LUN,IER)
      CALL D0OPEN(LUN,FILN(1:CL),'IF',OKK)
      IRUNE(1) = 0
      DO I=1,999
        READ(LUN,8,END=9)IRUNS(I),IRUNE(I), IVN(I), LDBFILES(I)
    8   FORMAT(2I12,I10,A80)
      ENDDO
      CALL INTMSG(' Error occured during reading of dbl3 list file')
      GOTO 999
    9 CONTINUE
      IF(I .EQ. 1 .AND. IRUNE(1) .EQ. 0) THEN
        CALL INTMSG(' Empty dbl3 list file. Will exit.')
        GOTO 999
      ENDIF
      NLST = I - 1
      CLOSE(UNIT=LUN)
      DO I=1,NLST
        CALL UPCASE(LDBFILES(I), LDBFILES(I))
      ENDDO
C
      IR = 0
    2 CONTINUE
      IF(IABS(IUF) .GE. 11 .AND. IABS(IUF) .LE. 12) THEN
        GENAM = '//FNAL/D0/DBL3/'//DATATYP(1:LD)//DETYP(1:3)
        CALL FMLFIL(GENAM,DBFILES,KEYS,NF,MAXFIL,JCONT,IRC)
        IF(NF .LE. 0) GOTO 999
C
        I = 0
  111   I = I + 1
        US1 = INDEX(DBFILES(I), '_')
        IF(US1 .LT. 10) GOTO 999
        CTEMP = DBFILES(I)(US1-10:)
        US1 = INDEX(CTEMP, '/')
        IF(US1 .EQ. 0) GOTO 999
        CTEMP = CTEMP(US1+1:)//'.DAT'
        CLN = TRULEN(CTEMP)
        CDBFILES(I) = CTEMP(1:CLN)
        IF(I .LT. NF) GOTO 111
C
        NF = -1
        GOTO 5
      ELSE
        CONT = 0
        NF = 0
        CONTINUE
        FTOFIND = DBAREA//':DB*$'//DETYP(1:3)//'*.DAT'
        CL = TRULEN(FTOFIND)
   30   STATUS = LIB$FIND_FILE(FTOFIND(1:CL),DFOUND_FILE,CONT)
        IF(STATUS) THEN
          NF = NF + 1
C-
C- Machine dependent block to extract name+type (tail in unix) from filename
C- (pathname) and convert to uppercase.
C-
C&IF VAXVMS
          MRK = INDEX(DFOUND_FILE, ']')
          CDBFILES(NF) = DFOUND_FILE(MRK+1:)
          MRK = INDEX(CDBFILES(NF), ';')
          IF(MRK .GT. 0) CDBFILES(NF) = CDBFILES(NF)(1:MRK-1)
C&ELSE
C&50        CONTINUE
C&          MRK = INDEX(DFOUND_FILE, '/')
C&          IF(MRK.EQ.0)GO TO 51
C&          DFOUND_FILE = DFOUND_FILE(MRK+1:)
C&          GO TO 50
C&51        CONTINUE
C&          CALL UPCASE(DFOUND_FILE, CDBFILES(NF))
C&ENDIF
          GOTO 30
        ELSE
          STATUS = LIB$FIND_FILE_END(CONT)
          CONT = 0
          IF(NF .EQ. 0) GOTO 999
        ENDIF
      ENDIF
C
      K = 0
      DO I=1,NF
        MTCH(I) = 0
        DO J=1,NLST
          IF(CDBFILES(I) .EQ. LDBFILES(J)) THEN
            K = K + 1
            MTCH(K) = J
          ENDIF
        ENDDO
      ENDDO
C
    5 CONTINUE
      RUNS = ' '
      RUNE = ' '
      VSN = ' '
      IC = 0
C
C - Search. Don't care about version match
C
      DO I=1,MIN0(NLST,NF)
        J = MTCH(I)
C        IF(CDBFILES(I)(1:2) .EQ. 'DB') THEN
C          US1 = INDEX(CDBFILES(I), '_')
C          IF(US1 .EQ. 0) THEN
C            GOTO 100
C          ENDIF
C        ELSE
C          US1 = 0
C        ENDIF
C        CTEMP = CDBFILES(I)(US1+1:)
C        US2 = INDEX(CTEMP, '_')
C        IF(US2 .EQ. 0) THEN
C          GOTO 100
C        ENDIF
C        RUNS = CTEMP(1:US2-1)
C        CTEMP = CTEMP(US2+1:)
C        DOT = INDEX(CTEMP, '.')
C        IF(DOT .EQ. 0) THEN
C          GOTO 100
C        ENDIF
C        US3 = INDEX(CTEMP, '_')
C        IF(US3 .EQ. 0) THEN
C          VSN = '0'
C          RUNE = CTEMP(1:DOT-1)
C        ELSE
C          VSN = CTEMP(US3+1:DOT-1)
C          RUNE = CTEMP(1:US3-1)
C        ENDIF
C
C        SL = TRULEN(RUNS)
C        EL = TRULEN(RUNE)
C        VL = TRULEN(VSN)
C        IRUNS = CTOI(RUNS(1:SL))
C        IRUNE = CTOI(RUNE(1:EL))
C        IVN = CTOI(VSN(1:VL))
C
        IF(IRUNO .GE. IRUNS(J) .AND. IRUNO .LE. IRUNE(J)) THEN
          IC = IC + 1
          FILEDB(IC) = LDBFILES(J)
          SRUN(IC) = IRUNS(J)
          ERUN(IC) = IRUNE(J)
          FIV(IC) = IVN(J)
        ENDIF
      ENDDO
C
      NFL = 1
      IF (IC .EQ. 0) THEN
        IF(IR .LT. 2 .AND. IABS(IUF) .EQ. 13) THEN
          IR = IR + 1
          GOTO 2
        ELSE
          GOTO 999
        ENDIF
      ENDIF
C
C - Search. Now care about version match.
C
      IFOUND = 0
      VB = -1
      DO I=1,IC
        IF (IV .LT. 0) THEN
          IF (FIV(I) .GT. VB) THEN
            IFOUND = I
            VB = FIV(I)
          ENDIF
        ELSE
          IF (FIV(I) .EQ. IV) THEN
            IFOUND = I
          ENDIF
        ENDIF
  100 ENDDO
C
      IF(IFOUND .GT. 0) THEN
        DBSVAL = SRUN(IFOUND)
        DBEVAL = ERUN(IFOUND)
        IF(IV .LT. 0) IV = FIV(IFOUND)
        WRITE(SRUNC(IFOUND), '(I9.0)') SRUN(IFOUND)
	CALL   SWORDS(SRUNC(IFOUND),IS,IE,SL)
	SRUNC(IFOUND) = SRUNC(IFOUND)(IS:IE)
        WRITE(ERUNC(IFOUND), '(I9.0)') ERUN(IFOUND)
	CALL   SWORDS(ERUNC(IFOUND),IS,IE,SL)
	ERUNC(IFOUND) = ERUNC(IFOUND)(IS:IE)
        WRITE(FCV(IFOUND), '(I2.0)') FIV(IFOUND)
	CALL   SWORDS(FCV(IFOUND),IS,IE,SL)
	FCV(IFOUND) = FCV(IFOUND)(IS:IE)
        DBFILE(1) = DBAREA//':'//FILEDB(IFOUND)
        DBFILE(2) =
     &     '//FNAL/D0/DBL3/'//DATATYP(1:LD)//'/'//DETYP(1:3)//
     &     '/'//SRUNC(IFOUND)(1:SL)//'_'//ERUNC(IFOUND)(1:EL)
        IF(FIV(IFOUND) .GT. 0) THEN
          CLN = TRULEN(DBFILE(2))
          DBFILE(2) = DBFILE(2)(1:CLN)//'_'//
     &                             FCV(IFOUND)(1:FVL(IFOUND))
        ENDIF
        IF(IABS(IUF) .EQ. 11 .OR. IABS(IUF) .EQ. 12 .OR.
     &      (IABS(IUF) .EQ. 13 .AND. IR .GT. 0)) THEN
          NFL = -3
        ELSE
          STATUS =
     &      LIB$FIND_FILE(DBFILE(1), FOUND_FILE, CONT)
          IF(STATUS) THEN
            STATUS = LIB$FIND_FILE_END(CONT)
            CONT = 0
            NFL = -2
          ELSE
            NFL = 1
          ENDIF
        ENDIF
      ELSE
        NFL = 1
        GOTO 999
      ENDIF
C
C - Initialize
C
  998 CONTINUE
      IF(IUF .EQ. 1) THEN
        CHOPT(1:CHL) = CHOP
        FILNAM = DBFILE(2)
        CL = TRULEN(FILNAM)
        CALL DBCLB_INIT(FILNAM(1:CL),CHOPT(1:CHL),IOK)
        IF (IOK) NFL = IABS(NFL)
      ENDIF
C
      I = 0
      IF(IUF .EQ. 2) THEN
        CHOPT(1:CHL) = CHOP
        FILNAM = DBFILE(2)
        CL = TRULEN(FILNAM)
   10   CALL DBCLB_INIT(FILNAM(1:CL),CHOPT(1:CHL),IOK)
        IF (IOK) THEN
          NFL = IABS(NFL)
        ELSE
          IF(I .LT. 1) THEN
            I = I + 1
            CHOPT(1:CHL) = CHOP
            FILNAM = DBFILE(1)
            GOTO 10
          ENDIF
        ENDIF
      ENDIF
C
      I = 0
      IF(IUF .EQ. 3) THEN
        CHOPT(1:CHL) = CHOP
        FILNAM = DBFILE(1)
        CL = TRULEN(FILNAM)
   20   CALL DBCLB_INIT(FILNAM(1:CL),CHOPT(1:CHL),IOK)
        IF (IOK) THEN
          NFL = IABS(NFL)
        ELSE
          IF(I .LT. 1) THEN
            I = I + 1
            CHOPT(1:CHL) = CHOP
            FILNAM = DBFILE(2)
            GOTO 20
          ENDIF
        ENDIF
      ENDIF
C
      IF(IUF .EQ. 4 .OR. IUF .EQ. 5) THEN
        CHOPT(1:CHL) = CHOP
        IF(IUF .EQ. 4) THEN
          FILNAM = DBFILE(1)
        ELSE
          FILNAM =
     &      DBAREA//':DB'//DATATYP(1:LD)//'$'//DETYP(1:3)//'.DAT'
        ENDIF
        CL = TRULEN(FILNAM)
        CALL DBCLB_INIT(FILNAM(1:CL),CHOPT(1:CHL),IOK)
        IF(IOK) THEN
          NFL = IABS(NFL)
        ENDIF
      ENDIF
C ----------------------------------------------------------------------
  999 CONTINUE
      DO I=1,IC
        J = MTCH(I)
        IRUNS(J) = 0
        IRUNE(J) = 0
        IVN(J) = 0
      ENDDO
      CALL RLUNIT(170,LUN,IER)
 1999 RETURN
C
C#######################################################################
      ENTRY GT_DBFILE_VALIDITY (SVALID, EVALID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get database validity range. Should be
C-                         called after gt_dbfiles is called
C-
C-   Inputs  : 
C-   Outputs : SVALID  Database start validity
C-             EVALID  Database end validity
C-                if either =0, then not available and either should be used
C-   Controls: 
C-
C-   Created  20-JAN-1994   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
C
      SVALID = DBSVAL
      EVALID = DBEVAL
C----------------------------------------------------------------------
 2999 RETURN
      END
