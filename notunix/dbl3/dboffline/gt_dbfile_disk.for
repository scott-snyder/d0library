      SUBROUTINE GT_DBFILE_DISK(IRT,DTYP,IV,IFC,DBFILE,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : According to run number (or time key for some
C-                         databases) returns the appropriate database filename
C-                         as found on the DBL3$XXX area. Also can fill
C-                         DBL3LIST.INC common block with lines of info.
C-
C-   Inputs  : IRT        Run number or Time key (for time dependence).
C-             DTYP       3 character database type (such as CAL, LUM, etc.).
C-             IV         Database version (if -1 then latest version)
C-             IFC        1=Fill common block with information,0=do not
C-
C-   Outputs :  DBFILE  Database files found
C-              IRET    0 = File found. (success)
C-                      1 = File to match not found
C-   Controls:
C-
C-   Created  26-OCT-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRT,IV,IFC,IRET
      CHARACTER*(*) DTYP,DBFILE
      INCLUDE 'D0$INC:DBL3LIST.INC'
C
      INTEGER IER,NF,US1,US2,US3,DOT,I,CTOI
      CHARACTER*(80) LDBFILES(NDB),CDBFILES(NDB)
      CHARACTER*(80) JDBFILES(NDB),CTEMP,FILN
      CHARACTER*8 DBAREA
      CHARACTER*9 RUNS,RUNE
      INTEGER IRUNS,IRUNE,CONT,IFILE,MRK,MTCH(NDB)
      INTEGER SL,TRULEN,ID,LBANK,IR,IFOUND,VB,SL1,EL1,VL1,ELM
      INTEGER LD,SRUN(NDB),ERUN(NDB),FIV(NDB),FVL(NDB)
      INTEGER J,NLST,K,IS,IE,MAXRUNE,SFLG
      INTEGER IC,EL,VL,IVN,CLN,CL,LC,IJ,IJK
      LOGICAL STATUS,LIB$FIND_FILE,LIB$FIND_FILE_END,IOK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      CHARACTER*1 CHOPT
      CHARACTER*2 VSN,FCV(NDB)
      CHARACTER*20 DATATYP
      CHARACTER*80 DFOUND_FILE,FOUND_FILE,FILEDB(NDB),FTOFIND
      CHARACTER*80 FILNAM,GENAM
      CHARACTER*9 SRUNC(NDB),ERUNC(NDB)
      INTEGER MAXFIL,LKEYFA,JCONT,IREC
      PARAMETER (MAXFIL=1000,LKEYFA=10)
      INTEGER KEYS(LKEYFA,MAXFIL),IRC
      CHARACTER*225 DBFILES(MAXFIL)
C
C - Misc.
C
      IRET = 1
      CONT = 0
      JCONT = 0
      CTEMP = ' '
      DATATYP = ' '
      DBFILE = ' '
      RUNS = ' '
      RUNE = ' '
      VSN = ' '
      IF(IFC .GT. 0) THEN
        DO I=1,NLINES
          DBLINES(I) = ' '
        ENDDO
        NLINES = 0
      ENDIF
      CL = LEN(DTYP)
      CALL UPCASE(DTYP(1:CL), DTYP(1:CL))
      DBAREA = 'DBL3$'//DTYP(1:3)
      IF(DTYP(1:3) .EQ. 'DBM' .OR. DTYP(1:3) .EQ. 'HVM' .OR.
     &      DTYP(1:3) .EQ. 'LUM') THEN
        DATATYP = 'MON'
        LD = 3
      ELSE
        DATATYP = 'CALIB'
        LD = 5
      ENDIF
C
C - Search
C
      CONT = 0
      NF = 0
      CONTINUE
      FTOFIND = DBAREA//':DB*$'//DTYP(1:3)//'*_*'//'*.DAT'
      CL = TRULEN(FTOFIND)
   30 STATUS = LIB$FIND_FILE(FTOFIND(1:CL),DFOUND_FILE,CONT)
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
C
    5 CONTINUE
      RUNS = ' '
      RUNE = ' '
      VSN = ' '
      IC = 0
C
C - Search. Don't care about version match
C
      SFLG = 0
      DO I=1,NF
        US1 = INDEX(CDBFILES(I), '_')
        CTEMP = CDBFILES(I)(US1+1:)
        US2 = INDEX(CTEMP, '_')
        IF(US2 .EQ. 0) THEN
          CALL UPCASE(CTEMP, CTEMP)
          IF(CTEMP(1:4) .EQ. 'SRVR') THEN
            SFLG = 1
            IJK = I
          ENDIF
          GOTO 100
        ENDIF
        RUNS = CTEMP(1:US2-1)
        CTEMP = CTEMP(US2+1:)
        DOT = INDEX(CTEMP, '.')
        IF(DOT .EQ. 0) THEN
          GOTO 100
        ENDIF
        US3 = INDEX(CTEMP, '_')
        IF(US3 .EQ. 0) THEN
          VSN = '0'
          RUNE = CTEMP(1:DOT-1)
        ELSE
          VSN = CTEMP(US3+1:DOT-1)
          RUNE = CTEMP(1:US3-1)
        ENDIF
C
        SL = TRULEN(RUNS)
        EL = TRULEN(RUNE)
        VL = TRULEN(VSN)
        IRUNS = CTOI(RUNS(1:SL))
        IRUNE = CTOI(RUNE(1:EL))
        IF(IRUNE .GT. MAXRUNE) THEN
          MAXRUNE = IRUNE
          ELM = EL
        ENDIF
        IF(IRUNS .EQ. 0 .OR. IRUNE .EQ. 0) GOTO 100
        IVN = CTOI(VSN(1:VL))
        CALL UPCASE(CDBFILES(I), CDBFILES(I))
C
C -   Fill common block if asked
C
        IF(IFC .GT. 0) THEN
          NLINES = NLINES + 1
          LC = TRULEN(CDBFILES(I))
          SL1 = 12 - SL
          EL1 = 12 - EL
          VL1 = 10 - VL
          WRITE(DBLINES(I),'(I<SL>,<SL1>X,I<EL>,
     &        <EL1>X,I<VL>,<VL1>X,A<LC>)') IRUNS,IRUNE,IVN,CDBFILES(I)
        ENDIF
C
C -   Now find the match
C
        IF(IRT .GE. IRUNS .AND. IRT .LE. IRUNE) THEN
          IC = IC + 1
          JDBFILES(IC) = CDBFILES(I)
          FIV(IC) = IVN
        ENDIF
  100 ENDDO
C
C - Find server conected database
C
      IF(SFLG .EQ. 1) THEN
        IRUNS = MAXRUNE + 1
        IRUNE = 99999999
        IVN = 0
        NLINES = NLINES + 1
        LC = TRULEN(CDBFILES(IJK))
        SL = ELM
        EL = 8
        VL = 1
        SL1 = 12 - SL
        EL1 = 12 - EL
        VL1 = 10 - VL
        WRITE(DBLINES(NLINES),'(I<SL>,<SL1>X,I<EL>,
     &        <EL1>X,I<VL>,<VL1>X,A<LC>)')
     &        IRUNS,IRUNE,IVN,CDBFILES(IJK)
        IF(IRT .GE. IRUNS .AND. IRT .LE. IRUNE) THEN
          IC = IC + 1
          JDBFILES(IC) = CDBFILES(IJK)
          FIV(IC) = IVN
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
      ENDDO
C
      IF(IFOUND .GT. 0) THEN
        DBFILE = JDBFILES(IFOUND)
        IRET = 0
      ENDIF
C
C----------------------------------------------------------------------
C
  999 CONTINUE
      RETURN
      END
