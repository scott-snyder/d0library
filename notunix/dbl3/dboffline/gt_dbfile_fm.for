      SUBROUTINE GT_DBFILE_FM(IRT,DTYP,IV,IFC,GNAME,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : According to run number (or time key for some
C-                         databases) returns the appropriate database filename
C-                         as found from FATMEN catalog. Also fills
C-                         common block DBL3LIST.INC with all relevant
C-                         databases information.
C-
C-   Inputs  : IRT     Run number or Time key (for time dependent databases).
C-             DTYP    3 character database type (such as CAL, CDC, LUM, etc.)
C-             IV      Database version
C-             IFC     1=fill DBL3LIST.INC, 0=do not
C-
C-   Outputs : GNAME   Database file generic name
C-             IRET     0 = File found
C-                      1 = No match found
C-   Controls:
C-
C-   Created  26-OCT-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRT,IV,IFC,IRET
      CHARACTER*(*) DTYP,GNAME
      INCLUDE 'D0$INC:DBL3LIST.INC'
C
      INTEGER IER,NF,USS,US1,US2,US3,DOT,I,CTOI,NR
      PARAMETER (NR=999)
      CHARACTER*(80) GNAMES(300),JGNAMES(300)
      CHARACTER*(80) CTEMP,CTEMP2
      CHARACTER*9 RUNS,RUNE
      INTEGER IRUNS,IRUNE,MAXRUNE,LC,EL1,VL1
      INTEGER SL,TRULEN,ID,LBANK,IR,IFOUND,VB,ELM
      INTEGER LD,SRUN(NR),ERUN(NR),FIV(NR),FVL(NR)
      INTEGER J,NLST,K,IS,IE
      INTEGER IC,EL,VL,IVN,CLN,CL,LUN,OKK
      LOGICAL FIRST
      CHARACTER*20 DATATYP
      CHARACTER*2 VSN,FCV(NR)
      CHARACTER*80 DFOUND_FILE,FOUND_FILE,FILEDB(NR),FTOFIND
      CHARACTER*80 FILNAM,GENAM
      INTEGER MAXFIL,LKEYFA,JCONT,IREC
      PARAMETER (MAXFIL=1000,LKEYFA=10)
      INTEGER KEYS(LKEYFA,MAXFIL),IRC,SL1,SL2
      CHARACTER*225 DBFILES(MAXFIL)
      DATA FIRST /.TRUE./
C
C - Misc.
C
      JCONT = 0
      GNAME = ' '
      IF(IFC .GT. 0) THEN
        DO I=1,NLINES
          DBLINES(I) = ' '
        ENDDO
        NLINES = 0
      ENDIF
      CL = LEN(DTYP)
      CALL UPCASE(DTYP(1:CL), DTYP(1:CL))
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
      IR = 0
      IC = 0
    2 CONTINUE
      GENAM = '//FNAL/D0/DBL3/'//DATATYP(1:LD)//DTYP(1:3)
      CALL FMLFIL(GENAM,DBFILES,KEYS,NF,MAXFIL,JCONT,IRC)
      IF(NF .LE. 0) GOTO 999
      DO I = 1,NF
        CTEMP = ' '
        RUNS = ' '
        RUNE = ' '
        VSN = ' '
        US1 = INDEX(DBFILES(I), '_')
        IF(US1 .LT. 10) GOTO 999
        US2 = INDEX(CTEMP2, '_')
        IF(US2 .GT. 0) THEN
          RUNE = CTEMP2(1:US2-1)
          VSN = CTEMP2(US2+1:)
        ELSE
          RUNE = CTEMP2(1:)
        ENDIF
        CTEMP = ' '
        CTEMP2 = ' '
        USS = US1 - 12
    9   CONTINUE
        CTEMP = DBFILES(I)(USS:)
        SL1 = INDEX(CTEMP, '/')
        CTEMP2 = CTEMP(SL1+1:)
        SL2 = INDEX(CTEMP2, '/')
        IF(SL2 .NE. 0) THEN
          USS = USS + 1
          GOTO 9
        ENDIF
        US1 = INDEX(CTEMP, '_')
        IF(SL1 .EQ. 0) GOTO 999
        RUNS = CTEMP(SL1+1:US1-1)
        GNAMES(I) = CTEMP(SL1+1:)
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
        CALL UPCASE(GNAMES(I), GNAMES(I))
C
        IF(IFC .GT. 0) THEN
          NLINES = NLINES + 1
          LC = TRULEN(GNAMES(I))
          SL1 = 12 - SL
          EL1 = 12 - EL
          VL1 = 10 - VL
          WRITE(DBLINES(I),'(I<SL>,<SL1>X,I<EL>,
     &        <EL1>X,I<VL>,<VL1>X,A<LC>)') IRUNS,IRUNE,IVN,GNAMES(I)
        ENDIF
C
        IF(IRT .GE. IRUNS .AND. IRT .LE. IRUNE) THEN
          IC = IC + 1
          JGNAMES(IC) = GNAMES(I)
          SRUN(IC) = IRUNS
          ERUN(IC) = IRUNE
          FIV(IC) = IVN
        ENDIF
      ENDDO
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
        GNAME  = JGNAMES(IFOUND)
        IRET = 0
      ENDIF
C
C----------------------------------------------------------------------
C
  999 CONTINUE
      RETURN
      END
