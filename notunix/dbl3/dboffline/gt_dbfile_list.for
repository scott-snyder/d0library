      SUBROUTINE GT_DBFILE_LIST(IRT,DTYP,IV,LIST_FILE,IFC,DBFILE,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : According to run number (or time key for some
C-                         databases) returns the appropriate database filename
C-                         as found on the XXX_DBFILES.DAT list. Also provides
C-                         the conetents of list file in common block
C-                         DBL3LIST.INC
C-
C-   Inputs  : IRT        Run number or Time key (for time dependence).
C-             DTYP       3 character database type (such as CAL, LUM, etc.).
C-             IV         Database version (if -1 then latest version)
C-             LIST_FILE  If non-empty character, then is used instead of
C-                        DBL3$XXX:XXX_DBFILES.DAT to search for files.
C-             IFC        1=Fill common block with information,0=do not
C-
C-   Outputs :  DBFILE  Database files found
C-              IRET    0 = File(s) found. (success)
C-                      1 = List file not found.
C-                      2 = File to match not found
C-   Controls:
C-
C-   Created  26-OCT-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IRT,IV,IFC,IRET
      CHARACTER*(*) DTYP,LIST_FILE,DBFILE
      INCLUDE 'D0$INC:DBL3LIST.INC'
C
      INTEGER IER,I
      CHARACTER*(80) JDBFILES(NDB),LDBFILES(NDB)
      CHARACTER*(80) FILN
      CHARACTER*8 DBAREA
      CHARACTER*9 RUNS,RUNE
      INTEGER IRUNS(NDB),IRUNE(NDB)
      INTEGER SL,TRULEN,ID,LBANK,IR,IFOUND,VB
      INTEGER SRUN(NDB),ERUN(NDB),FIV(NDB),FVL(NDB)
      INTEGER J,NLST,K,IS,IE
      INTEGER IC,EL,VL,IVN(NDB),CLN,CL,LUN,OK
      LOGICAL STATUS,LIB$FIND_FILE,LIB$FIND_FILE_END,IOK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      CHARACTER*2 VSN,FCV(NDB)
      CHARACTER*9 SRUNC(NDB),ERUNC(NDB)
C
      IRET = 2
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
C
C - Get all names from list file
C
      IF(LIST_FILE .EQ. ' ') THEN
        FILN = 'DBL3$'//DTYP(1:3)//':'//DTYP(1:3)//'_DBFILES.DAT'
      ELSE
        FILN = LIST_FILE
      ENDIF
      CL = TRULEN(FILN)
      CALL GTUNIT(170,LUN,IER)
      CALL D0OPEN(LUN,FILN(1:CL),'IF',OK)
      IF(.NOT. OK) THEN
        IRET = 1
        GOTO 99
      ENDIF
      READ(LUN,8,END=9)
     &        (IRUNS(I),IRUNE(I), IVN(I), LDBFILES(I), I=1,999)
    8 FORMAT(2I12,I10,A80)
      CALL INTMSG(' Error occured during reading of dbl3 list file')
      GOTO 99
    9 NLST = I - 1
C
C - Fill common block if asked
C
      IF(IFC .GT. 0) THEN
        REWIND(LUN)
        READ(LUN,10,END=11) (DBLINES(I), I=1,999)
   10   FORMAT(A120)
        CALL INTMSG(' Error occured during reading of dbl3 list file')
        GOTO 99
   11   NLINES = I - 1
      ENDIF
C
      IC = 0
      DO J=1,NLST
        CALL UPCASE(LDBFILES(J), LDBFILES(J))
        IF(IRT .GE. IRUNS(J) .AND. IRT .LE. IRUNE(J)) THEN
          IC = IC + 1
          JDBFILES(IC) = LDBFILES(J)
          SRUN(IC) = IRUNS(J)
          ERUN(IC) = IRUNE(J)
          FIV(IC) = IVN(J)
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
        DBFILE = JDBFILES(IFOUND)
        IRET = 0
      ENDIF
C
C----------------------------------------------------------------------
C
   99 CONTINUE
      CLOSE(UNIT=LUN)
      CALL RLUNIT(170,LUN,IER)
  999 CONTINUE
      RETURN
      END
