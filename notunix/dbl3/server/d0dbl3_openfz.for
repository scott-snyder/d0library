C----------------------------------------------------------------------
      LOGICAL FUNCTION D0DBL3_OPENFZ (FILNAM,ZDIV,CHOP,FILID,IRET)
C----------------------------------------------------------------------
C-   Purpose and Methods : Will open and initialize a FZ file. Default
C-    is exchange format, binary mode.
C-    
C-       *** Maximum of 8 FZ files can be open at same time ***
C-
C-   Return   .true.  it went ok
C-            .false. something went wrong
C-            
C-   Inputs  : FILNAM  (C)   Name of FZ file
C-             ZDIV    (I)   Zebra division used
C-             CHOP    (C)   ' '  Input only, exchange format, binary mode
C-                           'X'  Input only, exchange format, binary mode
C-                           'I'  Input only
C-                           'O'  Output only
C-                           'A'  Exchange format, alpha  mode
C-                           'N'  Native mode 
C-   Output  : FILID   (I)   ID of opened FZ files
C-             IRET    (I)   = iquest(1)
C-                             if -10 No more space for more files
C-                             if -11 D0OPEN failed
C-
C-   Updated  24-NOV-1992   Lars Rasmussen, change default fz file to 
C-                          exchange format, binary mode. Make it possible
C-                          to have mutiple open FZ files
C-   Updated  15-JAN-1993   Shahriar Abachi  IRET corrected & extended
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
C
      CHARACTER*(*) FILNAM, CHOP
      INTEGER ZDIV,FILID,IRET
C
      INTEGER I,J,K,LUNI,IOSTAT,IR,IOO,ILU,ILEN
      LOGICAL LOK
      CHARACTER*12 CHLP
      CHARACTER*4 CHPF(6), CHPO(6), CDUM
      LOGICAL FIRST
C
      INTEGER DBFZ_MXFZ
      PARAMETER (DBFZ_MXFZ=8)
      INTEGER DBFZ_NFLG
      PARAMETER (DBFZ_NFLG=8)
      INTEGER DBFZ_NFZ, DBFZ_LUN(DBFZ_MXFZ), DBFZ_ZDV(DBFZ_MXFZ)
      INTEGER DBFZ_FLG(DBFZ_NFLG,DBFZ_MXFZ)
      COMMON /D0DBL3FZ/ DBFZ_NFZ, DBFZ_LUN, DBFZ_ZDV, DBFZ_FLG
C
C- Currently valid combinations, for FZFILE and D0OPEN
C
      DATA CHPF /'IX   ','IA  ','I  ','OX  ','OA  ','O   '/
      DATA CHPO /'IG   ','IFL ','IU ','OG  ','OFL ','OU  '/
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      FILID= 0
      IRET = 0
      D0DBL3_OPENFZ = .FALSE.
      IF (FIRST) THEN
         FIRST = .FALSE.
         DBFZ_NFZ = 0
         DO I = 1,DBFZ_MXFZ
            DBFZ_LUN(I) = 0
         END DO
      END IF
C
C- Any free Units
C
      ILU = 0
      DO I = 1,DBFZ_MXFZ
         IF (DBFZ_LUN(I) .EQ. 0) THEN
            ILU=I
            GOTO 10
         END IF
      END DO
10    IF (ILU .EQ. 0) THEN
         CALL ERRMSG ('No more space for more files',
     &     'D0DBL3_OPENFZ',' ','E')
         IRET = -10
         RETURN
      END IF
C
      CHLP = CHOP
      CALL UPCASE (CHLP,CHLP)
      DO I = 1,DBFZ_NFLG
        DBFZ_FLG(I,ILU) = 0
      END DO
      IOO = 1
      IF (INDEX (CHLP,'O') .GT. 0) THEN
         DBFZ_FLG(1,ILU) = 1
         IOO = IOO + 3
      END IF
      IF (INDEX (CHLP,'A') .GT. 0) THEN
         DBFZ_FLG(2,ILU) = 1
         IOO = IOO + 1
      ELSE IF (INDEX(CHLP,'N') .GT. 0) THEN
         DBFZ_FLG(2,ILU) = 2
         IOO = IOO + 2
      END IF
C
C- Open FZ file
C
      CALL GTUNIT (984,LUNI,IR)
      CALL D0OPEN (LUNI,FILNAM,CHPO(IOO),LOK)
      IF (.NOT. LOK) THEN
         CALL ERRMSG 
     &     ('Error in opening FZ file','D0DBL3_OPENFZ',' ','E')
         IRET = -11
         RETURN
      END IF
      CALL XZRECL(ILEN,CDUM)
      CALL FZFILE (LUNI,ILEN,CHPF(IOO))
      IRET = IQUEST(1)
      IF (IRET .EQ. 0) THEN
         FILID = ILU
         D0DBL3_OPENFZ = .TRUE.
         DBFZ_LUN(ILU) = LUNI
         DBFZ_ZDV(ILU) = ZDIV
         DBFZ_NFZ = DBFZ_NFZ + 1
      END IF
C
      RETURN
      END
