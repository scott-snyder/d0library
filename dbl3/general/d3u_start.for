C----------------------------------------------------------------------
      SUBROUTINE D3U_START (DBFILE,CHOPT,IRET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To start/open DBL3
C-
C-   Inputs  :   DBFILE (C*)    DBL3 file name    
C-               CHOPT  (C*)   
C-          ' '  Read only. Changes by other processes not visible.
C-          'S'  Read only. Changes by other processes visible to the user
C-          'U'  for Exclusive Update privileges.
C-          'SU' for Shared Update pivileges.
C-          'Z'  Will create a new data base, D3U_CREATE has to be called
C-               after.
C-
C-   Outputs : IRET          0=OK
C-   Controls: 
C-
C-   Created  26-NOV-1990   Lars O. Rasmussen
C-   Modified 28-SEP-1992   Lor, To do the UNIX open thing.
C-   Modified 10-OCT-1992   Lor, To be consistent with multiple open files.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D3U.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER ICFIND,ICLOC,D3UNIT,D3UTOP
C
      CHARACTER*(*)  DBFILE
      CHARACTER*(*)  CHOPT
      INTEGER        IRET
C
      INTEGER        I,J,K,L
      INTEGER        IOS,C1,C2,IRECL,D3LT
      CHARACTER*3    CHOP,CHP
      CHARACTER*4    CPT
      CHARACTER*8    TOPD
      CHARACTER*40   CHLP
      LOGICAL OPTZ,OPTS,OPTU,OPTO,OPTP,OPTA,OPTX,OPTD,OPTJ,OPTB
      LOGICAL OK
      LOGICAL        FIRST 
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      D3_END = .TRUE.              ! Must call DBEND and set true
      CHP = CHOPT
      CALL UPCASE(CHP,CHP)
C
C- Get a free unit number
C
      IF (D3UTOP(D3_PATH,TOPD) .LE. 0) THEN
         CALL MSGO('e','D3U_START','Top directory not defined',0)
         IRET = -1
         GOTO 999
      END IF
      D3_UNIT = D3UNIT('N',TOPD)
      IF (D3_UNIT .LE. 0) THEN
         CALL MSGO('en','D3U_START',
     &     'Failer in gettting unit number, return=,',D3_UNIT)
         IRET = -1
         GOTO 999
      END IF
C
C- Just for fun, lets book a BIG bank and then drop it
C
      IF (FIRST) THEN
         OPTJ = .FALSE.
         OPTB = .FALSE.
         L = 0
         CALL MZBOOK(D3_DIV,L,L,2,'$RZ$',0,0,25000,2,-1)
         CALL MZDROP(D3_DIV,L,' ')
         IF (IQUEST(1) .NE. 0) THEN
            CALL MSGO('en','D3U_START','MZBOOK error, quest=',IQUEST(1))
            IRET = -1
            GO TO 999
         END IF
         CALL MZLINK(D3_DIV,'/D3TD/',D3_LT,D3_LK(1),D3_LD(255))
         CALL MZLINK (D3_DIV,'/LKD3/',
     &     D3_LNK(1),D3_LNK(D3_MXL),D3_LNK(1))
         FIRST = .FALSE.
      ENDIF
C
C- Open in Herb style
C
      OPTZ = .FALSE.
      OPTS = .FALSE.
      OPTU = .FALSE.
      OPTO = .FALSE.
      OPTP = .FALSE.
      OPTD = .FALSE.
      OPTA = .FALSE.
      OPTX = .FALSE.
      CHOP(1:3) = '   '
      J = 0      
      L = LEN(CHP)
      DO I=1,MIN(5,L)
        IF(CHP(I:I) .EQ. 'Z') OPTZ = .TRUE.
        IF(CHP(I:I) .EQ. 'S') OPTS = .TRUE.
        IF(CHP(I:I) .EQ. 'U') OPTU = .TRUE.
        IF(CHP(I:I) .EQ. 'O') OPTO = .TRUE.
        IF(CHP(I:I) .EQ. 'P') OPTP = .TRUE.
        IF(CHP(I:I) .EQ. 'J') OPTJ = .TRUE.
        IF(CHP(I:I) .EQ. 'X') OPTX = .TRUE.
        IF(CHP(I:I) .EQ. 'A') OPTA = .TRUE.
        IF(CHP(I:I) .EQ. 'D') OPTD = .TRUE.
        IF(CHP(I:I) .EQ. 'Z' .OR. CHP(I:I) .EQ. 'S' .OR.
     &     CHP(I:I) .EQ. 'U' .OR. CHP(I:I) .EQ. 'O' .OR.
     &     CHP(I:I) .EQ. 'P') THEN
          J = J + 1
          CHOP(J:J) = CHP(I:I)
        ENDIF
      ENDDO
C
      IOS = 0
      IF (OPTO) THEN
        CPT = 'IOSU'
        IRECL = 4096
      ELSEIF (OPTP) THEN
        CPT = 'ISU'
        IRECL = 4096
      ELSEIF (OPTZ) THEN
        CPT = 'OSU'
        IRECL = 4096
      ELSEIF (OPTU) THEN
        CPT = 'IOSU'
        IRECL = 4096
      ELSE
        CPT = 'ISU'
        IRECL = 4096
      ENDIF
C
C- Try atleast three times
C
      DO I = 1,3
        CALL D0RZOPEN(D3_UNIT,DBFILE,CPT,IRECL,OK)
        IF(.NOT. OK) THEN
          IOS = -30
        ELSE
          GOTO 11
        END IF
      END DO        
C                                                                             
11    IF (IOS .NE. 0) THEN
         CHLP = DBFILE
         CALL MSGO('e','D3U_START','Couldn''t open '//CHLP,0)
         IRET = -1
         GOTO 999
      END IF
C
      IF (OPTZ) THEN
          CALL DBINIT(D3_DIV,D3_UNIT,TOPD,D3LT,50000,CHOP)
      ELSE
          CALL DBINIT(D3_DIV,D3_UNIT,TOPD,D3LT,0,CHOP)
      ENDIF
      IF (IQUEST(1).NE.0) THEN
         CALL MSGO('en','D3U_START','DBINIT error, quest=',IQUEST(1))
         IRET = -1
         CALL D3U_END
         GO TO 999
      ENDIF
C
      CALL RZLDIR('//',' ')
      CALL DBLOGL(D3_UNIT,0)
      IRET = 0
C
999   RETURN
      END
