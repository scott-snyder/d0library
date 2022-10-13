      INTEGER FUNCTION MUNMOD3(ICODE,MODU)
C======================================================================
C
C   Description:  This function translates a module index into a
C   ============  D0 module number (if ICODE=1) or translates a
C                 D0 module number into a module index (if ICODE=2)
C
C                 If ICODE<0   Initialize list MODU, with -ICODE modules
C                              MUNMOD = 0 if OK
C                 If ICODE=0   MUNMOD = number of modules in system (MNUM)
C                 If ICODE=1   MODU= 1,2,3...  MUNMOD = 10,11,12...
C                              if out of range, MUNMOD = -1
C                 If ICODE=2   MODU= 10,11,12... MUNMOD = 1,2,3...
C                              if not on list, MUNMOD = -1
C                 If ICODE=3   Add module MODU to the list,
C                              MUNMOD = updated number of modules
C
C  can use LMUMOD from RCP to define what modules. LMUMOD=0 all
C         LMUMOD=1 central  LMUMOD=2  north LMUMOD=3 south
C
C  Revision History
C  =================
C   3/92 from munmod
C- Cecilia Gerber: separate modules in 3 geographic sectors.
C  DH 5/92 add lmumod=4 option for completeness
C  DH 7/92 forgot to do LISTM
C  PQ 10/93 add lmumod=5,6 to divide up central chambers
C           add lmumod=7 to do all end chambers at once
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER ICODE,MLIST0(164),MLIST1(94),MLIST2(35),MLIST3(35),
     &  MLIST5(46), MLIST6(48), MLIST7(70), MLIST(500), 
     &  MNUM0, MNUM1, MNUM2, MNUM3, MNUM5, MNUM6, MNUM7, MNUM
      INTEGER LISTM(307)
      INTEGER MODU(*)
      INTEGER I, LMUNMOD, IER
      LOGICAL FIRST
      DATA    FIRST   /.TRUE./
        DATA MLIST0/10,11,12,13, 15,16, 20,21,22,23, 25,26,
     &           30,31,32,33, 35,36, 61,62, 64,65, 67, 91,92, 94,95, 97,
     &           100,101,102,103,104,105,106,107,
     &           110,111,112,113,114,115,116,117,
     &           120,121,122,123,124, 127,
     &           130,131,132,133,134,135,136,137,
     &           140,141,142,143,144,145,146,147, 150, 153,
     &           160,161,162,163,164,165,166,167, 180, 183,
     &           190,191,192,193,194,195,196,197,
     &           200,201,202,203,204,205,206,207,
     &           210,211,212,213,214,215,216,217,
     &           220,221,222,223,224, 227,
     &           230,231,232,233,234,235,236,237,
     &           240,241,242,243,244,245,246,247, 250,251, 253, 255,
     &           260,261,262,263,264,265,266,267,
     &           270,271,272,273,274,275,276,277, 280,281, 283, 285,
     &           290,291,292,293,294,295,296,297,
     &           300,301,302,303,304,305,306,307/
        DATA MLIST1/10,11,12,13, 15,16, 20,21,22,23, 25,26,
     &           30,31,32,33, 35,36,
     &           100,101,102,103,104,105,106,107,
     &           110,111,112,113,114,115,116,117,
     &           120,121,122,123,124, 127,
     &           130,131,132,133,134,135,136,137,
     &           140,141,142,143,144,145,146,147,
     &           200,201,202,203,204,205,206,207,
     &           210,211,212,213,214,215,216,217,
     &           220,221,222,223,224, 227,
     &           230,231,232,233,234,235,236,237,
     &           240,241,242,243,244,245,246,247/
        DATA MLIST2/61,62, 64,65, 67, 150, 153,
     &           160,161,162,163,164,165,166,167,
     &           250, 253, 251, 255,
     &           270,271,272,273,274,275,276,277,
     &           260,261,262,263,264,265,266,267/
        DATA MLIST3/91,92, 94,95, 97, 180, 183,
     &           190,191,192,193,194,195,196,197,
     &           280, 283, 281, 285,
     &           300,301,302,303,304,305,306,307,
     &           290,291,292,293,294,295,296,297/
        DATA MLIST5/10,11,12,13,15,16, 
     &              20,21,22,23,25,26, 
     &              30,31,32,33,35,36,
     &           111,112,115,116,
     &           120,121,122,123,124,127,
     &           131,132,135,136,
     &           211,212,215,216,
     &           220,221,222,223,224,227,
     &           231,232,235,236/
        DATA MLIST6/
     &           100,101,102,103,
     &           104,105,106,107,
     &           110,113,114,117,
     &           130,133,134,137,
     &           140,141,142,143,
     &           144,145,146,147,
     &           200,201,202,203,
     &           204,205,206,207,
     &           210,213,214,217,
     &           230,233,234,237,
     &           240,241,242,243,
     &           244,245,246,247/
        DATA MLIST7/61,62, 64,65, 67, 
     &              91,92, 94,95, 97, 
     &           150, 153,
     &           160,161,162,163,164,165,166,167,
     &           180, 183,
     &           190,191,192,193,194,195,196,197,
     &           250, 253, 251, 255,
     &           260,261,262,263,264,265,266,267,
     &           270,271,272,273,274,275,276,277,
     &           280, 283, 281, 285,
     &           290,291,292,293,294,295,296,297,
     &           300,301,302,303,304,305,306,307/
        DATA MNUM7  /70/
        DATA MNUM6  /48/
        DATA MNUM5  /46/
        DATA MNUM3  /35/
        DATA MNUM2  /35/
        DATA MNUM1  /94/
        DATA LISTM / 307*-1 /    ! initialize to -1 for error checking
        DATA MNUM0  /164/
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL EZGET('LMUMOD',LMUNMOD, IER)
        IF (LMUNMOD.EQ.0) THEN   ! read all the modules
          MNUM = MNUM0
          DO I = 1, MNUM
            MLIST(I) = MLIST0(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
C
       IF (LMUNMOD.EQ.1) THEN   ! read only the central modules
          MNUM = MNUM1
          DO I = 1, MNUM
            MLIST(I) = MLIST1(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
C
        IF (LMUNMOD.EQ.2) THEN   ! read only the north modules
          MNUM = MNUM2
          DO I = 1, MNUM
            MLIST(I) = MLIST2(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
         ENDIF
C
        IF (LMUNMOD.EQ.3) THEN   ! read only the south modules
          MNUM = MNUM3
          DO I = 1, MNUM
            MLIST(I) = MLIST3(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
C
        IF(LMUNMOD.EQ.4) THEN           !Read list from MURECO.RCP
          CALL EZGSET('KMUMOD()',MNUM,1)
          CALL EZGSET('KMUMOD',MLIST(1),1)
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
      ENDIF
C
        IF (LMUNMOD.EQ.5) THEN   ! read only the top and bottom modules
          MNUM = MNUM5
          DO I = 1, MNUM
            MLIST(I) = MLIST5(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
         ENDIF
C
        IF (LMUNMOD.EQ.6) THEN   ! read only the east and west modules
          MNUM = MNUM6
          DO I = 1, MNUM
            MLIST(I) = MLIST6(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
C
        IF (LMUNMOD.EQ.7) THEN   ! read all the end modules
          MNUM = MNUM7
          DO I = 1, MNUM
            MLIST(I) = MLIST7(I)
          ENDDO
          DO I=1,MNUM
            LISTM(MLIST(I)) = I
          ENDDO
        ENDIF
C
C  Executable Code:
C  ==================
C
      MUNMOD3 = 0
      IF     (ICODE .LT. 0) THEN    ! Initialize List of Modules
        MNUM = -ICODE
        DO I=1,307                      ! re-initialize to -1
          LISTM(I) = -1
        ENDDO
        DO I=1,MNUM
          MLIST(I) = MODU(I)
        ENDDO
        DO I=1,MNUM
          LISTM(MLIST(I)) = I
        ENDDO
      ELSEIF (ICODE .EQ. 0) THEN    ! Number of Modules in System
        MUNMOD3 = MNUM
      ELSEIF (ICODE .EQ. 1) THEN    ! Module Number for MODU Order in List
        IF (MODU(1) .GT. MNUM) THEN
          MUNMOD3 = -1
        ELSE
          MUNMOD3 = MLIST(MODU(1))
        ENDIF
      ELSEIF (ICODE .EQ. 2) THEN    ! Order in List for Module MODU
        MUNMOD3 = LISTM(MODU(1))
      ELSEIF (ICODE .EQ. 3) THEN        ! add to list
        IF (LISTM(MODU(1)) .EQ. -1 ) THEN       ! not already in list
          MNUM = MNUM + 1
          MLIST(MNUM) = MODU(1)
          LISTM(MLIST(MNUM)) = MNUM
        ENDIF
        MUNMOD3 = MNUM
      ENDIF
  999 CONTINUE
      RETURN
      END
