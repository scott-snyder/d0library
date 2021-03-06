      INTEGER FUNCTION MUNMOD(ICODE,MODU)
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
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History
C  =================
C  Original Creation - November 4, 1988
C  dh 6/89   setup for test stand. Only two modules called 1,2
C  DH 2/90 TAKEN FROM EM...SETUP FOR 1 MODULE
C  J.Green 7/90 Setup for 164 modules + option for input list
C  J.Green 2/91 Add error return=-1 if not on list
C  DH 4/91 fix 35/36
C  DH 4/91 fix 145/146
C  J.Green 5/91 Add option ICODE=3
C-   Updated  16-MAY-1991 Silvia Repond
C-      Bug fix 
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER ICODE,MLIST(164),MNUM
      INTEGER LISTM(307)
      INTEGER MODU(*)
      INTEGER I
      LOGICAL FIRST
      DATA    FIRST   /.TRUE./
      DATA MLIST/10,11,12,13, 15,16, 20,21,22,23, 25,26,   
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
      DATA LISTM / 307*-1 /    ! initialize to -1 for error checking
      DATA MNUM  /164/
C
C  Executable Code:
C  ==================
C
      MUNMOD = 0
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        DO I=1,MNUM
          LISTM(MLIST(I)) = I
        ENDDO
      ENDIF
C
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
        MUNMOD = MNUM           
      ELSEIF (ICODE .EQ. 1) THEN    ! Module Number for MODU Order in List
        IF (MODU(1) .GT. MNUM) THEN
          MUNMOD = -1
        ELSE
          MUNMOD = MLIST(MODU(1))
        ENDIF
      ELSEIF (ICODE .EQ. 2) THEN    ! Order in List for Module MODU
        MUNMOD = LISTM(MODU(1))
      ELSEIF (ICODE .EQ. 3) THEN        ! add to list
        IF (LISTM(MODU(1)) .EQ. -1 ) THEN       ! not already in list
          MNUM = MNUM + 1
          MLIST(MNUM) = MODU(1)
          LISTM(MLIST(MNUM)) = MNUM
        ENDIF
        MUNMOD = MNUM
      ENDIF      
  999 CONTINUE
      RETURN
      END         
