      SUBROUTINE XCHKER(LUN,FILEN,XMODE,LREC,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks a zebra file to determine its format.
C-                         i.e. whether it is exchange or native mode.
C-
C-   Inputs  :
C-             LUN   =  Logical unit to open file with
C-             FILE  =  Name of file to check
C-   Outputs :
C-             XMODE =  File type.
C-                      0 = native
C-                      1 = exchange
C-                      2 = zzip
C-             LREC  =  Record length of exchange mode file.  Set
C-                      to 2044 for native mode.
C-             OK    =  Return status. TRUE=> everything alright.
C-   Controls:
C-
C-   Created  19-MAY-1992   Michael Diesburg
C-   Updated  22-MAY-1994   sss - recognize zzip files
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
 
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
 
C&IF VAXVMS
      INTEGER LUN,LREC,SBLOCK(8)
      REAL VAL
      INTEGER IVAL
      EQUIVALENCE (VAL,IVAL)
C&ELSE
C&      INTEGER LUN,LREC,SBLOCK(8190),NBLKS
C&ENDIF
      INTEGER BIT24
      PARAMETER (BIT24 = 2**24-1)
      INTEGER I,XCHIDS(4),IOFF,IOS,RECL
      LOGICAL OK,EXISTS,TAPE
      INTEGER XMODE
      BYTE B1,B2,B3,B4,B(32)
      CHARACTER*(*) FILEN
 
      EQUIVALENCE (SBLOCK,B)
 
      DATA XCHIDS / 19123695,-2140110736,1126280141,-2141093791 /
 
      OK = .TRUE.
      TAPE = .FALSE.
 
      INQUIRE(FILE=FILEN,EXIST=EXISTS,RECL=RECL,IOSTAT=IOS)
      IF(IOS.EQ.0) THEN
        IF(EXISTS.AND.(RECL.EQ.0)) TAPE = .TRUE.
      ENDIF
 
C&IF VAXVMS
      OPEN(UNIT=LUN,FILE=FILEN,FORM='UNFORMATTED',ERR=200,
     &     READONLY,STATUS='OLD')
      READ(LUN,IOSTAT=IOS) SBLOCK
      IVAL = SBLOCK(3)
C&ELSEIF SIUNIX
C&      OPEN(UNIT=LUN,FILE=FILEN,FORM='UNFORMATTED',ERR=200,
C&     &     ACCESS='DIRECT',RECL=8190,STATUS='OLD')
C&      READ(LUN,REC=1,IOSTAT=IOS) SBLOCK
C&ELSE
C&      OPEN(UNIT=LUN,FILE=FILEN,FORM='UNFORMATTED',ERR=200,
C&     &     ACCESS='DIRECT',RECL=32760,STATUS='OLD')
C&      READ(LUN,REC=1,IOSTAT=IOS) SBLOCK
C&ENDIF
 
      DO 20 I = 1,8
        IOFF = 4*(I-1)
        B1 = B(IOFF+BYTE1)
        B2 = B(IOFF+BYTE2)
        B3 = B(IOFF+BYTE3)
        B4 = B(IOFF+BYTE4)
        B(IOFF+1) = B4
        B(IOFF+2) = B3
        B(IOFF+3) = B2
        B(IOFF+4) = B1
20    CONTINUE
 
      XMODE = 1
      DO 40 I = 1,4
        IF(SBLOCK(I).NE.XCHIDS(I))  XMODE = 0
40    CONTINUE

      IF (XMODE .EQ. 0 .AND.
     &    SBLOCK(1) .EQ. 2054842736) THEN  ! `pizz'
        XMODE = 2
      ENDIF

      LREC = 2044
C&IF VAXVMS
      IF(XMODE .eq. 1)   LREC = IAND(SBLOCK(5), BIT24)
      IF(RECL.EQ.32764)  LREC = RECL/4
      IF (XMODE .EQ. 2) LREC = -1
C&ELSE
C&      IF(XMODE.EQ.1)  THEN
C&        LREC = IAND(SBLOCK(5), BIT24)
C&C        IF(LREC.EQ.900)  GO TO 998
C&C        NBLKS = SBLOCK(8)
C&C        DO 45 I = 1,NBLKS+1
C&C          READ(LUN,REC=I+1,IOSTAT=IOS) SBLOCK
C&C45      CONTINUE
C&C        DO 23 I = 1,8
C&C          IOFF = 4*(I-1)
C&C          B1 = B(IOFF+BYTE1)
C&C          B2 = B(IOFF+BYTE2)
C&C          B3 = B(IOFF+BYTE3)
C&C          B4 = B(IOFF+BYTE4)
C&C          B(IOFF+1) = B4
C&C          B(IOFF+2) = B3
C&C          B(IOFF+3) = B2
C&C          B(IOFF+4) = B1
C&C23      CONTINUE
C&C        IF(LREC.EQ.IAND(SBLOCK(5), BIT24)) GO TO 998
C&C        IF(LREC.NE.IAND(SBLOCK(5+NBLKS+1), BIT24))  GO TO 200
C&C        LREC = 8191
C&      ENDIF
C&ENDIF
C&IF VAXVMS
      IF(XMODE .EQ. 0 .AND.VAL.NE.12345.0)GOTO 200
C&ENDIF
 998  CLOSE(UNIT=LUN)
 999  RETURN
 
200   OK = .FALSE.
      CALL ERRMSG('MODE CHECK','XCHKER',
     &            'COULD NOT DETERMINE ZEBRA FILE TYPE','I')
      CLOSE(UNIT=LUN)
      RETURN
 
      END
