      SUBROUTINE D0DAD_FTYPE(FNAME,IFTYPE)
C-------------------------------------------------------------------------
C  Determine the d0dad type of an input file.  If an error occurs, return
C  IFTYPE<0.
C
C  Author:  John D. Hobbs
C  Date:     1-NOV-1993
C  Updated  19-JAN-1995 sss - Make sure file gets closed after EOF errors.
C           15-MAY-1995 JDH - File name length in OPEN statements
C            1-Jan-1995 sss - compile with g77
C
C  INPUTS: 
C     FNAME  - C - Name (set) of input file.
C  OUTPUTS: 
C     IFTYPE - I - >=0, D0dad file type (see d0dadcom.inc)
C                  <0,  Error code.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
C
      CHARACTER*(*) FNAME
      INTEGER IFTYPE,IREC(2),I,IUTMP,IERR,IDADFT,IKEY,L
      CHARACTER*8 CFTSTR
      INTEGER LENOCC
      EXTERNAL LENOCC
      LOGICAL INITDONE,LEXIST
      SAVE INITDONE,IDADFT
      DATA INITDONE/.FALSE./
C ---------------------------------------------------------------------
C
      IF( .NOT.INITDONE ) THEN
        CALL D0DAD_ZBINIT
        CALL D0DAD_GTUNIT(IDADFT,IKEY,IERR)
        INITDONE=.TRUE.
      ENDIF
C
      L=LENOCC(FNAME)
      IF( LDDBG.GT.10 ) THEN
        WRITE(D0DAD_ERRTXT,9901) FNAME(1:MIN(L,80))
 9901   FORMAT(' D0DAD_FTYPE: Input: ',A)
        CALL ERRMSG('D0DadFileName','D0DAD_FTYPE',D0DAD_ERRTXT,'I')
      ENDIF
C
      INQUIRE(FILE=FNAME(1:L),EXIST=LEXIST)
      IF( .NOT.LEXIST ) THEN
        IFTYPE = -1
        GOTO 999
      ENDIF
C
C  Try it as an unformatted, sequential file...
C
      OPEN(IDADFT,FILE=FNAME(1:L),FORM='UNFORMATTED',STATUS='OLD',
     +  ACCESS='SEQUENTIAL',ERR=101
C&IF IBMAIX,LINUX
C&     + )
C&ELSE
     + ,READONLY)
C&ENDIF
      READ(IDADFT,ERR=102,END=302) IREC(1),IREC(2)
      CALL UHTOC(IREC,4,CFTSTR,8)
      IF(LDDBG.GT.10) THEN
        WRITE(D0DAD_ERRTXT,9003) CFTSTR
 9003   FORMAT('Type string(1) = ',A8)
        CALL ERRMSG('D0DadFileType(1)','D0DAD_FTYPE',D0DAD_ERRTXT,'I')
      ENDIF
      IF( CFTSTR(1:5).EQ.'D0DAD' ) GOTO 10
 102  CONTINUE
      CLOSE(IDADFT)
C
C  If necesssary try it as an unformatted, direct access file...
C
 101  CONTINUE
      OPEN(IDADFT,FILE=FNAME(1:L),FORM='UNFORMATTED',STATUS='OLD',
     +  ACCESS='DIRECT',RECL=2,ERR=201
C&IF IBMAIX,LINUX
C&     + )
C&ELSE
     + ,READONLY)
C&ENDIF
      READ(IDADFT,ERR=202,END=302) IREC(1),IREC(2)
      CALL UHTOC(IREC,4,CFTSTR,8)
      IF(LDDBG.GT.10) THEN
        WRITE(D0DAD_ERRTXT,9003) CFTSTR
        CALL ERRMSG('D0DadFileType(2)','D0DAD_FTYPE',D0DAD_ERRTXT,'I')
      ENDIF
      IF( CFTSTR(1:5).EQ.'D0DAD' ) GOTO 10
  202 CONTINUE
      CLOSE(IDADFT)
C
C  Finally try it as a straight text file...
C
  201 CONTINUE
      OPEN(IDADFT,FILE=FNAME(1:L),STATUS='OLD',ERR=301
C&IF IBMAIX,LINUX
C&     + )
C&ELSE
     + ,READONLY)
C&ENDIF
      READ(IDADFT,'(A)',ERR=302,END=302) CFTSTR
      CLOSE(IDADFT)
      IF(LDDBG.GT.10) THEN
        WRITE(D0DAD_ERRTXT,9003) CFTSTR
        CALL ERRMSG('D0DadFileType(2)','D0DAD_FTYPE',D0DAD_ERRTXT,'I')
      ENDIF
      IF( CFTSTR(1:5).NE.'D0DAD' ) THEN
        IFTYPE = -2
        GOTO 999
      ENDIF
C
C  It's a d0dad system file.  Which type?
C
 10   CONTINUE
      CLOSE(IDADFT)
      IFTYPE = -3
      DO I=1,NFTYPE
        IF( CFTSTR(7:8).EQ.CFTYPE(I) ) IFTYPE=I
      ENDDO
      GOTO 999
C
 999  CONTINUE
      IF( LDDBG.GT.10 .AND. IFTYPE.GE.0 ) THEN
        WRITE(D0DAD_ERRTXT,9902) CFTSTR,IFTYPE
 9902   FORMAT('Header String: ',A8,' Type: ',I3)
        CALL ERRMSG('D0DadFileType','D0DAD_FTYPE',D0DAD_ERRTXT,'I')
      ENDIF
      RETURN
C     
 301  CONTINUE
 302  CONTINUE
      CLOSE(IDADFT)
      IFTYPE = -4
      RETURN
C     
      END
