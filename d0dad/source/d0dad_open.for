      SUBROUTINE D0DAD_OPEN(IFTYPE,FNAME,FCOPT,LUN,IERR)
C-------------------------------------------------------------------------
C  Open a file for use by the d0dad system.  The file type (see 
C  d0dadcom.inc ) is defined by IFTYPE.  For the open of existing
C  files, headers are read.
C
C  Author:  John D. Hobbs
C  Date:     1-NOV-1993
C
C  INPUTS: 
C     IFTYPE - I - Type of file (see d0dadcom.inc)
C     FNAME  - C - Name (set) of input file.
C     FCOPT  - C - Option string for opening file.
C  OUTPUTS: 
C     LUN    - I - Logical unit for file.
C     IERR   - I - 0 ==> All is OK.
C
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
*
      INTEGER IFTYPE,LUN,IERR,IKEY
      CHARACTER*(*) FNAME,FCOPT
*
      LOGICAL LEXIST,LWRITE,INITDONE
      CHARACTER*8 CSTR,CSTAT,CONE*1
      INTEGER  LENOCC,I
      EXTERNAL LENOCC
      SAVE INITDONE
      DATA INITDONE/.FALSE./
C
C  Initialize D0DAD specific zebra.  The routine D0DAD_ZBINIT contains
C  protection for multiple calls.
C
      IF( .NOT.INITDONE ) CALL D0DAD_ZBINIT
      INITDONE=.TRUE.
*
*  Check file type
*
      IF( IFTYPE.LT.1 .OR. IFTYPE.GT.NFTYPE ) THEN
         IERR = -1
         WRITE(D0DAD_ERRTXT,1001) IFTYPE
 1001    FORMAT('Unknown file type',I3)
         IF(LDDBG.GT.0) CALL ERRMSG(' ','D0DAD_OPEN',D0DAD_ERRTXT,'W')
         GOTO 997
      ENDIF
*
*  Check for file
*
      INQUIRE(FILE=FNAME,EXIST=LEXIST)
*
*  Decode options (W=WRITE(NEW),A=APPEND(WRITE,OLD), R=READONLY(OLD).
*
      LWRITE=.FALSE.
      CSTAT='XXX'
      DO I=1,LEN(FCOPT)
         IF( FCOPT(I:I).EQ.'R') THEN
            LWRITE=.FALSE.
            CSTAT='OLD'
         ELSE IF( FCOPT(I:I).EQ.'W') THEN
            LWRITE=.TRUE.
            CSTAT='NEW'
         ELSE IF( FCOPT(I:I).EQ.'A') THEN
            LWRITE=.TRUE.
            IF( LEXIST ) THEN
               CSTAT='OLD'
            ELSE
               CSTAT='NEW'
            ENDIF
         ELSE IF( FCOPT(I:I).NE.' ') THEN
            CONE=FCOPT(I:I)
            WRITE(D0DAD_ERRTXT,9001) CONE
 9001       FORMAT('Unknown option: ',A1)
            IF(LDDBG.GT.1)CALL ERRMSG(' ','D0DAD_OPEN',D0DAD_ERRTXT,'W')
         ENDIF
      ENDDO
*
*  Check/enforce logic of options.
*
      IF( CSTAT.EQ.'XXX' ) THEN
         IERR = -2
         WRITE(D0DAD_ERRTXT,1002) 
 1002    FORMAT('Unknown file status: XXX')
         IF(LDDBG.GT.0) CALL ERRMSG(' ','D0DAD_OPEN',D0DAD_ERRTXT,'W')
         GOTO 997
      ENDIF
      IF( .NOT.LWRITE .AND. .NOT.LEXIST ) THEN
         WRITE(D0DAD_ERRTXT,1003) FNAME(1:MIN(LENOCC(FNAME),80))
 1003    FORMAT('Readonly file',A,' non-existant')
         IF(LDDBG.GT.0) CALL ERRMSG(' ','D0DAD_OPEN',D0DAD_ERRTXT,'W')
         IERR = -3
         GOTO 997
      ENDIF
C
C  Allocate a logical unit...
C
      CALL D0DAD_GTUNIT(LUN,IKEY,IERR)
      IF( IERR.NE.0 ) GOTO 997
*
*  Open and header-write based on file type.
*
      IF( IFTYPE.EQ.JFUE ) THEN
*
         CALL UEOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
         IF( IERR.NE.0 ) GOTO 998

      ELSEIF( IFTYPE.EQ.JFEC ) THEN
*
         CALL ECOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
         IF( IERR.EQ.(-7) ) GOTO 995
         IF( IERR.EQ.(-6) ) GOTO 996
         IF( IERR.NE.0 ) GOTO 998
*
      ELSEIF( IFTYPE.EQ.JFDF ) THEN
*
         CALL DFOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
         IF( IERR.NE.0 ) GOTO 998
*
      ELSEIF( IFTYPE.EQ.JFFC ) THEN
*
         CALL FCOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
         IF( IERR.NE.0 ) GOTO 998
*
      ENDIF
C
 999  CONTINUE
      IERR=0
      RETURN
C
 995  CONTINUE
      WRITE(D0DAD_ERRTXT,1004) IERR,CFTYPE(IFTYPE),FNAME
      IERR = -7
      RETURN
C
 996  CONTINUE
      WRITE(D0DAD_ERRTXT,1004) IERR,CFTYPE(IFTYPE),FNAME
      IERR = -6
      RETURN
C
 997  CONTINUE
      IERR = -4
      RETURN
C
 998  CONTINUE
      WRITE(D0DAD_ERRTXT,1004) IERR,CFTYPE(IFTYPE),FNAME
 1004 FORMAT('Error ',I3,' from',A2,'OPEN. File: ',A40)
      IERR = -5
      RETURN
      END
