      SUBROUTINE D0DAD_SYSOPEN(FNAME,INUNIT,IERR)
C-------------------------------------------------------------------------
C  Open a d0dad file from within a D0 framework.  This routine requires
C  no commons for storing the logical units for the file catalog and
C  Zebra file.
C
C  Author:  John D. Hobbs
C  Date:     16-DEC-1993
C
C  INPUTS: 
C     FNAME  - C - Name of input d0dad file.
C     LUN    - I - Fortran logical unit for file.
C  OUTPUTS: 
C     IERR   - I - 0 ==> All is OK.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FNAME
      INTEGER INUNIT,IERR
C
      INTEGER ICNTXT,IFTYPE,IDADFC,ILEN,I,IKEY
      CHARACTER FCDIR*(*),FCNAME*132,FCGENE*132
      PARAMETER(FCDIR='D0$D0DAD$CATALOGS:')
      LOGICAL  LIB$FIND_FILE
      INTEGER  LENOCC
      EXTERNAL LIB$FIND_FILE,LENOCC
      DATA IDADFC/0/
C-------------------------------------------------------------------------
C
C  Initially assume the blissful result that everything works as expected...
C
      LISDAD=.FALSE.
      IERR=0
C
C  Open the d0dad file.
C
      CALL D0DAD_OPEN(JFDF,FNAME,'R',INUNIT,IERR)
      IF( IERR.NE.0 ) THEN
         IERR = -1
         GOTO 999
      ENDIF
C
C  Parse tag string into file catalog name and test to see if it exists
C  and is a file catalog.
C
      DO I=1,LEN(CDFTAG)
         IF( CDFTAG(I:I).LT.CHAR(20) ) CDFTAG(I:I)=' '
      ENDDO
      ILEN=LENOCC(CDFTAG)
      FCNAME = CDFTAG(1:ILEN)//'.filecat'
      FCGENE = FCDIR//FCNAME
      IF( .NOT.LIB$FIND_FILE(FCGENE,FCNAME,ICNTXT) ) THEN
         IERR = -2
         GOTO 999
      ENDIF
C
      CALL D0DAD_FTYPE(FCNAME,IFTYPE)
      IF( IFTYPE.NE.JFFC ) THEN
         IERR = -3
         GOTO 999
      ENDIF
C
C  Open file catalog and set in D0DAD_DFREAD.
C
      IF( IDADFC.NE.0 ) CALL D0DAD_CLOSE(IDADFC,IERR)
      CALL D0DAD_OPEN(JFFC,FCNAME,'R',IDADFC,IERR)
      IF( IERR.NE.0 ) THEN
         IERR = -5
         GOTO 999
      ENDIF
C
C  Completed OPEN(s).  Mark it.
C
      LISDAD=.TRUE.
      CALL D0DAD_DFREAD_SETX(IDADFC)
      CALL EVTIN_D0DAD(LISDAD)
C
 999  CONTINUE
      CALL LIB$FIND_FILE_END(ICNTXT)
      END
