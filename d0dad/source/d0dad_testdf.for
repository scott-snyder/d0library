      SUBROUTINE D0DAD_TESTDF(FINDF,FINFC,IERR)
C-------------------------------------------------------------------------
C  Read ZEBRA files using d0dad files.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER  IERR
      CHARACTER*(*) FINDF,FINFC
      INTEGER  LENOCC
      EXTERNAL LENOCC
C-------------------------------------------------------------------------
C
C  Copy input file names to common storage.
C
      FDFNAM=FINDF
      FFCNAM=FINFC
C
C  Diagnostic output....
C
      IF( LDDBG.GT.3 ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' ********* In TESTDF **********'
         WRITE(*,*) ' D0DadFile: ',FDFNAM(1:LENOCC(FDFNAM))
         WRITE(*,*) ' FileCatalog: ',FFCNAM(1:LENOCC(FFCNAM))
         WRITE(*,*) ' Flag: ',ITEST
         WRITE(*,*) ' '
      ENDIF
C
C& IF VAXVMS
C& ELSE
C&      CALL FLUSH_STD
C& ENDIF
      IF( ITEST.EQ.1 ) THEN
         CALL D0DAD_TESTDF_DOALL(IERR)
      ELSE
         CALL D0DAD_TESTDF_FRAMES(IERR)
      ENDIF
C
      RETURN
      END
