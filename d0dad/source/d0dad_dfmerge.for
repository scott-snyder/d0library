      SUBROUTINE D0DAD_DFMERGE(FILE_NAMES,DFOUT,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Merge a set of D0DAD files into a common 
C-     output file...
C-
C-     CAVEATS:  The individual D0DADF files must be in increasing run/
C-      event order for the merge to work
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-Mar-1995   John D. Hobbs
C-   Updated  15-May-1995   JDH - Change to D0OPEN
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) FILE_NAMES,DFOUT,HERE
      INTEGER IERR
C
      INTEGER NDFIN_MAX
      PARAMETER(NDFIN_MAX=12,HERE='D0DAD_DFMERGE')
      INTEGER NDFIN,INLUN(NDFIN_MAX),RUN(NDFIN_MAX),EVENT(NDFIN_MAX)
      INTEGER FID(NDFIN_MAX),ZRN(NDFIN_MAX),ZBO(NDFIN_MAX)
      INTEGER DATTIM(NDFIN_MAX)
      LOGICAL ISDONE(NDFIN_MAX)
      INTEGER I,LUNOUT,CURRENT,OLDRUN,NEVTS,ITERATION,NEVTS_TOT
C
      INTEGER LENOCC,DFMERGE_NEXTEVT
C-----------------------------------------------------------------------
C
C  Initialize, including protected links for input catalogs run sections...
C
      IERR=0
      NDFIN=0
      NEVTS_TOT=0
      CALL VZERO(ISDONE,NDFIN_MAX)
C
C  Open the input d0dad files list...
C
      CALL D0OPEN(IDADOK,FILE_NAMES(1:LENOCC(FILE_NAMES)),'I',IERR)
      IF(LDDBG.GT.3) WRITE(*,1000) 
 1000 FORMAT(/,'MERGE, Input D0DAD files: ')
 10   CONTINUE
        IF( NDFIN.GT.NDFIN_MAX ) THEN
          WRITE(D0DAD_ERRTXT,9001) NDFIN_MAX
 9001     FORMAT('Too many input d0dad files to merge. Limit=',I6)
          IF(LDDBG.GT.0) CALL ERRMSG('InputOver',HERE,D0DAD_ERRTXT,'E')
          IERR = -1
          GOTO 999
        ENDIF
        READ(IDADOK,'(A)',END=20,ERR=9902) FNTMP
        NDFIN=NDFIN+1
        CALL D0DAD_OPEN(JFDF,FNTMP,'R',INLUN(NDFIN),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9003) IERR,FNTMP(1:LENOCC(FNTMP))
 9003     FORMAT('Error ',I3,' opening input file: ',A)
          IF(LDDBG.GT.0)CALL ERRMSG('OpenError',HERE,D0DAD_ERRTXT,'E')
          IERR = -3
          GOTO 999
        ELSE
          IF(LDDBG.GT.3) WRITE(*,1001) NDFIN,FNTMP(1:LENOCC(FNTMP))
 1001     FORMAT('  D0DAD file ',I2,': ',A)
        ENDIF
      GOTO 10
 20   CONTINUE
      CLOSE(IDADOK)
      IF(LDDBG.GT.3) WRITE(*,*) ' ' 
C
C  Open the output file...
C
      CALL D0DAD_OPEN(JFDF,DFOUT,'W',LUNOUT,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,9004) IERR,DFOUT(1:LENOCC(DFOUT))
 9004   FORMAT('Error ',I3,' opening output file: ',A)
        IF(LDDBG.GT.0)CALL ERRMSG('OpenError',HERE,D0DAD_ERRTXT,'E')
        IERR= - 4
        GOTO 999
      ENDIF
C
C  Loop over all run/events until exhausted...
C
      OLDRUN=0
      ITERATION=1
      CURRENT=DFMERGE_NEXTEVT(NDFIN,INLUN,RUN,EVENT,FID,ZRN,ZBO,DATTIM,
     >   ISDONE)
      DO WHILE( CURRENT.GT.0 )
        IF(RUN(CURRENT).NE.OLDRUN .AND.OLDRUN.GT.0) THEN
          ITERATION=ITERATION+1
          IF(LDDBG.GT.4)WRITE(*,1002) ITERATION,CURRENT,OLDRUN,NEVTS
 1002     FORMAT(I9,': File ',I2,', Run ',I7,', #Events: ',I8)
          NEVTS=0
        ENDIF
        NEVTS_TOT=NEVTS_TOT+1
        NEVTS=NEVTS+1
        OLDRUN=RUN(CURRENT)
        I=CURRENT
        CALL DFPUT(LUNOUT,RUN(I),EVENT(I),FID(I),ZRN(I),ZBO(I),
     >     DATTIM(I),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9005)IERR,ITERATION,RUN(I),EVENT(I),CURRENT
 9005     FORMAT('Error ',I3,' from DFPUT on event',I6,' R/E=',2I8,
     >      ' Input file number=',I5)
          IF(LDDBG.GT.0) CALL ERRMSG('WriteError',HERE,D0DAD_ERRTXT,'E')
          IERR = -5
          GOTO 999
        ENDIF
        CURRENT=DFMERGE_NEXTEVT(NDFIN,INLUN,RUN,EVENT,FID,ZRN,ZBO,
     >     DATTIM,ISDONE)
      ENDDO
C
      IF(LDDBG.GT.4)WRITE(*,1002) ITERATION,CURRENT,OLDRUN,NEVTS
      IF(LDDBG.GT.3)WRITE(*,1003) ITERATION,NEVTS_TOT
 1003 FORMAT('  Merge complete.  ',I6,' runs with ',I9,' events.')
      IF(CURRENT.LT.0 ) THEN
        WRITE(D0DAD_ERRTXT,9006) CURRENT
 9006   FORMAT('Error ',I3,' returned from DFMERGE_NEXTEVT')
        IF(LDDBG.GT.0) CALL ERRMSG('LoopError',HERE,D0DAD_ERRTXT,'E')
        IERR = -6
        GOTO 999
      ENDIF
C
C
C  Close the input files...
C
      DO I=1,NDFIN
        CALL D0DAD_CLOSE(INLUN(I),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9007) IERR,I,INLUN(I)
 9007     FORMAT('Error',I3,' closing input d0dadf',I3,' on unit',I3)
          IF(LDDBG.GT.0) CALL ERRMSG('CloseError',HERE,D0DAD_ERRTXT,'E')
          IERR = -7
          GOTO 999
        ENDIF
      ENDDO

C
C  Close the output file...
C
      CALL D0DAD_CLOSE(LUNOUT,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,9008) IERR,LUNOUT
 9008   FORMAT('Error ',I3,' closing output d0dad file on unit',I3)
        IF(LDDBG.GT.0) CALL ERRMSG('CloseError',HERE,D0DAD_ERRTXT,'E')
        IERR = -8
        GOTO 999
      ENDIF
C
  999 CONTINUE
      RETURN
C
 9902 CONTINUE
      IERR = -2
      GOTO 999
      END
