      SUBROUTINE D0DAD_ECMERGE(FILE_NAMES,ECOUT,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Merge event catalogs.  
C-    
C-     CAVEATS: The output catalog cannot exist.  The event catalogs 
C-       must share a common file catalog.  This routine will be
C-       obsoleted once catalogs no longer require ordering of run
C-       data...
C-
C-   Inputs  : FILE_NAMES - File containing list of input Event catalogs
C-             ECOUT      - Output event catalog.  (Created here)
C-   Outputs : IERR       - 0 ==> No error
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
      CHARACTER*(*) FILE_NAMES,ECOUT,HERE
      INTEGER IERR
C
      INTEGER NECIN_MAX,NSKIP_MAX
      PARAMETER(NECIN_MAX=12,NSKIP_MAX=100,HERE='D0DAD_ECMERGE')
      INTEGER NECIN,INLUN(NECIN_MAX),LRUNDAT(NECIN_MAX),NRUNS(NECIN_MAX)
      INTEGER NEXT_RUN(NECIN_MAX),DR(NECIN_MAX),LEVTDAT(NECIN_MAX)
      INTEGER NSKIP(NECIN_MAX),SKIP(NSKIP_MAX,NECIN_MAX)
      CHARACTER*255 INLINE,SKIPSTR,CRUN*12
C
      INTEGER LUNOUT,ITERATION,CURRENT,IRUN,IREC,NEVTS,LINTC(2),LBASE,I
      INTEGER IDFID,J
      INTEGER LENOCC,D0DAD_ECMERGE_NEXTRUN
      LOGICAL D0DAD_SKIP_RUN
C
      COMMON/MERGELINKS/LINTC,LRUNDAT,LEVTDAT
C
C-----------------------------------------------------------------------
C
C  Initialize, including protected links for input catalogs run sections...
C
      CALL VZERO(NSKIP,NECIN_MAX)
      IERR=0
      NECIN=0
      CALL MZLINT(IXCOM,'/DADMERGE/',LINTC,LRUNDAT,LEVTDAT(NECIN_MAX))
C
C  Open the input file catalogs...
C
      CALL D0OPEN(IDADOK,FILE_NAMES(1:LENOCC(FILE_NAMES)),'I',IERR)
      IF(LDDBG.GT.3) WRITE(*,1000) 
 1000 FORMAT(/,'MERGE, Input Event Catalogs: ')
 10   CONTINUE
        IF( NECIN.GT.NECIN_MAX ) THEN
          WRITE(D0DAD_ERRTXT,9001) NECIN_MAX
 9001     FORMAT('Too many input d0dad files to merge. Limit=',I6)
          IF(LDDBG.GT.0) CALL ERRMSG('InputOver',HERE,D0DAD_ERRTXT,'E')
          IERR = -1
          GOTO 999
        ENDIF
        FNTMP=' '
        SKIPSTR=' '
        READ(IDADOK,'(A)',END=20,ERR=9902) INLINE
        CALL GETWORD(INLINE,1,FNTMP)
        CALL GETWORD(INLINE,2,SKIPSTR)
        NECIN=NECIN+1
        CALL D0DAD_OPEN(JFEC,FNTMP,'R',INLUN(NECIN),IERR)
        IF( IERR.NE.0 ) THEN
          IERR = -3
          GOTO 999
        ELSE
          IF(LDDBG.GT.3) WRITE(*,1001) NECIN,FNTMP(1:LENOCC(FNTMP))
 1001     FORMAT('  Catalog ',I2,': ',A)
        ENDIF
        CALL ECLSET(INLUN(NECIN),IERR)
        NRUNS(NECIN)=IQ(LECHD+NDEC+JEC_IECRUN)
        LRUNDAT(NECIN)=LRUNS
        DR(NECIN)=IQ(LECHD+NDEC+JEC_IRECEC)
        NEXT_RUN(NECIN)=1
C-   Check for local skips
        IF( LENOCC(SKIPSTR).GT.0 ) THEN
          IF( SKIPSTR(1:1).EQ.'(' ) SKIPSTR(1:1)=' '
          I=LENOCC(SKIPSTR)
          IF( SKIPSTR(I:I).EQ.' ' ) SKIPSTR(I:I)=' '
          DO I=1,LENOCC(SKIPSTR)
            IF( SKIPSTR(I:I).EQ.',') SKIPSTR(I:I)=' '
          ENDDO
 21       CONTINUE
            CRUN=' '
            CALL GETWORD(SKIPSTR,NSKIP(NECIN)+1,CRUN)
            IF( LENOCC(CRUN).LE.0 ) GOTO 10
            NSKIP(NECIN)=NSKIP(NECIN)+1
            READ(CRUN,*) SKIP(NSKIP(NECIN),NECIN)
          GOTO 21
        ENDIF
      GOTO 10
 20   CONTINUE
      CLOSE(IDADOK)
      IF(LDDBG.GT.3) WRITE(*,*) ' ' 
C
C  Open the output catalog...
C
      CALL D0DAD_OPEN(JFEC,ECOUT,'W',LUNOUT,IERR)
      IF( IERR.NE.0 ) THEN
        IERR= - 4
        GOTO 999
      ENDIF
C
C  Loop over all runs in all catalogs until exhausted...
C
      ITERATION=0
      CURRENT=D0DAD_ECMERGE_NEXTRUN(NECIN,LRUNDAT,NRUNS,NEXT_RUN,DR)
      DO WHILE( CURRENT.GT.0 )
        ITERATION=ITERATION+1
        IRUN=IQ(LRUNDAT(CURRENT)+DR(CURRENT)*(NEXT_RUN(CURRENT)-1)+1)
        IF( D0DAD_SKIP_RUN(IRUN) ) THEN
          WRITE(D0DAD_ERRTXT,8004) IRUN,'global'
 8004     FORMAT('Run ',I6,' on ',A,' skip list.  Do not process')
          IF(LDDBG.GT.2)CALL ERRMSG('SkpRun','ECMERGE',D0DAD_ERRTXT,'W')
          GOTO 30
        ENDIF
C-     Check for local runs to skip
        IF( NSKIP(CURRENT).GT.0 ) THEN
          DO I=1,NSKIP(CURRENT)
            IF( IRUN.EQ.SKIP(I,CURRENT) ) THEN
              DO J=I+1,NSKIP(CURRENT)
                SKIP(J-1,CURRENT)=SKIP(J,CURRENT)
              ENDDO
              NSKIP(CURRENT)=NSKIP(CURRENT)-1
              WRITE(D0DAD_ERRTXT,8004) IRUN,'local'
              IF(LDDBG.GT.2)CALL ERRMSG('SkpRun','ECMERGE',
     >          D0DAD_ERRTXT,'W')
              GOTO 30
            ENDIF
          ENDDO
        ENDIF
        CALL ECLSET(INLUN(CURRENT),IERR)
        CALL ECGRUN(IRUN,IREC,IERR)
        NEVTS=IQ(LRDAT-1)/DR(CURRENT)
        LEVTDAT(CURRENT)=LRDAT
        IF(LDDBG.GT.4) WRITE(*,2001) ITERATION,CURRENT,IRUN,NEVTS
 2001   FORMAT(I6,': ',I6,', Run=',I8,' has',I9,' events')
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,9005) IERR,IRUN,CURRENT
 9005     FORMAT('Error ',I5,' getting run ',I7,' from file #',I2)
          IF(LDDBG.GT.0)CALL ERRMSG('ReadRunFail',HERE,D0DAD_ERRTXT,'E')
          NEVTS=0
        ENDIF
        DO I=1,NEVTS
          LBASE=LEVTDAT(CURRENT)+DR(CURRENT)*(I-1)
          IDEVNT=IQ(LBASE+1)
          CALL UCOPY(IQ(LBASE+2),ISTMSK,2)
          IDFID=IQ(LBASE+4)
          IDZRN=IQ(LBASE+5)
          IDZBO=IQ(LBASE+6)
          CALL ECPUT(LUNOUT,IRUN,IDEVNT,ISTMSK,IDFID,IDZRN,
     >       IDZBO,1,IERR)
          IF( IERR.NE.0 ) THEN
            WRITE(D0DAD_ERRTXT,9006) IERR,IRUN,IDEVNT,I,NEVTS
 9006       FORMAT('Error ',I3,' inserting run/event',2I7,' (Seq #',I6,
     >         ', of',I7)
            IF(LDDBG.GT.0)CALL ERRMSG('ECPUTFail',HERE,D0DAD_ERRTXT,'E')
          ENDIF
        ENDDO
        CALL ECLSET(INLUN(CURRENT),IERR)
        CALL ECDRUN(IERR)
 30     CURRENT=D0DAD_ECMERGE_NEXTRUN(NECIN,LRUNDAT,NRUNS,NEXT_RUN,DR)
      ENDDO
C
C  Close the input catalogs
C
      DO I=1,NECIN
        CALL D0DAD_CLOSE(INLUN(I),IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(D0DAD_ERRTXT,8001) IERR,I
 8001     FORMAT('Error ',I3,' closing input file #',I3)
          CALL ERRMSG('CloseError(IN)',HERE,D0DAD_ERRTXT,'E')
        ENDIF
      ENDDO
C
C  Flush the internal buffer and close output event catalog...
C
      IRUN=-1
      CALL ECPUT(LUNOUT,IRUN,IDEVNT,ISTMSK,IDFID,IDZRN,IDZBO,
     +   1,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,8002) IERR
 8002   FORMAT('Error ',I3,' returned from ECPUT to flush output')
        CALL ERRMSG('ECFlushError',D0DAD_ERRTXT,HERE,'E')
      ENDIF
      CALL D0DAD_CLOSE(LUNOUT,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(D0DAD_ERRTXT,8003) IERR
 8003   FORMAT('Error ',I3,' returned from Closing output')
        CALL ERRMSG('CloseError(OUT)',HERE,D0DAD_ERRTXT,'E')
      ENDIF
C
  999 RETURN
C
 9902 CONTINUE
      IERR = -2
      GOTO 999
C
      END
