      INTEGER FUNCTION D0DAD_ECMERGE_NEXTRUN(N,LRUNS,NRUNS,NEXT,DR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Return the index of the event catalog from
C-    which the next run will be processed
C-
C-   Inputs  : N     - Number of event catalogs being merged
C-             LRUNS - Protected links to run sections
C-             NRUNS - Number of runs in event catalog
C-             NEXT  - Index of next unprocessed run in catalog
C-   Outputs : Return the index of the next catalog to process or 0
C-    if finished...
C-
C-   Controls:
C-
C-   Created   4-Mar-1995   John D. Hobbs
C-   Modified 17-Dec-1995   JDH - Change next selection algorithm to
C-        pick lowest numbered file before others instread of 
C-        picking current before others if two or more catalogs have
C-        the same run number.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N,LRUNS(*),NRUNS(*),NEXT(*),DR(*)
      INTEGER I,ILAST,MIN_RUN,THE_RUN,IRUN
      LOGICAL ALL_DONE
      DATA ILAST/0/
      SAVE ILAST
C-----------------------------------------------------------------------
C
C  Check that there is more data to process...
C
      IF( ILAST.NE.0 ) NEXT(ILAST)=NEXT(ILAST)+1
      ALL_DONE=.TRUE.
      DO I=1,N
        IF( NEXT(I).LE.NRUNS(I) ) ALL_DONE=.FALSE.
      ENDDO
      IF( ALL_DONE ) THEN
        D0DAD_ECMERGE_NEXTRUN=0
        GOTO 999
      ENDIF
C
C  Get next run (start with something outrageous...
C
      THE_RUN=2000000000
      DO I=1,N
        IRUN=IQ(LRUNS(I)+DR(I)*(NEXT(I)-1)+1)
        IF( IRUN.LT.THE_RUN .AND. NEXT(I).LE.NRUNS(I) ) THEN
          MIN_RUN=I
          THE_RUN=IRUN
        ENDIF
      ENDDO
C
      ILAST=MIN_RUN
      D0DAD_ECMERGE_NEXTRUN=MIN_RUN
C
 999  RETURN
      END
