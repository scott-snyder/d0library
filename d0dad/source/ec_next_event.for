      LOGICAL FUNCTION EC_NEXT_EVENT(SELECT,RUN,EVENT)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Get the next event to be processed according
C-     to the selection method in SELECT.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) SELECT
      INTEGER ILUN,RUN,EVENT
C- Temporary internal variables
      INTEGER MATCH_RUN,MATCH_EVENT,MATCH_FID,IEVT,IRECEC,IERR,FID,IREC
      INTEGER IOFFS
      LOGICAL MATCHED
C- External functions
      INTEGER LENOCC
C- Long term internal storage.
      INTEGER OLDRUN,NEXT_EVENT,NEVT
      CHARACTER*256 MATCH_STRING
      DATA OLDRUN/0/,NEXT_EVENT/1/,NEVT/0/,MATCH_STRING/' '/
      SAVE OLDRUN,NEXT_EVENT,NEVT,MATCH_STRING
C-----------------------------------------------------------------------
C
      MATCH_RUN=0
      MATCH_EVENT=0
      MATCH_FID=0
      CALL D0DAD_EXTRACT_IFIELD('RUN=',SELECT,MATCH_RUN)
      CALL D0DAD_EXTRACT_IFIELD('EVENT=',SELECT,MATCH_EVENT)
      CALL D0DAD_EXTRACT_IFIELD('FID=',SELECT,MATCH_FID)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)

C- Check for invalid selection string (Require specific run)

      IF( MATCH_RUN.EQ.0 ) THEN
        WRITE(*,1001) SELECT(1:LENOCC(SELECT))
 1001   FORMAT(' EC_NEXT_EVENT: Invalid match string: ',A)
        GOTO 998
      ENDIF

C- Do we have a new search string?

      IF( MATCH_STRING.NE.SELECT ) THEN
        IF( MATCH_RUN.NE.OLDRUN ) THEN
          CALL ECGRUN(MATCH_RUN,IREC,IERR)
          IF( IERR.NE.0 ) THEN
            WRITE(*,1002) IERR,MATCH_RUN
 1002       FORMAT(' Error ',I5,' getting data for run',I7)
            GOTO 998
          ENDIF
          IOFFS=IQ(LECHD+JIRUN)
          NEVT=IQ(LRUNS+IRECEC*(IOFFS-1)+4)
          OLDRUN=MATCH_RUN
        ENDIF
        MATCH_STRING=SELECT
        NEXT_EVENT=1
      ENDIF

C- Loop over events until a match is found or all events have been
C- examimed

      RUN=MATCH_RUN
      IEVT=NEXT_EVENT
      MATCHED=.FALSE.
      DO WHILE( .NOT.MATCHED .AND. IEVT.LE.NEVT ) 
        IOFFS=IRECEC*(IEVT-1)
        EVENT=IQ(LRDAT+IOFFS+JEVNT)
        FID=IQ(LRDAT+IOFFS+JFID)
        MATCHED=.TRUE.
        IF( MATCH_EVENT.NE.0 .AND. MATCH_EVENT.NE.EVENT )MATCHED=.FALSE.
        IF( MATCH_FID.NE.0 .AND. MATCH_FID.NE.FID ) MATCHED=.FALSE.
        IEVT=IEVT+1
      ENDDO
C
      IF( .NOT.MATCHED ) GOTO 998
C
 999  CONTINUE
      NEXT_EVENT=IEVT
      EC_NEXT_EVENT=.TRUE.
      RETURN

 998  CONTINUE
      EC_NEXT_EVENT=.FALSE.
      END
