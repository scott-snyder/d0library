      SUBROUTINE EVENT_LIST(NRUN,NEVENT,STREAM,FILTER_WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates and updates an event list for each
C-                         of the specified output streams.
C-
C-   Inputs  : NRUN   I run number 
C-             NEVENT I event number
C-             STREAM C 3 char stream name
C-             FILTER_WORD I FILTER WORD
C-   Outputs : File with 
C-   Controls: 
C-
C-   Created  16-MAR-1993   Lee Lueking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NRUN,NEVENT,FILTER_WORD
      CHARACTER*3 STREAM,STREAM_NAME(20)
      CHARACTER*40 FILENAME,LABEL
      LOGICAL OK,FIRST
      INTEGER LUN(20),ERR,NSTRM,LENGTH,I,UNIT
      INTEGER TRULEN
      DATA STREAM_NAME /20*'   '/,FIRST/.TRUE./
C----------------------------------------------------------------------
C Add the event to the proper stream list
C----------------------------------------------------------------------
      DO I=1,NSTRM
        IF(STREAM_NAME(I)(1:3).EQ.STREAM(1:3))THEN
          UNIT=LUN(I)
        ENDIF
      ENDDO
      WRITE(UNIT,50)NRUN,NEVENT,FILTER_WORD
 50   FORMAT( 2I10,1I15)
      RETURN
C----------------------------------------------------------------------
C Beginning of job entry point OPEN A FILE FOR CURRENT STREAM
C----------------------------------------------------------------------
      ENTRY EVENT_LIST_BOJ(NRUN,NEVENT,STREAM)
      IF(FIRST)THEN
C   Get parameters from files.RCP
        CALL INRCP('FILES_RCP',ERR)
        IF (ERR.NE.0) THEN
          LABEL = 'EVENT'
          LENGTH = TRULEN(LABEL)
        ELSE
          CALL EZPICK('FILES_RCP')
          CALL EZGETS('TAPE_NAME',1,LABEL,LENGTH,ERR)
        ENDIF
        FIRST=.FALSE.
        NSTRM=0
      ENDIF
      OK=.TRUE.
      NSTRM=NSTRM+1
      STREAM_NAME(NSTRM)=STREAM
      FILENAME=LABEL(1:length)//'.LIST_'//STREAM(1:3)
      CALL GTUNIT(200,LUN(NSTRM),ERR)
      WRITE(6,*)'Write stream ',STREAM_NAME(NSTRM),
     &' to unit ',LUN(NSTRM),' File name ',FILENAME 
      IF(ERR.NE.0)THEN
        OK=.FALSE.
        RETURN
      ENDIF
      CALL D0OPEN (LUN(NSTRM),FILENAME,'OFL',OK)
      RETURN
C----------------------------------------------------------------------
C End of job entry point, CLOSE ALL FILES FOR STREAMS
C----------------------------------------------------------------------
      ENTRY EVENT_LIST_EOJ(NRUN,NEVENT,STREAM)
      DO I=1,NSTRM
        CLOSE(LUN(I))
        CALL RLUNIT(200,LUN(I),ERR)
      ENDDO
      RETURN
      END
