      SUBROUTINE D0DAD_LIST(NRUN,NEVENT,IRECORD,IBYTE,STREAM,
     &  FILTER_WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates and updates an event list for each
C-                         of the specified output streams.
C-
C-   Inputs  : NRUN   I run number
C-             NEVENT I event number
C-             IRECORD I record
C-             IBYTE   I Byte offset
C-             STREAM C NAME OR LOGICAL NAME OF THE OUTPUT FILE,stream name
C-             FILTER_WORD I(2) FILTER WORDs
C-   Outputs : D0DAD FILE
C-   Controls:
C-
C-   Created  17-FEB-1994   Lee Lueking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NRUN,NEVENT,FILTER_WORD(2)
      CHARACTER*40 STREAM_NAME(40)
      CHARACTER*(*)STREAM
      CHARACTER*40 FILENAME
      LOGICAL OK,FIRST
      INTEGER LUN(20),ERR,NSTRM,LENGTH,I,UNIT
      INTEGER TRULEN,IRECORD,IBYTE
      DATA STREAM_NAME /40*'                                        '/
      DATA FIRST/.TRUE./
      SAVE FIRST,NSTRM,STREAM_NAME
C----------------------------------------------------------------------
C Add the event to the proper stream list
C----------------------------------------------------------------------
      DO I=1,NSTRM
        IF(STREAM_NAME(I).EQ.STREAM)THEN
          UNIT=LUN(I)
        ENDIF
      ENDDO
      WRITE(UNIT,50)NRUN,NEVENT,FILTER_WORD(1),FILTER_WORD(2),IRECORD,
     &  IBYTE
   50 FORMAT( 2I8,2I15,2I10)
      RETURN
C----------------------------------------------------------------------
C Beginning of job entry point OPEN A FILE FOR CURRENT STREAM
C----------------------------------------------------------------------
      ENTRY D0DAD_LIST_BOJ(STREAM)
      IF(FIRST)THEN
        FIRST=.FALSE.
        NSTRM=0
      ENDIF
      OK=.TRUE.
      NSTRM=NSTRM+1
      STREAM_NAME(NSTRM)=STREAM
      FILENAME=STREAM
      CALL GTUNIT(200,LUN(NSTRM),ERR)
C      WRITE(6,*)'Write stream ',STREAM_NAME(NSTRM),
C     &' to unit ',LUN(NSTRM),' File name ',FILENAME
      IF(ERR.NE.0)THEN
        OK=.FALSE.
        RETURN
      ENDIF
      UNIT=LUN(NSTRM)
      CALL D0OPEN (UNIT,FILENAME,'OFL',OK)
      WRITE(UNIT,51)
   51 FORMAT('D0DAD UE  00.00 128 128 128 128')
      WRITE(UNIT,52)
   52 FORMAT('!FNAME  ',120(' '))
      WRITE(UNIT,53)
   53 FORMAT('!GNAME  ',120(' '))
      WRITE(UNIT,54)
   54 FORMAT('!TAPE   ',120(' '))
      WRITE(UNIT,55)
   55 FORMAT('!DATE   ',120(' '))
      WRITE(UNIT,56)
   56 FORMAT
     &('*HEAD RUN     EVENT   FILTER_BIT1   FILTER_BIT2',
     &'   RECORD   BYTE')
      RETURN
C----------------------------------------------------------------------
C End of job entry point, CLOSE ALL FILES FOR STREAMS
C----------------------------------------------------------------------
      ENTRY D0DAD_LIST_EOJ(STREAM)
      DO I=1,NSTRM
        CLOSE(LUN(I))
        CALL RLUNIT(200,LUN(I),ERR)
      ENDDO
      RETURN
      END
