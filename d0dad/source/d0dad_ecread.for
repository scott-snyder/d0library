      SUBROUTINE D0DAD_ECREAD(INCATALOG,IRUN,IEVENT,IER)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Read the given run/event using an event 
C-     the given event catalog.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-May-1994   John D. Hobbs
C-
C--------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0DADCOM.INC'
      INTEGER IRUN,IEVENT,IER,IFID,ICNTXT,IMSK(2),IZRN,IZBO,ILUN
      INTEGER ILEN,ILUNFC,I
      INTEGER LENOCC
      LOGICAL LIB$FIND_FILE
      EXTERNAL LENOCC
      CHARACTER*132 CATALOG,NEWCATALOG,FCATALOG,FTEMP,ERRTXT
      CHARACTER*(*) INCATALOG
      DATA CATALOG/'NULLCATALOG'/
      SAVE CATALOG
C--------------------------------------------------------------------
C
      IER=0
      NEWCATALOG=INCATALOG
      CALL D0DAD_CPAD(NEWCATALOG)
      ILEN=LENOCC(NEWCATALOG)
C
C   Open catalogs only if the event requires a new event catalog...
C
      IF( NEWCATALOG.NE.CATALOG ) THEN
C    Find the file...
        IF( .NOT.LIB$FIND_FILE(NEWCATALOG(1:ILEN),CATALOG,ICNTXT) ) 
     >  THEN
          CATALOG='NULLCATALOG'
          IER = -1
          CALL LIB$FIND_FILE_END(ICNTXT)
          GOTO 999
        ENDIF
C    Open event catalog...
        CALL D0DAD_OPEN(JFEC,CATALOG,'R',ILUN,IER)
        IF( IER.NE.0 ) THEN
          WRITE(ERRTXT,1001) IER,NEWCATALOG(1:LENOCC(NEWCATALOG))
 1001     FORMAT(' Error ',I4,' opening event catalog: ',A)
          CALL ERRMSG('CATALOG_OPENFAIL','D0DAD_ECREAD',ERRTXT,'W')
          IER = -2
          GOTO 999
        ENDIF
C   Open file catalog...
        CALL FILENAME_PARSE(CATALOG,'DIR+NAM',FTEMP,ILEN)
        DO I=1,LEN(FTEMP)
           IF( FTEMP(I:I).LT.CHAR(20) ) FTEMP(I:I)=' '
        ENDDO
        ILEN=LENOCC(FTEMP)
        FCATALOG=FTEMP(1:ILEN)//'.filecat'
        CALL D0DAD_OPEN(JFFC,FCATALOG,'R',ILUNFC,IER)
        IF( IER.NE.0 ) THEN
          WRITE(ERRTXT,1005) IER,FCATALOG(1:LENOCC(FCATALOG))
 1005     FORMAT(' Error ',I4,' opening file catalog: ',A)
          CALL ERRMSG('CATALOG_OPENFAIL','D0DAD_ECREAD',ERRTXT,'W')
          IER = -6
          GOTO 999
        ENDIF        
        CALL D0DAD_DFREAD_SETX(ILUNFC)
        CATALOG=NEWCATALOG
      ENDIF
C
C   Get the event entry from the catalog
C
      CALL ECGET(ILUN,IRUN,IEVENT,IMSK,IFID,IZRN,IZBO,IER)
      IF( IER.EQ.(-1) .OR. IER.EQ.(-2) ) THEN
        WRITE(ERRTXT,1002) IRUN,IEVENT
 1002   FORMAT(' Run/Event ',2I8,' not found in event catalog')
        IER = -3
        GOTO 999
      ELSEIF( IER.NE.0 ) THEN
        WRITE(ERRTXT,1003) IER
 1003   FORMAT(' Error ',I5,' returned from event catalog read')
        CALL ERRMSG('EC_READ_ERROR','D0DAD_ECREAD',ERRTXT,'W')
        IER = -4
        GOTO 999
      ENDIF
C
C   Read it...
C
      CALL D0DAD_READ_EVENT(IRUN,IEVENT,IZRN,IZBO,IFID,IER)
      IF( IER.NE.0 ) THEN
        WRITE(ERRTXT,1004) IRUN,IEVENT,IER
 1004   FORMAT(' Error ',I4,' from D0DAD_READEVENT for run/event ',2I8)
        CALL ERRMSG('EVENT_READ_ERROR','D0DAD_ECREAD',ERRTXT,'W')
        IER = -5
        GOTO 999
      ENDIF
C
  999 RETURN
      END
