      INTEGER FUNCTION DFMERGE_NEXTEVT(NIN,INLUN,RUN,EVENT,FID,ZRN,
     >   ZBO,TIME,ISDONE)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Choose the input file number from which to
C-    write the next event...
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-Mar-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) HERE
      PARAMETER(HERE='DFMERGE_NEXTEVT')
      INTEGER NIN,INLUN(*),RUN(*),EVENT(*),FID(*),ZRN(*),ZBO(*),TIME(*)
      LOGICAL ISDONE(*)
      INTEGER I,J,IEVT,IERR,IFID,LAST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,LAST
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN   ! Initialize
        LAST=0
        CALL VZERO(ISDONE,NIN)
        DO I=1,NIN
          CALL DFGET(INLUN(I),RUN(I),EVENT(I),FID(I),ZRN(I),ZBO(I),
     >       TIME(I),IERR)
          IF( IERR.EQ.1 ) ISDONE(I)=.TRUE.
          IF( IERR.LT.0 ) THEN
            WRITE(D0DAD_ERRTXT,9001) IERR,I
 9001       FORMAT('Error ',I4,' returned from DFGET for file ',I2)
            IF(LDDBG.GT.0)CALL ERRMSG('ErrorDFG',HERE,D0DAD_ERRTXT,'E')
            DFMERGE_NEXTEVT = -1
            GOTO 999
          ENDIF
        ENDDO
        FIRST=.FALSE.
      ENDIF
C
C- Can overwrite the info for the last event used in order to set up
C- for the next check...
C
      IF( LAST.NE.0 ) THEN
        I=LAST
        LAST=0
        CALL DFGET(INLUN(I),RUN(I),EVENT(I),FID(I),ZRN(I),ZBO(I),
     >       TIME(I),IERR)        
        IF( IERR.EQ.1 ) ISDONE(I)=.TRUE.
        IF( IERR.LT.0 ) THEN
          WRITE(D0DAD_ERRTXT,9001) IERR,I
          IF(LDDBG.GT.0)CALL ERRMSG('ErrorDFG',HERE,D0DAD_ERRTXT,'E')
          DFMERGE_NEXTEVT = -1
          GOTO 999
        ENDIF
      ENDIF

C- Find the lowest numbered run/event pair left to process (ignore dups)...
C- J is index of file with lowest numbered run/event pair. 
C-   Find first file with events remaining
      DFMERGE_NEXTEVT=0
      J=1
      DO WHILE( ISDONE(J) .AND. J.LE.NIN)
        J=J+1
      ENDDO
      IF( J.GT.NIN ) GOTO 999
C-   See if any later files have strictly lower run/event number
      DO I=1,NIN
        IF( I.EQ.J ) GOTO 10
        IF( ISDONE(I) ) GOTO 10
        IF( RUN(I).GT.RUN(J) ) GOTO 10
        IF( RUN(I).EQ.RUN(J) .AND. EVENT(I).GE.EVENT(J) ) GOTO 10
        J=I
 10     CONTINUE
      ENDDO
C
C- Check for duplicates (J is file index of first file with lowest run/event)...
C- If any are found, read past them.  This assumes that each file is ordered
C- properly.
C
      DO I=1,NIN
        IF( I.EQ.J ) GOTO 20
        IF( RUN(I).NE.RUN(J) ) GOTO 20
        IF( EVENT(I).NE.EVENT(J) ) GOTO 20
C
        CALL DFGET(INLUN(I),RUN(I),EVENT(I),FID(I),ZRN(I),ZBO(I),
     >       TIME(I),IERR)        
        IF( IERR.EQ.1 ) ISDONE(I)=.TRUE.
        IF( IERR.LT.0 ) THEN
          WRITE(D0DAD_ERRTXT,9001) IERR,I
          IF(LDDBG.GT.0)CALL ERRMSG('ErrorDFG',HERE,D0DAD_ERRTXT,'E')
          DFMERGE_NEXTEVT = -1
          GOTO 999
        ENDIF
C
 20     CONTINUE
      ENDDO
C
C- Return and save the file index...
C
      DFMERGE_NEXTEVT = J
      LAST = J
C
 999  CONTINUE
      RETURN
      END
