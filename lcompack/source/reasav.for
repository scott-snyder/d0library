      SUBROUTINE REASAV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read back and display lines which was written
C-                         to unit 6 while opened as file FILSAV.
C-                         VAX-specific.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER IOS,IUNI,IERR
      CHARACTER*80 MSGLIN
      LOGICAL OK
C----------------------------------------------------------------------
      CLOSE(6)
      CALL GTUNIT(555,IUNI,IERR)
      IF(IERR.EQ.0) THEN
        CALL D0OPEN(IUNI, FILSAV, 'IF', OK)
        IF(OK) THEN
          IOS = 0
          DO WHILE (IOS.EQ.0)
            READ(IUNI,FMT='(A)',IOSTAT=IOS) MSGLIN
            IF(IOS.EQ.0) THEN
              CALL INTMSG(' '//MSGLIN)
            ENDIF
          ENDDO
          CLOSE(UNIT=IUNI,STATUS='DELETE')
          CALL RLUNIT(555,IUNI,IERR)
        ENDIF
      ENDIF
      RETURN
      END
