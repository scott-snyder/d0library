      SUBROUTINE D0DBL3_FINISH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Close database and file. Finish up.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-JUN-1992   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER NF,I
      CHARACTER*80 MSG
      CHARACTER*12 PATH
C----------------------------------------------------------------------
C
      NF = 1
      IF(ICOMP .GT. 0) NF = 2
C
      IF (DBINI) THEN
        DO I=1,NF
          PATH = '//'//TOPN(I)
          CALL RZCDIR(PATH, ' ')
          IF(OPTJ) CALL D0DBL3_END_JOURNAL
          CALL DBEND
          IF (IQUEST(1).NE.0) THEN
            CALL ERRDB(' DBEND')
            GOTO 999
          ENDIF
          CLOSE(UNIT=DBLUN(I))
        ENDDO
        DBINI = .FALSE.
      ELSE
        WRITE(MSG,10)
   10   FORMAT(' Attempt to Call DBEND the second time. Aborted ')
        CALL INTMSG(MSG)
      ENDIF
C
  999 RETURN
      END
