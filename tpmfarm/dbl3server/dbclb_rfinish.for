      SUBROUTINE DBCLB_RFINISH(DETYP,IC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 - Finish reading dbl3 server
C-
C-   Inputs  :  DETYP   Detector type
C-              IC      0=do not close file, 1=close file
C-   Outputs :
C-   Controls:
C-
C-   Created   08-FEB-1994   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DETYP
      INTEGER IC
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER IER,ISEQ
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
      DOPT = 0
      LFORCE = .FALSE.
      LVSN = .FALSE.
      CALL DBCLB_DETSEQ(DETYP,ISEQ)
      IF (CDBEND(ISEQ)) THEN
	CDBEND(ISEQ) = .FALSE.
        TOPN(2:2) = DETYP(1:1)
        IF(DETYP(1:3) .EQ. 'CDC') TOPN(2:2) = 'D'
        CALL DBENDF(TOPN)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB(' DBEND')
        ENDIF
        IF(IC .GT. 0) CLOSE(UNIT=LUNDB(ISEQ))
      ELSE
        WRITE(MSG,10)
   10   FORMAT(' Attempt to Call DBEND the second time. Aborted ')
        CALL INTMSG(MSG)
      ENDIF
      TOPN = 'D0STP'
C
      IF(OPTJ) THEN
        CALL DBCLB_END_JOURNAL
        OPTJ = .FALSE.
      ENDIF
C
  999 RETURN
      END
