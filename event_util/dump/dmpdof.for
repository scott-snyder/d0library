      SUBROUTINE DMPDOF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Do a formatted  (proces) dump
C-
C-   Created   4-AUG-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      LOGICAL FLGVAL,OK
      EXTERNAL PRHEAD,PRTSUM
      CHARACTER*24 FILNAM
      INTEGER IER,N
C----------------------------------------------------------------------
C
      CALL GTUNIT(22,DUNIT,IER)
      CALL EVENT_FILE(FILNAM,N)
      FILNAM=FILNAM(1:N)//'.DUMP'
      IF (.NOT.FLGVAL('DUMP_NONE_H')) THEN
        CALL D0OPEN(DUNIT,FILNAM,'A',OK)
        IF(.NOT.OK) GOTO 100
      ELSE
        CALL D0OPEN(DUNIT,FILNAM,'O',OK)
        IF(.NOT.OK) GOTO 100
      ENDIF
      CALL FLSETS('DMPUSR',.TRUE.)     ! reset all flags for declared banks
      CALL DMPANY('HEAD',PRHEAD)       ! dump header bank
      CALL DMPANY('TSUM',PRTSUM)       ! dump trigger names bank
      CALL DMPUSR                      ! user hook
      CLOSE(DUNIT)
      CALL RLUNIT(22,DUNIT,IER)
C
      IF(FLGVAL('DUMP_SCREEN')) CALL DMPSCR(FILNAM) ! put dump on screen
C
      IF(FLGVAL('DUMP_QPRINT')) 
     &  CALL QPRINT(FILNAM,'DUMP_PRINTER','QTYPE',.TRUE.) ! dump on
C                                                         ! laser printer
      RETURN
  100 CALL INTMSG(' Cannot open'//FILNAM)
  999 RETURN
      END
