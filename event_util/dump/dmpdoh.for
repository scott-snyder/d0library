      SUBROUTINE DMPDOH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     do logic for hexadecimal (raw) event dump
C-
C-   Created   4-AUG-1988   Serban D. Protopopescu
C- 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      LOGICAL FLGVAL,ALL
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INTEGER IER,IZ,N
      LOGICAL OK
      CHARACTER*24 FILNAM
C----------------------------------------------------------------------
C
      CALL GTUNIT(22,DUNIT,IER)
      CALL EVENT_FILE(FILNAM,N)
      FILNAM=FILNAM(1:N)//'.DUMP'
      CALL D0OPEN(DUNIT,FILNAM,'O',OK)
      IF(.NOT.OK) GOTO 100
C
      ALL=FLGVAL('DUMP_ALL_H')
C
      IF(FLGVAL('DUMP_TRGR').OR.ALL) CALL DMPPRH(DUNIT,IZTRGR)
C
      IF(FLGVAL('DUMP_MUD1').OR.ALL) CALL DMPPRH(DUNIT,IZMUD1)
C
      IF(FLGVAL('DUMP_CDD1').OR.ALL) CALL DMPPRH(DUNIT,IZCDD1)
C
      IF(FLGVAL('DUMP_CDD2').OR.ALL) CALL DMPPRH(DUNIT,IZCDD2)
C
      IF(FLGVAL('DUMP_CDD3').OR.ALL) CALL DMPPRH(DUNIT,IZCDD3)
C
      IF(FLGVAL('DUMP_CDD4').OR.ALL) CALL DMPPRH(DUNIT,IZCDD4)
C
      IF(FLGVAL('DUMP_CAD1').OR.ALL) CALL DMPPRH(DUNIT,IZCAD1)
C
      IF(FLGVAL('DUMP_CAD2').OR.ALL) CALL DMPPRH(DUNIT,IZCAD2)
C
      CLOSE(DUNIT)
      CALL INTMSG(' Raw event dump in EVENT.DUMP')
      CALL RLUNIT(22,DUNIT,IER)
C
      IF(FLGVAL('DUMP_SCREEN')) CALL DMPSCR(FILNAM) ! put dump on screen
C
      IF(FLGVAL('DUMP_QPRINT')) 
     &  CALL QPRINT(FILNAM,'DUMP_PRINTER','QTYPE',.TRUE.) ! dump on
C                                                         ! laser printer
c
      RETURN
  100 CALL INTMSG(' Cannot open EVENT.DUMP')
  999 RETURN
      END
