      SUBROUTINE ERNMEN(NINTEG,ILIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   A menu for solid and surface rendering
C-
C-   Inputs   NINTEG, ILIST ;  number & list of segments
C-   Outputs
C-            ILIST(NINTEG+1) ; if .ne. -1, then stop
C-
C-   Created   2-MAY-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  NINTEG, ILIST(*), I, ISEG
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      CHARACTER*15 ANS
      CHARACTER*4 SEGN, SSTR
      LOGICAL COUNT
      EXTERNAL ERRHND
C
      WRITE(6,1)
    1 FORMAT(/10X,'TOGGLE ',5X,'BK-FACE REMOV',5X,'HIDN-LIN REMV',/
     &        10X,'SHADE  ',5X,'UN-RENDER    ',5X,'RENDER AGAIN ',/
     &        10X,'SOLID  ',5X,'SURFACE      ',5X,'WASH-SCREEN  ',/
     &        10X,'TURN ON',5X,'STOP         ',//
     &        10X,'OPTION ? ---->   ', $)
      ACCEPT '(A2)', ANS
C
      IF(ANS(1:2) .EQ. 'ST') THEN
        ILIST(NINTEG+1) = 0
        GOTO 99
      ENDIF
C
      IF(ANS(1:2) .EQ. 'WA') THEN
        CALL PSNFIX(2, 7, 'SHADINGENVIRONMENT', ERRHND)
        GOTO 99
      ENDIF
C
      IF(ANS(1:2) .EQ. 'TU') THEN
        CALL PSNFIX(0, 1, 'TURNONDISPLAY', ERRHND)
        GOTO 99
      ENDIF
C
C----------------------------------------------------------------
C
      CALL PFN('CONS"', 'CONSTANT', ERRHND)
      CALL PSNFIX(0, 2, 'CONS"', ERRHND)
C      IF(IREND .EQ. 1) THEN
C            CALL PCONN(DISPL//'.SURRN"', 1, 1, 'CONS"', ERRHND)
C            CALL PCONN('CONS"', 1, 1, 'TURNONDISPLAY', ERRHND)
C        IF(ANS(1:2) .EQ. 'TO') THEN
C          CALL PSNFIX(0, 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'BK') THEN
C          CALL PSNFIX(3, 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'HI') THEN
C          CALL PSNFIX(4, 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'SH') THEN
C          CALL PSNFIX(7, 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'UN') THEN
C          CALL PSNBOO(.FALSE., 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'RE') THEN
C          CALL PSNBOO(.TRUE., 1, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'SO') THEN
C          CALL PSNBOO(.TRUE., 2, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C        IF(ANS(1:2) .EQ. 'SU') THEN
C          CALL PSNBOO(.FALSE., 2, DISPL//'.SURRN', ERRHND)
C          GOTO 99
C        ENDIF
C
C      ELSE
C
      ISEG = 0
      DO 99 I = 1,NINTEG
        COUNT = .TRUE.
        DO 3 WHILE(COUNT)
        ISEG = ISEG + 1
        IF ( ISEG .GE. NSEGS) COUNT = .FALSE.
        IF ( SEGINF(1,ISEG) .EQ. ILIST(I) ) COUNT = .FALSE.
    3 CONTINUE
      CALL KBLDN(SEGINF(6,ISEG), SSTR)
      SEGN = 'R' // SSTR(1:3)
C
      CALL PCONN(SEGN//'.SURRN"', 1, 1, 'CONS"', ERRHND)
      CALL PCONN('CONS"', 1, 1, 'TURNONDISPLAY', ERRHND)
C
      IF(ANS(1:2) .EQ. 'TO') THEN
        CALL PSNFIX(0, 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'BK') THEN
        CALL PSNFIX(3, 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'HI') THEN
        CALL PSNFIX(4, 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'SH') THEN
        CALL PSNFIX(7, 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'UN') THEN
        CALL PSNBOO(.FALSE., 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'RE') THEN
        CALL PSNBOO(.TRUE., 1, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'SO') THEN
        CALL PSNBOO(.TRUE., 2, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
      IF(ANS(1:2) .EQ. 'SU') THEN
        CALL PSNBOO(.FALSE., 2, SEGN//'.SURRN', ERRHND)
        GOTO 98
      ENDIF
C
   98 CONTINUE
C          CALL PPURGE(ERRHND)
C          CALL PSNFIX(0, 1, 'TURNONDISPLAY', ERRHND)
      CALL PPURGE(ERRHND)
C
   99 CONTINUE
C      ENDIF
C
      CALL PPURGE(ERRHND)
C
  999 RETURN
      END
