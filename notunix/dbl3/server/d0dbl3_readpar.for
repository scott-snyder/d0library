      SUBROUTINE D0DBL3_READPAR (IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To read from parameter file record size
C-                         and wakeup time intervals.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IERR    If not 0 then trouble
C-
C-   Created   18-JUN-1992   SHAHRIAR ABACHI
C-   moodified 12-NOV-1993   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER IERR,IER,LUNPAR,NW,LIB$WAIT,I
C
      IERR = 0
      NW = 0
      DO I=1,2
      TOPN(I) = ' '
      ENDDO
      CALL GTUNIT(171,LUNPAR,IER)
   10 OPEN (UNIT=LUNPAR,FILE='DBPARAM',ACCESS='SEQUENTIAL'
     &             ,FORM='FORMATTED',STATUS='OLD',ERR=100)
      READ(LUNPAR,*) ISTOP
      READ(LUNPAR,9000) TOPN(1)
      TOPN(2) = TOPN(1)
      TOPN(2)(2:2) = 'Z'
      READ(LUNPAR,9000) DTIME
      READ(LUNPAR,*) TSEC
      READ(LUNPAR,9000) CHOPJ
      READ(LUNPAR,*) NCLOS
      READ(LUNPAR,*) NFRSH
      READ(LUNPAR,*) IONLINE
      READ(LUNPAR,*) ISHIP
      READ(LUNPAR,*) ICOMP
 9000 FORMAT(A)
      CLOSE (UNIT=LUNPAR)
      CALL RLUNIT(171,LUNPAR,IER)
      GOTO 999
C
  100 CONTINUE
      NW = NW + 1
      IF(NW .LT. 4) THEN
        CALL LIB$WAIT(10.0)
        GOTO 10
      ENDIF
      IERR = 1
C
  999 RETURN
      END
