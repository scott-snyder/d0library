      SUBROUTINE REPSCR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Repair screen image line-by-line
C-
C-   Inputs  : 
C-   Outputs : None
C-
C-   Created   2-DEC-1987   Jan S. Hoftun
C-   Updated  24-OCT-1988   Jan S. Hoftun  (Added CURFLG) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LIBPUT,LIBBIG,I,ISTAT,TRULEN,J
C----------------------------------------------------------------------
      IF(FULSCR) THEN
        ISTAT=LIBBIG(LINBUF(1),1,1,3)
        IF(MOD(ISTAT,2).EQ.0) THEN
          CALL MSGSCR(ISTAT,'LIBBIG-->')
          GOTO 999
        ENDIF
        DO I=2,MIN(PBROWS-1,NUMSAV)
          ISTAT=LIBPUT(LINBUF(I),I,1,0)
          IF(MOD(ISTAT,2).EQ.0) THEN
            CALL MSGSCR(ISTAT,'LIBPUT-->')
            GOTO 999
          ENDIF
        ENDDO
        CALL PFLABL(PFLINE(1),PFLINE(2),PFLINE(3),PFLINE(4))
        IF(CURFLG) THEN
          CALL CURONF(1)   !Turn off cursor again
        ENDIF
      ELSE
        J=MAX(NUMSAV-PBROWS,0)+1
        DO I=J,NUMSAV
          ISTAT=LIBPUT(LINBUF(I)(1:TRULEN(LINBUF(I))),I-J+1,1,0)
          IF(MOD(ISTAT,2).EQ.0) THEN
            CALL MSGSCR(ISTAT,'LIBPUT-->')
            GOTO 999
          ENDIF
        ENDDO
      ENDIF
  999 CONTINUE
      RETURN
      END
