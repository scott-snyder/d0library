      SUBROUTINE SAVSCR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save screen image line-by-line
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created   2-DEC-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER LINGET,I,ISTAT,IBLANK
      LOGICAL LBLANK
C----------------------------------------------------------------------
      IF(FULSCR) THEN
        CALL PFGET(PFLINE)
        NUMSAV=PBROWS-1
      ELSE
        NUMSAV=PBROWS
      ENDIF
      LBLANK=.FALSE.
      IBLANK=0
      DO I=1,NUMSAV
        ISTAT=LINGET(I,LINBUF(I))
        IF(MOD(ISTAT,2).EQ.0) THEN
          CALL MSGSCR(ISTAT,'SAVSCR-->')
          GOTO 999
        ENDIF
        IF(.NOT.FULSCR) THEN
          IF(LBLANK) THEN
            IF(LINBUF(I).EQ.' ') THEN
              IBLANK=IBLANK+1
              IF(IBLANK.GT.3) THEN
                NUMSAV=I-3
                GOTO 999
              ENDIF
            ELSE
              IBLANK=0
            ENDIF
          ELSE
            IF(LINBUF(I).EQ.' ') THEN
              LBLANK=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDDO
  999 CONTINUE
      RETURN
      END
