      SUBROUTINE GETWORD(INSTR,IWORD,OUTSTR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Copy the IWORD-th word from INSTR into OUTSTR
C-      Words are delimited by one or more spaces or tabs.  Words with
C-      spaces can be delimited with ".
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) INSTR,OUTSTR,CTAB*1
      INTEGER IWORD,I,J,K,L,NWORD
      LOGICAL LSPACE,QUOTED,ISCHAR
C
      INTEGER TAB
      PARAMETER(TAB=9)
C-----------------------------------------------------------------------
      LSPACE=.TRUE.
      QUOTED=.FALSE.
      NWORD=0
      CTAB=CHAR(TAB)
      I=0
 10   CONTINUE
         IF( I.GE.LEN(INSTR) ) GOTO 999
         IF( NWORD.EQ.IWORD ) GOTO 20
         I=I+1
         ISCHAR=INSTR(I:I).NE.' ' .AND. INSTR(I:I).NE.CTAB 
         IF( ISCHAR .OR. QUOTED ) THEN
            IF( INSTR(I:I).EQ.'"' ) QUOTED=.NOT.QUOTED
            IF(LSPACE) NWORD=NWORD+1
            LSPACE=.FALSE.
         ELSE
            LSPACE=.TRUE.
         ENDIF
      GOTO 10
C
 20   CONTINUE
      QUOTED=.FALSE.
      J=I
      ISCHAR = (INSTR(J:J).NE.' '.AND.INSTR(J:J).NE.CTAB) .OR. QUOTED
      DO WHILE( J.LE.LEN(INSTR) .AND. ISCHAR )
        IF( INSTR(J:J).EQ.'"') QUOTED=.NOT.QUOTED
        J=J+1
        ISCHAR = (INSTR(J:J).NE.' '.AND.INSTR(J:J).NE.CTAB) .OR. QUOTED
      ENDDO
C
      L=1
      OUTSTR=' '
      DO K=I,J-1
        IF( INSTR(K:K).NE.'"' .AND. L.LE.LEN(OUTSTR) ) THEN
          OUTSTR(L:L)=INSTR(K:K)
          L=L+1
        ENDIF
      ENDDO
C
  999 RETURN
      END
