      SUBROUTINE TLINE0(DISLIN,MAXPOS,NUMSPA,NUMCOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a page of lines from TABDIS in LINE mode
C-
C-   Inputs  : DISLIN: Array of lines for display
C-             MAXPOS: Maximum number of lines being used
C-             NUMSPA: Line spacing in display
C-             NUMCOL: Number of columns in display
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER NUMCOL,NUMSPA,MAXPOS,MAXI
      CHARACTER*(*) DISLIN(1:MAXPOS)
      CHARACTER*256 MSGLIN
      INTEGER ISTAT,I,LOWUSE,J,TRULEN,COLUMN,COL1,MAXLEN
      COLUMN(I)=COL1+MOD(I-1,NUMCOL)*(MAXLEN+MAX(3,MAXLEN/2))
C----------------------------------------------------------------------
      MAXLEN=0
      DO I=1,MAXPOS
        J=TRULEN(DISLIN(I))
        IF(J.GT.MAXLEN) THEN
          MAXLEN=J
        ENDIF
      ENDDO
      COL1=5
      CALL OUTMSG(' ')
      MAXI=MAXPOS/NUMCOL
      DO I=1,MAXI
        LOWUSE=(I-1)*NUMCOL+1
        MSGLIN=' '
        DO J=LOWUSE,LOWUSE+NUMCOL-1
          MSGLIN(COLUMN(J):)=DISLIN(J)
        ENDDO
        CALL OUTMSG(MSGLIN)
        IF(NUMSPA.GT.1) THEN
          DO J=1,NUMSPA-1
            CALL OUTMSG(' ')
          ENDDO
        ENDIF
      ENDDO
      IF(MAXI.LT.MAXPOS) THEN
        LOWUSE=MAXI*NUMCOL+1
        MSGLIN=' '
        DO J=LOWUSE,LOWUSE+MOD(MAXPOS,NUMCOL)-1
          MSGLIN(COLUMN(J):)=DISLIN(J)
        ENDDO
        CALL OUTMSG(MSGLIN)
        IF(NUMSPA.GT.1) THEN
          DO J=1,NUMSPA-1
            CALL OUTMSG(' ')
          ENDDO
        ENDIF
      ENDIF
      RETURN
      END
