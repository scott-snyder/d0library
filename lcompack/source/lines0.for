      SUBROUTINE LINES0(MENLIN,MAXPOS,NUMCOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output list of available menu items in LINE mode
C-
C-   Inputs  : MENLIN: Array of menuitems
C-             MAXPOS: Maximum number of menuitems at this level
C-             NUMCOL: Number of columns in display
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUMCOL,MAXPOS,MAXI
      CHARACTER*(*) MENLIN(1:MAXPOS)
      CHARACTER*80 MSGLIN
      CHARACTER*40 BLANKS
      INTEGER ISTAT,I,J,K,M,L
      DATA BLANKS/' '/
C----------------------------------------------------------------------
      MAXI=MAXPOS-MOD(MAXPOS,NUMCOL)
      M=79/NUMCOL-6
      L=MAX0(M-LEN(MENLIN(1)),0)/2
      CALL OUTMSG(' ')
      DO I=1,MAXI,NUMCOL                               ! Do the complete lines
        IF(L.GT.0) THEN                               ! Put in blanks
          WRITE(MSGLIN,981) (BLANKS(1:L),I+J,MENLIN(I+J)
     *           (1:MIN0(M,LEN(MENLIN(I+J)))),J=0,NUMCOL-1)
  981     FORMAT(' ',10(A,I4,': ',A:))
        ELSE
          WRITE(MSGLIN,982) (I+J,MENLIN(I+J)
     *           (1:MIN0(M,LEN(MENLIN(I+J)))),J=0,NUMCOL-1)
  982     FORMAT(' ',10(I4,': ',A:))
        ENDIF
        CALL OUTMSG(MSGLIN)
      ENDDO
      K=MOD(MAXPOS,NUMCOL)
      IF(K.GT.0) THEN                                   ! Do incomplete line if needed
        IF(L.GT.0) THEN
          WRITE(MSGLIN,981) (BLANKS(1:L),MAXI+J,MENLIN(MAXI+J)
     *           (1:MIN0(M,LEN(MENLIN(MAXI+J)))),J=1,K)
        ELSE
          WRITE(MSGLIN,982) (MAXI+J,MENLIN(MAXI+J)
     *           (1:MIN0(M,LEN(MENLIN(MAXI+J)))),J=1,K)
        ENDIF
        CALL OUTMSG(MSGLIN)
      ENDIF
      RETURN
      END
