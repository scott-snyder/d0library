      SUBROUTINE GTCHOT(NTOT,ICHOT,MAXFLG,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns hot channel flag for each calorimeter chan.
C-
C-   Inputs  : none
C-   Outputs : NTOT - Total number of "hot channels"
C-             ICHOT - Array containing hot channel flag for each calorimeter
C-                       channel, indexed by eta, phi, layer.
C-             MAXFLG - set to 1, if max no. of hot channels reached (in bank)
C-   Controls: none
C-
C-   Created  26-JAN-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LCHOT,NH,NV,NTOT,IETA,IPHI,ILYR,IFLG,ICHOT(-37:37,64,17)
      INTEGER IWORD,IER,I,MAXFLG
      INTEGER GZCHOT
      BYTE IBWORD(4)
      EQUIVALENCE (IWORD,IBWORD)
C----------------------------------------------------------------------
      IER = 0
      NTOT = 0
      CALL VZERO(ICHOT,500)
C
      LCHOT = GZCHOT()
      IF (LCHOT.LE.0) THEN
        IER = -1
        GOTO 999
      ENDIF
C
      NH = IC(LCHOT+1)
      NV = IC(LCHOT+2)
      NTOT = IC(LCHOT+3)
      MAXFLG = IC(LCHOT+4)
C
      IF (NTOT.LE.0) GOTO 999
      DO I=1,NTOT
        IWORD = IC(LCHOT+NH+I)
        IETA = IBWORD(BYTE4)
        IPHI = IBWORD(BYTE3)
        ILYR = IBWORD(BYTE2)
        IFLG = IBWORD(BYTE1)
        ICHOT(IETA,IPHI,ILYR) = IFLG
      ENDDO
C
  999 RETURN
      END
