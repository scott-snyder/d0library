      SUBROUTINE MOVE_ON_LINEAR_CHAIN(L,N,LOUT,I,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : move along a zebra linear chain in either direction
C-          Movement stops when any error condition is encountered.
C-          Unless the input link was invalid, the output bank is always valid
C-
C-   Inputs  : L  link to a current member of a linear chain
C-             N  number of steps to move: 
C-                  + is along "NEXT" link direction, away from support bank
C-                  - is opposite to "NEXT" links, towards support bank
C-   Outputs : LOUT link to the bank at which movement stopped
C-             I  number of banks actually moved
C-             IER  = 0 if all OK
C-                    -1 if hit end of chain before all N steps (I .NE. N)
C-                        (hit support bank (- N) or end of chain (+ N)
C-                    -2 if got a zero link during a - shift: hit end of chain
C-                        (valid only if linear chain is a standalone bank)
C-                     1 original bank not found
C-                     2 if bank name changed during chaining
C-                     3 if NEXT link failed to point back to previous bank 
C-                          during - N chaining (invalid link structure)
C-   Controls: none
C-
C-   Created   4-JUN-1993   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBQ.INC'       !for all-store zebra common
      INCLUDE 'D0$INC:MZCB.INC'       !for current-store offset KQS
      INTEGER L,N,LOUT,I,IER
      INTEGER J,LNEW,LNK,LSUP,NSTRL_SUP,BNAME
      LOGICAL IN_SUPPORT_BANK
C----------------------------------------------------------------------
C...Statement function to tell if a link is inside the support bank
C...    LSUP and NSTRL must be defined before use: parameters of support bank
      IN_SUPPORT_BANK(LNK) = (LNK.LT.LSUP).AND.(LNK.GE.(LSUP-NSTRL_SUP))
C----------------------------------------------------------------------
      BNAME = IQQ(L+KQS-4)  !Bank name in Hollerith
      LOUT = L
      I = 0
      IER = 1
      IF (L.EQ.0) GO TO 999
      IF (N.GT.0) THEN  !Movement along NEXT link Direction
        DO J = 1 , N
          LNEW = LQQ(L+KQS) !Follow NEXT link
          IER = -1
          IF (LNEW.LE.0) GO TO 999  !Hit end of linear chain
          IER = 2
          IF (IQQ(LNEW+KQS-4).NE.BNAME) GO TO 999  !bank name changed
          LOUT = LNEW !new bank is OK
          I = I + 1
        ENDDO
      ELSE    !move opposite to linear chain
C
C...define region in which ORIGIN link can point in support bank
        LSUP = LQQ(L+KQS+1) !support bank
        NSTRL_SUP = IQQ(LSUP+KQS-2) !number of structural links in support bank
C
C...move opposite to NEXT links by following ORIGIN links
        DO J  = -1 ,N, -1
          LNEW = LQQ(L+KQS+2)
          IER = -2
          IF (LNEW.EQ.0) GO TO 999  !no bank at origin link
          IER = -1  
          IF (IN_SUPPORT_BANK(LNEW)) GO TO 999  !hit end of linear chain
          IER = 3
          IF (LQQ(LNEW+KQS).NE.LOUT) GO TO 999  !NEXT link didn't point back
          IER = 2
          IF (IQQ(LNEW+KQS-4).NE.BNAME) GO TO 999 !Bank name changed
          LOUT = LNEW
          I = I - 1
        ENDDO
      ENDIF
      IER = -1
      IF (I.EQ.N) IER = 0
  999 RETURN
      END
