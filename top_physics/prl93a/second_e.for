      SUBROUTINE SECOND_E(LSEL,ET,PHI,THETA,P)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     find a 2nd e (gamma) candidate
C-   Inputs  : 
C-      LSEL = pointer to first candidate
C-   Outputs : 
C-        ET, PHI, THETA, P(4)
C-   Controls: 
C-
C-   Created  22-DEC-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSEL
      REAL    ET,PHI,THETA,P(4)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZPPHO,GZPELC
      INTEGER L_EL_OR_PH,I,NTRY
      LOGICAL GOOD_PH
C----------------------------------------------------------------------
      ET=0
      PHI=0
      THETA=0
      DO I=1,4
        P(I)=0
      ENDDO
      L_EL_OR_PH=GZPELC()
      NTRY=1
    1 CONTINUE
      DO WHILE (L_EL_OR_PH.GT.0)
        IF(Q(L_EL_OR_PH+7).GT.ET.AND.GOOD_PH(L_EL_OR_PH)
     &      .AND.L_EL_OR_PH.NE.LSEL) THEN
          ET=Q(L_EL_OR_PH+7)
          THETA=Q(L_EL_OR_PH+8)
          PHI=Q(L_EL_OR_PH+10)
          CALL UCOPY(Q(L_EL_OR_PH+3),P,4)
        ENDIF
        L_EL_OR_PH=LQ(L_EL_OR_PH)
      ENDDO
      IF(NTRY.EQ.2) GOTO 999
      L_EL_OR_PH=GZPPHO()
      NTRY=2
      GOTO 1
  999 RETURN
      END
