      SUBROUTINE Q_WRITE_HEADINGS(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      formatting for Q_SOLVE outputs
C-   Inputs  : LUN,P,B,R,DEAD_FRAC
C-   Outputs :
C-   Controls: EXACT  IF .TRUE. use left column
C-
C-   Created  29-AUG-1995   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,P,B,PEX,BEX
      REAL    R,DEAD_FRAC,REX,DEX
      LOGICAL EXACT
C----------------------------------------------------------------------
      WRITE (LUN,100)
  100 FORMAT('              Exact',26X,'    Approximate'/
     &  2('    P    B      R     u %dead R thruput'))
      RETURN
      ENTRY Q_WRITE_SOLN(LUN,P,B,R,DEAD_FRAC,EXACT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-AUG-1995   James T. Linnemann
C-
C----------------------------------------------------------------------
      IF(EXACT) THEN
        PEX = P !save values from exact solution
        BEX = B
        REX = R
        DEX = DEAD_FRAC
      ELSE
        WRITE (LUN,300)   !write out when get approx solutions
     &        PEX,BEX,REX,PEX*REX,100.*DEX,R*(1-DEX),
     &        P,B,R,P*R,100.*DEAD_FRAC,R*(1-DEAD_FRAC)
  300   FORMAT(1X,2(I4,I5,F7.4,2F6.2,F10.4,1X))
      ENDIF
  999 RETURN
      END
