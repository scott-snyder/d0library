      SUBROUTINE MNMBGE( IBMOD, NMOD, IMOD )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Guess hit position from B-layer hit
C_                         for scint hit
C-
C-   Inputs  : IBMOD : B-layer module ID
C-   Outputs : NMOD  : number of C-layre module
C-             IMOD  : C-layer module ID
C-   Controls: 
C-
C-   Created  23-FEB-1994   Atsushi Taketani
C-   Revised  31-OCT-1994   Tao Hu :  add octant 4 and 7 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IBMOD, NMOD, IMOD(*)
C
      INTEGER  SMODL(7,8), SMODL2(7,8)
      INTEGER  ICMOD, K1,K2, N1, N2
      DATA   SMODL /  0,  0,  0,  0,  0,  0, 0,
     1                0,207,217,227,237,247, 0,
     2                0,200,210,220,230,240, 0,
     3                0,201,211,221,231,241, 0,
     4                0,202,212,222,232,242, 0,
     5                0,203,213,223,233,243, 0,
     6                0,204,214,224,234,244, 0,
     7                0,  0,  0,  0,  0,  0, 0  /
      DATA   SMODL2/  0,206,216,226,236,246, 0,
     1                0,207,217,227,237,247, 0,
     2                0,200,210,220,230,240, 0,
     3                0,201,211,221,231,241, 0,
     4                0,202,212,222,232,242, 0,
     5                0,203,213,223,233,243, 0,
     6                0,204,214,224,234,244, 0,
     7                0,205,215,225,235,245, 0  /
C----------------------------------------------------------------------
C
      ICMOD = IBMOD + 100
      NMOD = 0
C
      DO 100 K1=2,8
      DO 100 K2=1,8
        IF ( SMODL2(K1,K2).EQ.ICMOD ) GOTO 200
  100 CONTINUE
      GOTO 999
C
  200 CONTINUE
      IF ( SMODL(K1,K2).NE.0 ) THEN
        NMOD = NMOD + 1
        IMOD(NMOD) = ICMOD
      END IF
C
      DO 210 N1=-1,1
      DO 210 N2=-1,1
        IF ( N1.NE.0.OR.N2.NE.0 ) THEN
          IF ( K1+N1.GE.1.AND.K2+N2.GE.1 ) THEN
            IF ( SMODL(K1+N1,K2+N2).NE.0 ) THEN
              NMOD = NMOD + 1
              IMOD(NMOD) =  SMODL(K1+N1,K2+N2)
            END IF
          END IF
        END IF
  210 CONTINUE
C
  999 RETURN
      END
