      INTEGER FUNCTION GET_CAPH_ALG(LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get current jet algorithm for the specified
C-                         jet.
C-
C-   Returned value:  Algorithm - 1,2,3,4 = .7 cone, .5 cond, .3 cone, NN
C-                                0 = none/unknown/other.
C-   Inputs:    LJETS - Link of JETS bank.
C-
C-   Created  30-Dec-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INTEGER LJETS, LCAPH
      INTEGER CAPH_ALG
      REAL JET_CONE
C----------------------------------------------------------------------
      LCAPH = 0
      IF(LJETS.NE.0)LCAPH = LQ(LJETS+1)
      IF(LCAPH.EQ.0)THEN
        GET_CAPH_ALG = 0
        GO TO 999
      ENDIF
      CAPH_ALG = IQ(LCAPH+4)
      IF(CAPH_ALG .EQ. A_CONE_JET)THEN
      JET_CONE = Q(LCAPH+6)
        IF(ABS(JET_CONE-0.7).LT.0.01)THEN
          GET_CAPH_ALG = 1
        ELSEIF(ABS(JET_CONE-0.5).LT.0.01)THEN
          GET_CAPH_ALG = 2
        ELSEIF(ABS(JET_CONE-0.3).LT.0.01)THEN
          GET_CAPH_ALG = 3
        ENDIF
      ELSEIF(IQ(LCAPH+4).EQ.A_NN_JET)THEN
        GET_CAPH_ALG = 4
      ELSE
        GET_CAPH_ALG = 0
      ENDIF
 999  RETURN
      END
