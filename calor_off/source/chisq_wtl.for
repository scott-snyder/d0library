      SUBROUTINE CHISQ_WTL(ENERGY,WCHSQL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : work out chisquared for 
C-                         longitudinal matrix only
C-                         using an energy weighting
C-
C-   Inputs : ENERGY of cluster
C-   Outputs  : WCHSQL = weighted longitudinal chisquared 
C-
C-   Controls: 
C-
C-   Created  25-OCT-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      INTEGER I,J
      REAL    DI,DJ,ENERGY,WCHSQL,WEIGHT
      INTEGER IDEGL,IER
      LOGICAL FIRST
C----------------------------------------------------------------------
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('CHISQUARE_WEIGHT',WEIGHT,IER)
        CALL EZRSET
      ENDIF
      WCHSQL = 0.0
      DO 300 I = 1,NDIMVL              ! Summing over visible energy.
        DI = QUANTL(I)-AVERL(I)
        DO 400 J = 1,NDIMVL
          DJ = QUANTL(J)-AVERL(J)
          WCHSQL = WCHSQL + DI*HMATRL_VIS(J,I)*DJ
  400   CONTINUE
  300 CONTINUE
      WCHSQL = WCHSQL*(ENERGY**WEIGHT)
  999 RETURN
      END
