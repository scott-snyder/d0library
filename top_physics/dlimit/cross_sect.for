      REAL FUNCTION CROSS_SECT(MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : YIELDS THE TOP CENTRAL VALUE CROSS SECTION
C-   FOR A GIVEN MASS.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TOP_CROSS.INC'
      INCLUDE 'D0$INC:SIG_EFF.INC'
      REAL    MASS,DIVDIF
C----------------------------------------------------------------------
      IF ( IEN.EQ.18 ) THEN
        CROSS_SECT = DIVDIF(TTB_CROSS_N18,TM_N18,N18,MASS,MPOL) !AT 1.8 TEV
      ELSE
        CROSS_SECT = DIVDIF(TTB_CROSS_N20,TM_N20,N20,MASS,MPOL) !AT 2.0 TEV
      ENDIF
  999 RETURN
      END
