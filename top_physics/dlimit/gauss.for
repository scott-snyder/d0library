      REAL FUNCTION GAUSS(CEN,SIG,GMIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GENERATE A GAUSSIAN WITH CENTRAL VALUE AND SIGMA
C-
C-   Returned value  : GAUSSIAN DISTRIBUTED VALUE. 
C-   Inputs  : CEN=CENTRAL VALUE
C-             SIG= STD DEVIATION
C-             GMIN = SMALLEST VALUE FOR GAUSSIAN
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-FEB-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    R1,R2
      REAL    CEN,SIG
      REAL    GMIN
C----------------------------------------------------------------------
1     CALL RANNOR(R1,R2)
      GAUSS = CEN + SIG*R1
      IF ( GAUSS.LT.GMIN ) THEN
        GO TO 1
      ENDIF
  999 RETURN
      END
