      INTEGER FUNCTION RUN_NUMBER_ZONE(RUN_LIMITS,RUN_NUMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  :  RUN_LIMITS integer(2,M)
C-              RUN_NUMBER
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-FEB-1994   Alain PLUQUET
C-   Updated  29-SEP-1994   Alain PLUQUET  Increased number of run zones. 
C-   Updated   4-OCT-1994   Alain PLUQUET  Add run_number argument 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER M,RUN_NUMBER,I
      PARAMETER (M=200)
      INTEGER RUN_LIMITS(2,M)
      LOGICAL FOUND
      FOUND=.FALSE.
      RUN_NUMBER_ZONE=1
      I=1
      DO WHILE (.NOT.FOUND.AND.I.LE.M)
        FOUND=RUN_NUMBER.GE.RUN_LIMITS(1,I)
     &   .AND.RUN_NUMBER.LE.RUN_LIMITS(2,I)
        IF (FOUND) RUN_NUMBER_ZONE=I
        I=I+1
      ENDDO
      END
