      SUBROUTINE MUJUNK(INOUT,I1,I2,I3,I4,F1,F2,F3,F4)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : saves 4 floating point and 4 integer values
C-   currently, I1 and I2 will be used for the run number and a
C    32 bit word for the Date/Time                      
C-   Inputs  : 
C-   Outputs : 
C-   Controls: INOUT = 0 input, =1 output
C-
C-   Created   1-APR-1992   David Hedin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I1,I2,I3,I4,INOUT,J1,J2,J3,J4
      REAL F1,F2,F3,F4,G1,G2,G3,G4
      DATA J1,J2,J3,J4,G1,G2,G3,G4/4*0,4*0./
      IF(INOUT.EQ.0) THEN    ! INPUT
        J1=I1
        J2=I2
        J3=I3
        J4=I4
        G1=F1
        G2=F2
        G3=F3
        G4=F4
      ELSE IF(INOUT.EQ.1) THEN  ! OUTPUT
        I1=J1
        I2=J2
        I3=J3
        I4=J4
        F1=G1
        F2=G2
        F3=G3
        F4=G4
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
