      SUBROUTINE NEURAL_FILL(IDN_OUT,IPAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill output ntuple
C-
C-   Inputs  : IDN_OUT  [I] ID of output ntuple
C-             IPAT     [I] Pattern number
C-   Outputs : 
C-   Controls: 
C-
C-   Created  11-MAR-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDN_OUT, IPAT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER I, J
      REAL    XT(MAXIN)
C----------------------------------------------------------------------
C
      CALL UCOPY(PATTERN_IN(1,IPAT),XT(1),NFIELDS)
      J = NFIELDS
      DO I = 1, NOUTPUTS
        J = J + 1
        XT(J) = PATTERN_OUT(I,IPAT)   ! Desired output
        J = J + 1
        XT(J) = OUT(I)                ! Actual output
      END DO
C
      CALL HCDIR('//OUTPUT',' ')
      CALL HCDIR('//PAWC',' ')
      CALL HFN(IDN_OUT,XT)
  999 RETURN
      END
