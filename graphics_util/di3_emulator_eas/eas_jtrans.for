      SUBROUTINE JTRANS(MATRIX, TRCODE, P1, P2, P3, P4, P5, P6, P7)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module initializes the modeling transformation matrix (MATRIX)
CD   to the identity matrix then sets any additional operations
CD   according to the code value (TRCODE) and parameters (P1-P7).
CD   The parameter MATRIX is assumed to be a 4x4 real matrix; TRCODE is
CD   an integer parameter designated the type of matrix to be created
CD   with valid values of 1..11; P1-P1 are real values used in building
CD   the modeling transformation matrix.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-OCT-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL MATRIX(4,4)
      INTEGER TRCODE
      REAL P1, P2, P3, P4, P5, P6, P7
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
      IF (TRCODE .LT. 1 .OR. TRCODE .GT. 11) THEN
        CALL ERROR('JTRANS: TRCODE OUT OF RANGE (1..11)')
        GOTO 999
      ENDIF
      NMAT = NMAT + 1
      NTRN(NMAT)  = 1
      MODACT(1,NTRN(NMAT),NMAT) = FLOAT(TRCODE)
      MODACT(2,NTRN(NMAT),NMAT) = P1
      MODACT(3,NTRN(NMAT),NMAT) = P2
      MODACT(4,NTRN(NMAT),NMAT) = P3
      MODACT(5,NTRN(NMAT),NMAT) = P4
      MODACT(6,NTRN(NMAT),NMAT) = P5
      MODACT(7,NTRN(NMAT),NMAT) = P6
      MODACT(8,NTRN(NMAT),NMAT) = P7
C
      CALL KCALMX(MATRIX, TRCODE, P1, P2, P3, P4, P5, P6, P7)
C
      MATRIX(4,1) = FLOAT(NMAT)
C
  999 continue
      RETURN
      END
