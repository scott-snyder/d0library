      SUBROUTINE JBUILD(MATRIX, TRCODE, P1, P2, P3, P4, P5, P6, P7)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module adds to the modeling transformation matrix (MATRIX)
CD   the additional operations according to the code value (TRCODE) and
CD   parameters (P1-P7). The parameter MATRIX is assumed to be a 4x4
CD   real matrix; TRCODE is an integer parameter designated the type of
CD   matrix to be created with valid values of 1..11; P1-P1 are real
CD   values used in building the modeling transformation matrix.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-OCT-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      REAL MATRIX(4,4)
      INTEGER TRCODE
      REAL P1, P2, P3, P4, P5, P6, P7
      INTEGER I, J, IJMP, TNMAT
      REAL A, B, TMAT(4,4), WMAT(4,4), CNV, ANG, ANGD
      DATA CNV/0.017453292/
C
      IF (TRCODE .LT. 1 .OR. TRCODE .GT. 11) THEN
        CALL ERROR('JTRANS: TRCODE OUT OF RANGE (1..11)')
      ENDIF
      CALL KMTID(TMAT)
      TNMAT = INT(MATRIX(4,1))
      MATRIX(4,1) = 0.0
      NTRN(TNMAT)  = NTRN(TNMAT) + 1
      MODACT(1,NTRN(TNMAT),TNMAT) = FLOAT(TRCODE)
      MODACT(2,NTRN(TNMAT),TNMAT) = P1
      MODACT(3,NTRN(TNMAT),TNMAT) = P2
      MODACT(4,NTRN(TNMAT),TNMAT) = P3
      MODACT(5,NTRN(TNMAT),TNMAT) = P4
      MODACT(6,NTRN(TNMAT),TNMAT) = P5
      MODACT(7,NTRN(TNMAT),TNMAT) = P6
      MODACT(8,NTRN(TNMAT),TNMAT) = P7
C
      CALL KCALMX(TMAT, TRCODE, P1, P2, P3, P4, P5, P6, P7)
C
      CALL KMMUL(TMAT, MATRIX, MATRIX)
C
      MATRIX(4,1) = FLOAT(TNMAT)
C
  999 RETURN
      END
