      SUBROUTINE CELXYZ_FAST(IETA,IPHI,ILYR,X,Y,Z,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO USE AN ARRAY TO UNPACK CELXYZ FASTER
C-
C-   Inputs  : IETA, IPHI, ILYR CALORIMETER PHYSICS ADDRESS 
C-   Outputs : X,Y,Z FOR CENTER OF CELL AND IER=0 IF OK
C-   Controls: NONE
C-
C-   Created  14-DEC-1990   Chip Stewart
C-   Updated   8-SEP-1994   Meenakshi Narain  skip loop over ieta=0 
C-   Updated  30-SEP-1994   Meenakshi Narain  
C-                            Check if the cell exists
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA,IPHI,ILYR,I,J,K,IER
      CHARACTER*12 TMP(-NETAL:NETAL,1:NPHIL,1:NLYRL),TMP1
      CHARACTER*4  TMP2(3)
      REAL*4 X1,Y1,Z1
      REAL X,Y,Z
      EQUIVALENCE (X1,TMP2(1)), (Y1,TMP2(2)), (Z1,TMP2(3))
      EQUIVALENCE (TMP1,TMP2(1))
      LOGICAL CEXIST, CELL_EXISTS
      LOGICAL FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (.NOT. FIRST) GOTO 100
      FIRST = .FALSE.
      DO I = -NETAL,NETAL
        IF (I.EQ.0) GOTO 50
        DO J = 1,NPHIL
          DO K = 1, NLYRL
            IF(CEXIST(I,J,K)) THEN
              CALL CELXYZ(I,J,K,X,Y,Z,IER)
              X1 = X
              Y1 = Y
              Z1 = Z
              TMP(I,J,K) = TMP1
            END IF
          END DO
        END DO
   50   CONTINUE
      END DO
  100 CONTINUE
      IER = 0
      CELL_EXISTS = CEXIST(IETA,IPHI,ILYR)
      IF (.NOT.CELL_EXISTS) GOTO 200
      IF(IPHI.GT.NPHIL) GOTO 200
      IF(IPHI.LT.1)     GOTO 200
      IF(ILYR.LT.1)     GOTO 200
      IF(ILYR.GT.NLYRL) GOTO 200
      IF(IETA.GT.NETAL) GOTO 200
      IF(IETA.LT.-NETAL)GOTO 200
      TMP1 = TMP(IETA,IPHI,ILYR)
      X = X1
      Y = Y1
      Z = Z1
  999 RETURN
  200 IER = -1
      END
