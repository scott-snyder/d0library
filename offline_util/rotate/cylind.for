      REAL FUNCTION CYLIND(X, I, ICSYS)
C------------------------------------------------------------------------
C
C     GIVES CYLINDERICAL VARIABLE FROM VARIABLES IN VARIOUS COORD SYSTEMS.
C     INPUT:    X       POSITION VECTOR
C               I       CYLINDERICAL VECTOR ELEMENT INDEX
C               ICSYS   COORD SYSTEM FLAG
C     
C     OUTPUT:   CYLIND  CYLINDERICAL VECTOR ELEMENT
C
C     AUTHOR:   S KAHN         16 JAN 1988
C------------------------------------------------------------------------
      IMPLICIT NONE
      REAL X(3)
      INTEGER I, ICSYS
C
      IF(ICSYS .EQ. 345) THEN         ! cylindrical system
        CYLIND = X(I)
      ELSE IF(ICSYS .EQ. 123) THEN    ! cartesian system
        IF(I .EQ. 1) THEN
           CYLIND = SQRT(X(1)**2+X(2)**2)  ! R
        ELSE IF(I .EQ. 2) THEN
           CYLIND = ATAN2(X(2),X(1))  ! phi
        ELSE IF(I .EQ. 3) THEN
           CYLIND = X(3)              ! Z
        ELSE
           WRITE (6,*) ' CYLIND: BAD INDEX ', I
           STOP 123
        END IF
      ELSE
        WRITE (6,*) ' CYLIND: COORD SYST NOT CODED ', ICSYS
        STOP 124
      END IF
C
      RETURN
      END

