      REAL FUNCTION CARTES(X, I, ICSYS)
C------------------------------------------------------------------------
C
C     GIVES CARTESIAN VARIABLE FROM VARIABLES IN VARIOUS COORD SYSTEMS.
C     INPUT:    X       POSITION VECTOR
C               I       CARTESION VECTOR ELEMENT INDEX
C               ICSYS   COORD SYSTEM FLAG
C     
C     OUTPUT:   CARTES  CARTESIAN VECTOR ELEMENT
C
C     AUTHOR:   S KAHN         30 JULY 1987
C------------------------------------------------------------------------
      IMPLICIT NONE
      REAL X(3)
      INTEGER I, ICSYS
C
      IF(ICSYS .EQ. 123) THEN         ! cartesian system
        CARTES = X(I)
      ELSE IF(ICSYS .EQ. 345) THEN    ! cylindrical system
        IF(I .EQ. 1) THEN
           CARTES = X(1)*COS(X(2))    ! X
        ELSE IF(I .EQ. 2) THEN
           CARTES = X(1)*SIN(X(2))    ! Y
        ELSE IF(I .EQ. 3) THEN
           CARTES = X(3)              ! Z
        ELSE
           WRITE (6,*) ' CARTES: BAD INDEX ', I
           STOP 127
        END IF
      ELSE
        WRITE (6,*) ' CARTES: COORD SYST NOT CODED ', ICSYS
        STOP 128
      END IF
C
      RETURN
      END

