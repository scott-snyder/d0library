      SUBROUTINE J_CROSS(X,Y,Z)
C  FORM THE CROSS PRODUCT Z=X X Y
      REAL X(3),Y(3),Z(3)
      Z(1)=(X(2)*Y(3)-X(3)*Y(2))
      Z(2)=(X(3)*Y(1)-X(1)*Y(3))
      Z(3)=(X(1)*Y(2)-X(2)*Y(1))
      END
