      SUBROUTINE CLOSE_DIST(RCENT,VERT,UVEC,DCL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the distance of closest approach
C-                         of a track emanating from the vertex VERT with 
C-                         directions UVEC to a point RCENT
C-
C-   Inputs  : RCENT,VERT,UVEC
C-   Outputs : DCL = distance of closest approach.
C-   Controls: 
C-
C-   Created  30-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      REAL    RCENT(3),VERT(3),UVEC(3),DCL,ALAM,RDIFF(3),DCLOSE(3)
C----------------------------------------------------------------------
      DO I = 1 , 3
        RDIFF(I) = RCENT(I) - VERT(I)
      ENDDO
C
      ALAM = RDIFF(1)*UVEC(1)+RDIFF(2)*UVEC(2)+RDIFF(3)*UVEC(3)
C
      DO I = 1 , 3
      DCLOSE(I) = RDIFF(I) - ALAM*UVEC(I)
      ENDDO
C
C ****  DCLOSE is the vector of closest approach.
C
      DCL = SQRT(DCLOSE(1)**2+DCLOSE(2)**2+DCLOSE(3)**2)
  999 RETURN
      END
