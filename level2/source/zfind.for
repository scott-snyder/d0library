      SUBROUTINE ZFIND(TIM,DLHIT,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find Z values corresponding to DL times
C-
C-   Inputs  : TIM(DL,N/S)     times for 16 DL ends DL=1..8, N/S=1 for N
C-   Outputs : DLHIT,Z(8)      DLHIT(I)=1 if DLI has hit, 0 if not
C-   Controls: 
C-
C-   Created  22-APR-1991   Jim Cochran
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,DLHIT(8)
      REAL TIM(8,2),Z(8)
C----------------------------------------------------------------------
      DO I=1,8
        DLHIT(I)=0
        Z(I)=0.
      ENDDO
C
      DO I=1,8
        DLHIT(I)=1
        IF (TIM(I,1) .EQ. 0. .OR. TIM(I,2) .EQ. 0.) THEN
          DLHIT(I)=0
        ELSEIF (TIM(I,1) .NE. 0. .AND. TIM(I,2) .NE. 0.) THEN
          Z(I)=0.5*(TIM(I,1)+TIM(I,2))
          IF (ABS(Z(I)).GT.100.) DLHIT(I) = 0
        ENDIF
      ENDDO
  999 RETURN
      END
