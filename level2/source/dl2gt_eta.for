      SUBROUTINE DL2GT_ETA(SCTL0,SCTL1,SCTL2,SCTL3,THETA,ZVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : SECTOR(0:3)
C-   Outputs : THETA,ZVTX,CHISQ
C-   Controls: 
C-
C-   Created  22-APR-1991   Jim Cochran
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SCT(0:3),DLHIT(8)
      INTEGER SCTL0,SCTL1,SCTL2,SCTL3
      REAL TIM(8,2),Z(8),THETA,ZVTX
C----------------------------------------------------------------------
      SCT(0) = SCTL0
      SCT(1) = SCTL1
      SCT(2) = SCTL2
      SCT(3) = SCTL3
C
      CALL DL2TIM(SCT,TIM)
      CALL ZFIND(TIM,DLHIT,Z)
      CALL DL2_RZTRK(DLHIT,Z,ZVTX,THETA)
      IF (ABS(ZVTX).GT.150.) THEN
        ZVTX = 999.
        THETA = 999.
      ENDIF
C
  999 RETURN
      END
