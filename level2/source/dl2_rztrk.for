      SUBROUTINE DL2_RZTRK(DLHIT,Z,ZVTX,THETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find best RZ track (theta)
C-
C-   Inputs  : DLHIT,Z(8)         DLHIT(I)=1 if DLI has hit, 0 if not
C-   Outputs : THETA
C-   Controls:
C-
C-   Created   20-APR-1991   Jim Cochran
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  DLHIT(8),NZHITS,I
      REAL     RAD(8),R(8),Z(8),DL,THETA,ZVTX
      REAL     SUMR,SUMZ,SUMRR,SUMRZ
      DATA RAD /52.093,55.914,57.275,61.095,62.421,66.311,
     &  67.733,71.553/
C------------------------------------------------------------
C
C                               --- determine which DLs have hits
      NZHITS = 0
      DO I=1,8
        IF (DLHIT(I) .EQ. 1) THEN
          NZHITS=NZHITS+1
          R(NZHITS)=RAD(I)
          Z(NZHITS)=Z(I)
        ENDIF
      ENDDO
C
      DL=0.
      SUMRR = 0.
      SUMRZ = 0.
      SUMZ  = 0.
      SUMR  = 0.
      THETA = 999.
      ZVTX  = 999.
      IF (NZHITS .LT. 2) GOTO 999       !  bad RZ track
C
C  Least square fit of the RZ track
C
      IF (NZHITS.EQ.2) THEN
        THETA = ATAN2((R(2) - R(1)),(Z(2) - Z(1)))
        ZVTX = Z(2) - R(2)*(Z(2) - Z(1))/(R(2) - R(1))
        IF (THETA.LT.0.) THETA = 3.141593 + THETA
      ELSE
        DO I=1,NZHITS
          SUMRR = SUMRR + R(I)**2
          SUMR  = SUMR  + R(I)
          SUMZ  = SUMZ  + Z(I)
          SUMRZ = SUMRZ + R(I)*Z(I)
        END DO
C
C  Calculate Z vertex = intercept of the line and
C            Theta of the track = atan(1/slope) of the line
C
        DL=NZHITS*SUMRR-SUMR**2
        IF (DL.NE.0.) THEN
          ZVTX   = (SUMRR*SUMZ-SUMR*SUMRZ)/DL         ! intercept
          THETA  = ATAN2(DL,NZHITS*SUMRZ-SUMR*SUMZ)
          IF (THETA.LT.0.) THETA = 3.141593 + THETA
        ENDIF
      ENDIF
C
  999 RETURN
      END
