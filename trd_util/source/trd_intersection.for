      SUBROUTINE TRD_INTERSECTION (VIN,THETA,PHI,HIT)
C----------------------------------------------------------------------
C-   Purpose and methods :    determination of intersection (yes or no)
C-                            TRD/track
C-   Inputs  : VIN    real(6) X,Y,Z and director cosines of the track
C-             ETA,PHI  real   eta and phi of the track, not used,
C-                             kept for compatibility
C-   Outputs : HIT LOGICAL(3) HIT(I)=TRUE <=>    intersection layer i/track
C-                            HIT(I)=FALSE<=> no intersection layer i/track
C-   Controls: none
C-
C-   Created  12-MAY-1993     Alain PLUQUET
C-   Updated  12-MAR-1994   A. Zylberstejn   : Change input arguments
C-   Updated  14-JUN-1995   A. Zylberstejn   : Keep z,theta, phi in trd_phi_z
C-   Updated  22-JUN-1995   A. Zylberstejn   Reduce Z active region to 80cm
C-   Updated  29-JAN-1996   A. ZYLBERSTEJN   go back to full Z active region
C-   Updated  30-JAN-1996   L. T. Goss   set ZMIN to 82.0 (advice of B. Kehoe)
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ZMIN,VIN(6),VOUT(6),ETA,PHI,POINT(3),RADIUS(3),THETA,ST
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INTEGER LAYER,IREJ
      LOGICAL HIT(3)
      DATA ZMIN/82.0/
      DATA RADIUS/26.3,36.85,47.4/
      DO LAYER=1,3
C        THETA=ACOS(TANH(ETA))
C        VIN(1)=POINT(1)
C        VIN(2)=POINT(2)
C        VIN(3)=POINT(3)
C        ST=SIN(THETA)
C        VIN(4)=COS(PHI)*ST
C        VIN(5)=SIN(PHI)*ST
C        VIN(6)=COS(THETA)
        CALL EXTCYL(VIN,VOUT,RADIUS(LAYER),IREJ)
        HIT(LAYER)=IREJ.EQ.0.AND.ABS(VOUT(3)).LE.ZMIN
        PHI_TRD(LAYER)=ATAN2(-VOUT(2),-VOUT(1))+3.14159361
        THETA_TRD(LAYER)=THETA
        R_TRD(LAYER)=RADIUS(LAYER)
        Z_TRD(LAYER)=VOUT(3)
      ENDDO
      END
