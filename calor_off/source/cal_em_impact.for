      SUBROUTINE CAL_EM_IMPACT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Works out the point of Impact of Isajet Track
C-                         along a plane normal to the EM3 vector.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-AUG-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:DEAD_MATERIALS.PARAMS'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      REAL    XX,YY,ZZ
      REAL    VRT_EM3,UVEC_EM3
      INTEGER IOK,I
C----------------------------------------------------------------------
      DEM32 = 0.
      VRT_EM3 = 0.
      UVEC_EM3 = 0.
C
      EM3AV(1) = Q(LCACL+14)
      EM3AV(2) = Q(LCACL+15)
      EM3AV(3) = Q(LCACL+16)            ! CENTER OF SHOWER.
C
      DO I = 1,3
        DEM32 = DEM32 + EM3AV(I)*EM3AV(I)       ! MAGNITUDE SQUARED.
        VRT_EM3 = VRT_EM3 + EM3AV(I)*VERT(I)
        UVEC_EM3 = UVEC_EM3 + EM3AV(I)*UVEC(I,1)
      ENDDO
C
      ALAM = (DEM32 - VRT_EM3)/UVEC_EM3

      DO I = 1 , 3
        RIMPACT(I) = VERT(I) + ALAM*UVEC(I,1)
        DEL_IMPACT(I) = RIMPACT(I) - EM3AV(I)
      ENDDO
C
  999 RETURN
      END
