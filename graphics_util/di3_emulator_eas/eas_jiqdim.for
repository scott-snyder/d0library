      SUBROUTINE JIQDIM(DSPDEV,FLAG,MAXDIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To return the maximum dimension of the
C-                         specified display device in meters.
C-
C-   Inputs  : DSPDEV    [I] The initialized display device.
C-   Outputs : FLAG      [L] .TRUE. device is capable of sizing in
C-                           physical units.
C-                           .FALSE. device is not capable of sizing.
C-             MAXDIM(1) [I] The min. X of the device in meters.
C-             MAXDIM(2) [I] The max. X of the device in meters.
C-             MAXDIM(3) [I] The min. Y of the device in meters.
C-             MAXDIM(4) [I] The max. Y of the device in meters.
C-   Controls: 
C-
C-   Created  18-MAR-1991   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DSPDEV
      LOGICAL FLAG
      REAL    MAXDIM(4)
C----------------------------------------------------------------------
      IF (DSPDEV .EQ. 0) GO TO 999
C-
C--- Following returned values are same as those of GPV driver.
C-
      FLAG = .FALSE.
      MAXDIM(1) = 0.
      MAXDIM(2) = 913.
      MAXDIM(3) = 0.
      MAXDIM(4) = 761.
C-
  999 RETURN
      END
