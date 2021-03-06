C==================================================================
      SUBROUTINE ISZ3D
C==================================================================
C
C  Description:  Displays tracks generated by ISAJET with
C  ============  with lengths proportional to Pt or in XYZ space
C                in 3D
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - November 23,1988
C
C===================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
C
C  Local Declarations:
C  ====================
C
      INTEGER I3D             ! Calling mode indicator for ISZEVT
      INTEGER SI              ! Max no. of particles
      PARAMETER(SI=1000)
      INTEGER ID(SI)          ! ID of particles returned from GIPART
      INTEGER NPART           ! Actual no. of parts returned from GIPART
      INTEGER XYZPT
      REAL PX(SI),PY(SI)      ! XYZ components of momentum
      REAL PZ(SI)
      REAL P(SI)              ! Total momentum
      REAL M(SI)              ! Mass of particle
      REAL PHI(SI)            
      REAL THETA(SI),ETA(SI)  ! Angles of particle
      REAL X(SI),Y(SI),Z(SI)  ! XYZ space position of particle
C
C  Executable Code:
C  ================
C
      CALL GIPART(NPART,ID,PX,PY,PZ,P,M,PHI,THETA,ETA,X,Y,Z)
C
C
C  Draw 3D view...
C  ================
C
      CALL PUGETV('XYZ OR PT SPACE',XYZPT)
      IF (XYZPT .EQ. 1) CALL ISZPT3(NPART,ID,PX,PY,PZ,P,THETA)
      IF (XYZPT .NE. 1) THEN
         I3D = 1
         CALL ISZXYZ(NPART,ID,PX,PY,PZ,X,Y,Z,P,THETA,I3D)
      ENDIF
C
  999 CONTINUE
      RETURN
      END
C
