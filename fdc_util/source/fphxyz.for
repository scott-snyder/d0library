      SUBROUTINE FPHXYZ(HALF,SECTOR,FHIT,LHIT,WIRES,DRIFTS,X,Y,Z,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find three points to form a plane from a
C-                         chain of Phi sector hits.
C-
C-   Inputs  : HALF,SECTOR = Logical address of Phi Sector
C-             FHIT,LHIT   = First and last hits in partial segment
C-             WIRES       = Wires used in partial segment
C-             DRIFTS      = Drift distances of hits used in partial segment
C-   Outputs : X,Y,Z = coordinates of the three points to define plane.
C-             OK    = TRUE if all three points found
C-
C-   Created  12-JUN-1991   Jeffrey Bantly   
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR     ! Logical address of sector
      INTEGER FHIT,LHIT                 ! First and last hit in set
      INTEGER WIRES(NBPSEN)             ! Sense wires in set
C
      REAL    DRIFTS(NBPSEN)            ! Drift distances in set
      REAL    X(3),Y(3),Z(3)            ! Three 3-D points for plane
      REAL    DELAY                     ! Faked third coordinate
      REAL    DRIFTD                    ! Drift dist adjusted by stagger
      REAL    SDRIFT,CDRIFT             ! Drift direction cosines
      REAL    XC,YC,ZC                  ! Center of wire in 3-D
      REAL    STAGGER                   ! Wire stagger distance
      REAL    FSTAGR                    ! Function returns wire stagger
      EXTERNAL FSTAGR
C
      LOGICAL OK                        ! set TRUE if three non-colinear
C                                       ! points are found
C----------------------------------------------------------------------
      OK=.FALSE.
      IF(LHIT.LE.FHIT) GOTO 999
C
C  Fetch the drift direction cosines for the sector.  
C
      CALL FDRIFTDIR(HALF,1,0,SECTOR,0,SDRIFT,CDRIFT)
C
C  Accumulate data on first hit and fill X,Y,Z for two points using
C  fake delay line distance of 0.,20.cm for Phi chamber partial segment
C
      CALL GTFALH(HALF,1,0,SECTOR,WIRES(FHIT),XC,YC,ZC)
      IF(ZC.EQ. 0.0) GOTO 999
      STAGGER = 0.0
      STAGGER = FSTAGR(HALF,1,0,SECTOR,WIRES(FHIT))
      DRIFTD  = DRIFTS(FHIT)-STAGGER
      DELAY   = 20.
      X(1) = XC+DRIFTD*CDRIFT
      X(2) = XC+DRIFTD*CDRIFT+DELAY*(-SDRIFT)
      Y(1) = YC+DRIFTD*SDRIFT
      Y(2) = YC+DRIFTD*SDRIFT+DELAY*CDRIFT
      Z(1) = ZC
      Z(2) = ZC
C
C  Accumulate data on last hit and fill X,Y,Z for third point using
C  fake delay line distance of 0.0 cm even for Phi chamber segments
C
      CALL GTFALH(HALF,1,0,SECTOR,WIRES(LHIT),XC,YC,ZC)
      STAGGER = 0.0
      STAGGER = FSTAGR(HALF,1,0,SECTOR,WIRES(LHIT))
      DRIFTD  = DRIFTS(LHIT)-STAGGER
      IF(ZC.EQ. 0.0) GOTO 999
      X(3) = XC+DRIFTD*CDRIFT
      Y(3) = YC+DRIFTD*SDRIFT
      Z(3) = ZC
C
      OK=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
