      SUBROUTINE MUCATRAK(LMUON,POINT,COV,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reconstruct muon track in the Calorimeter.
C-   For a given muon candidate loop over primary vertices and determine
C-   the exit point in the calorimeter. 
C-
C-   Inputs  : LMUON  pointer to MUON bank
C-   Outputs : POINT,COV    - coordinates of exit point and cov matrix 
C-             IERR       - 0 if sucessful (to be defined)
C- 
C-   For QUAD=1,3 POINT(1,2) =  Y,Z at XOUT
C-   (for QUAD=2,4 x<->y), for  QUAD>4 x<->z)
C-
C-   Controls: 
C-
C-   Created  18-APR-1993   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER IERR,IER,LMUON,LMUOT,MAXVER,NVER,IVER,QUAD,I,MUVERT
      PARAMETER (MAXVER=5)
      REAL VERT(3,MAXVER),VXYZ(3),DIR(3),PHI,THETA,ETA
      REAL POINT(3),XYZ(3),COV(2,2),COVXYZ(2,2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      CALL EZPICK('MURECO_RCP')
      IF(FIRST) THEN
        CALL EZGET('MUVERT',MUVERT,IER)
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
      CALL VZERO(POINT(1),3)
      CALL VZERO(VXYZ(1),3)
      IERR=1
      LMUOT=LQ(LMUON-11)
      THETA=Q(LMUON+16)
      ETA=Q(LMUON+17)
      PHI=Q(LMUON+18)
      DIR(1) = SIN(THETA)*COS(PHI)
      DIR(2) = SIN(THETA)*SIN(PHI)
      DIR(3) = COS(THETA)
      IVER = -1
      CALL MVERXYZ(IVER,MAXVER,VERT,NVER)
      IF(NVER .GT. 0) THEN
        DO I=1,3
          VXYZ(I) = VERT(I,NVER)
        END DO
C
C  for the line defined by VXYZ point and THETA,PHI find entry and exit 
C  point in the calorimeter. 
C
        CALL MUCAXYZ(VXYZ,DIR,XYZ,COVXYZ,IERR)
        IF (IERR.EQ.0) THEN
          IVER=I
          CALL UCOPY(XYZ,POINT,3)
          CALL UCOPY(COVXYZ,COV,4)
        END IF
      ENDIF
      WRITE (0,101) (POINT(I),I=1,3)
  101 FORMAT(' MUCATRAK:  CAL exit point',3F10.2)
  999 RETURN
      END
