      SUBROUTINE PIDIST(PTMIN,LTRK,PXYZ,P,XVW,XC,XPV,DIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Auxiliary routine of PIPICK that gets dist
C-                         from a virtual point to an ISAJET track
C-
C-   Inputs  : PTMIN   - minimum Pt to display a track
C-             LTRK    - track length for display
C-             PXYZ(3) - Px, Py, Pz of particle
C-             P       - momentum of particle
C-             XVW(3)  - world coord of track begin point
C-             XC(3)   - world coord of track end point, if there...
C-             XPV(2)  - virtual coord of the point
C-   Outputs : DIST    - distance from the track to the point
C-
C-   Created  13-JUN-1990   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I
      REAL    PXYZ(3),P, XVW(3),XC(3),XPV(2),DIST
      REAL    PT,PTMIN,LTRK
      REAL    XCW(3),XVV(2),XCV(2),SL,FL,XMV
C----------------------------------------------------------------------
      DIST=1E6
      PT = SQRT(PXYZ(1)**2 + PXYZ(2)**2)
      IF (PT .LT. PTMIN) GO TO 999
        IF(XC(1).EQ.0. .AND. XC(2).EQ.0. .AND. XC(3).EQ.0.) THEN
          DO 50 I=1,3
            XCW(I) = XVW(I) + (LTRK*PXYZ(I)/P)
   50     CONTINUE
        ELSE
          DO 75 I=1,3
            XCW(I)=XC(I)
   75     CONTINUE
        ENDIF
        CALL JCONWV(XVW(1),XVW(2),XVW(3),XVV(1),XVV(2))
        CALL JCONWV(XCW(1),XCW(2),XCW(3),XCV(1),XCV(2))
        SL=0.
        FL=0.
        DO 100 I=1,2
          SL=SL+( XPV(I)-XVV(I) )*( XCV(I)-XVV(I) )
          FL=FL+( XCV(I)-XVV(I) )**2
  100   CONTINUE
        IF(SL.LT.0. .OR. SL.GT.FL) GO TO 999
        DIST=0.
        DO 200 I=1,2
          XMV=XVV(I)+(SL/FL)*( XCV(I)-XVV(I) )
          DIST=DIST+( XPV(I)-XMV )**2
  200   CONTINUE
        DIST=SQRT(DIST)
C-
  999 RETURN
      END
