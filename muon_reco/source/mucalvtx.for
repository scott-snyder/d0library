      SUBROUTINE MUCALVTX(XM,NV,VERTEX,NC,IVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : In case of multiple vertex, determine
C-                         which vertex is prefered by calorimeter
C-                         considering expected muon dedx deposition.
C-
C-   Inputs  :  XM(3)            Mid mag coordinate
C-              NV               Maximum # of vertices to consider
C-              VERTEX(3,*)     Coordinate for all vertices
C-              NC               Number of cal neighboring cells to consider
C-
C-   Outputs :  IVTX         Vertex number chosen by Cal
C-   Controls:
C-
C-   Created  15-APR-1993   SHAHRIAR ABACHI
C-   Updated  12-OCT-1993   Daria Zieminska  if NV>1 preset IVTX to 0 
C-                          instead of 1 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NV,IVTX,NC
      REAL XM(3),VERTEX(3,*)
      REAL VL,THETA,PHI,DIRC(3),EEM,ETOT,ET,EPS
      REAL TANT,ETA,VERT(3),PI
      INTEGER I,IER,IVTRY,IV
      DATA EPS,PI /1.0E-5,3.1415/
C
      IVTX = 0
      IF(NV .LT. 2) THEN
        IVTX = 1
        GOTO 999
      ENDIF
C
      IVTRY = 1
      IV = 1
   10 CONTINUE
      VL = 0.
      DO I=1,3
        VERT(I) = VERTEX(I,IV)
        VL = VL + (VERT(I) - XM(I))**2
      ENDDO
      VL = SQRT(VL)
      DO I=1,3
        DIRC(I) = (XM(I) - VERT(I)) / VL
      ENDDO
      PHI = ATAN2(DIRC(2), DIRC(1))
      IF(DIRC(2) .LT. 0.0) PHI = PHI + 2.0 * PI
      THETA = ACOS(DIRC(3))
C
      IF(THETA .LT. EPS) THETA = THETA + EPS
      IF(THETA .GT. 3.1) THETA = THETA - EPS
      TANT = TAN(THETA/2.0)
      ETA = -ALOG(TANT)
      CALL CAL_ECELL(VERT,ETA,PHI,NC,EEM,ETOT,ET,IER)
      IF((ETOT .LT. 1.0 .AND.
     &         (ABS(ETA) .LT. 0.95 .OR. ABS(ETA) .GT. 1.15)) .OR.
     &         (ETOT .LT. 0.5 .AND.
     &         (ABS(ETA) .GE. 0.95 .AND. ABS(ETA) .LE. 1.15))) THEN
        IF(IVTRY .LT. NV) THEN
          IVTRY = IVTRY + 1
          IV = IV + 1
          GOTO 10
        ENDIF
      ELSE
        IVTX = IV
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
