      SUBROUTINE MU_VERTEX_INFO( NMUONS,ID_VERT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the vertex number associated with
C-    each muon in the event
C-
C-   Outputs :
C-  NMUONS            - the number of muons in the event
C-                      (0 if there are no muons in the event)
C-                      a maximum of 30 muons may be found
C-  ID_VERT(1:NMUONS) - the number of the vertex (using ZVERTE)
C-                      assoiciated with each of the NMUONS muons
C-                      (0 if no vertices are found using zverte)
C-                      (-1 if unable to make a vertex determination)
C-                      (-2 if abs(zvertex-zvert)>500 cm)
C-
C-   Created  25-JUL-1995   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c- subroutine output
      INTEGER nmuons,id_vert(30)
C- getting the event vertices
      INTEGER nvert,ivert
      REAL    zvert(10), dzvert(10)
C- local
      INTEGER lpmuo,lmuot,lcaeq,lcaep
      INTEGER gzpmuo,gzcaeq,gzcaep
      REAL    dircos(3),point(3),rad, zvertex,hadfrac,enersum,
     &        distmin, dist

C----------------------------------------------------------------------
C- initialize output
      nmuons = 0
C----------------------------------------------------------------------
C- A calorimeter cell energy bank must be available.
C- if CAEP exists, use it;  if CAEQ but no CAEP, create CAEP from CAEQ.
      lcaep = gzcaep()
      IF(lcaep.LE.0) THEN
        lcaeq = gzcaeq()
        IF(  lcaeq.LE. 0 ) THEN
          WRITE(6,*) ' MU_VERTEX_INFO: error - no CAEP or CAEQ bank'
          RETURN
        ELSE
          IF(lcaep .LE. 0 ) CALL caeq_to_caep
          lcaep = gzcaep()
        ENDIF
      ENDIF
C----------------------------------------------------------------------
C- get the event vertices - if none, then set id_vert to 0 for all muons
      CALL zverte( nvert,zvert,dzvert)

C- loop through muon banks
      lpmuo=gzpmuo(0)
      IF(lpmuo.NE.0) THEN
        DO WHILE (lpmuo.GT.0)
          IF(nmuons.EQ.30) RETURN

          nmuons = nmuons + 1
          IF(nvert.EQ.0) THEN
            id_vert(nmuons) = 0
            go to 303
          END IF

          lmuot = lq(lpmuo-2)
C- MUOT words 14-16 are dir cos of track from this point (inside magnet)
          dircos(1) = q(lmuot+14)
          dircos(2) = q(lmuot+15)
          dircos(3) = q(lmuot+16)
C- MUOT words 8,9,10 - x,y,z coord of track closest to the cal (a-layer)
          point(1)   = q(lmuot+8)
          point(2)   = q(lmuot+9)
          point(3)   = q(lmuot+10)
C- if the above point is on the beamline, then use the pnt at toroid center
          rad = sqrt( point(1)**2 + point(2)**2 + point(3)**2)
          IF(rad.LT.100.) THEN
C- MUOT words 11,12,13 - xyz outside magnet: NOMINALLY AT CENTER OF MAGNET
            point(1)   = q(lmuot+11)
            point(2)   = q(lmuot+12)
            point(3)   = q(lmuot+13)
          ENDIF
C- find the point on the beamline that the muon points to
          CALL mtc_getmuvtx( point,dircos, zvertex,hadfrac,enersum)
          IF(hadfrac.EQ.0.) THEN
            id_vert(nmuons) = -1
            go to 303
          ENDIF
C- 'vertex' found for this muon - see which event vertex it is closest to
          distmin = 500.
          DO ivert=1,nvert
            dist = abs(zvert(ivert) - zvertex)
            IF(dist.LT.distmin) THEN
              id_vert(nmuons) = ivert
              distmin = dist
            ENDIF
          ENDDO
C- no vertex within 500 cm found ???
          IF(distmin.EQ.500.) id_vert(nmuons) = -2

  303     CONTINUE
          lpmuo=lq(lpmuo)          ! pointer to next muon
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
