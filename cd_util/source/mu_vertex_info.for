      SUBROUTINE MU_VERTEX_INFO( NMUONS,ID_VERT,VERT_MTC)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : returns the vertex number and
C-                        the vertex found for each PMUO
C-                        muon candidate in the event
C-
C-  Outputs :
C-  NMUONS            - the number of PMUO muon candidates in the event
C-                      (0 if there are no muons in the event)
C-                      a maximum of 30 muons may be found
C-  ID_VERT(1:NMUONS) - the number of the vertex (using ZVERTE)
C-                      assoiciated with each of the NMUONS muons
C-                      (0 if no vertices are found using zverte)
C-                      (-1 if unable to make a vertex determination)
C-                      (-2 if abs(zvertex-zvert)>500 cm)
C-  VERT_MTC(1:NMUONS) - the vertex position calculated by this
C-                      program for each of the NMUONS muons
C-                      (-200. if no vertices are found using zverte)
C-                      (-201. if unable to make a vertex determination)
C-
C-   Created  25-JUL-1995   Elizabeth Gallas
C-   Modified 28-SEP-1995   EG - impose some good muon requirements
C-   Modified 04-OCT-1995   EG - add VERT_MTC to output argument list
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c- subroutine output
      INTEGER nmuons,id_vert(30)
      REAL    vert_mtc(30)
C- getting the event vertices
      INTEGER nvert,ivert
      REAL    zvert(10), dzvert(10)
C- local
      INTEGER lpmuo,lmuot,lcaeq,lcaep, ifw4
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

          lmuot = lq(lpmuo-2)
C- impose some good muon criteria - first reject A-stubs
          IF(IQ(LMUOT+4).EQ.5) THEN
            id_vert(nmuons) = -1
            vert_mtc(nmuons) = -201.
            go to 303
          ENDIF
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
          IF(hadfrac.LE.0.65) THEN
            id_vert(nmuons) = -1
            vert_mtc(nmuons) = -201.
            go to 303
          ENDIF
C- impose more good muon criteria
          IFW4=IQ(LPMUO+9)
C-        for central muons
          IF(IQ(LPMUO+7).LE.4) then
            IF(IFW4.GE.2 .OR. HADFRAC.LT.0.66) THEN
              id_vert(nmuons) = -1
              vert_mtc(nmuons) = -201.
              go to 303
            ENDIF
          ELSE
C-        for forward muons
            IF(IFW4.GE.1 .OR. HADFRAC.LT.0.79) THEN
              id_vert(nmuons) = -1
              vert_mtc(nmuons) = -201.
              go to 303
            ENDIF
          ENDIF
C- 'vertex' found for this muon - see which event vertex it is closest to
          vert_mtc(nmuons) = zvertex
          IF(nvert.EQ.0) THEN
            id_vert(nmuons) = 0
          ELSE
            distmin = 500.
            DO ivert=1,nvert
              dist = abs(zvert(ivert) - zvertex)
              IF(dist.LT.distmin) THEN
                id_vert(nmuons) = ivert
                distmin = dist
              ENDIF
              IF(distmin.EQ.500.) id_vert(nmuons) = -2
            ENDDO
          ENDIF

  303     CONTINUE
          lpmuo=lq(lpmuo)          ! pointer to next muon
        ENDDO
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
