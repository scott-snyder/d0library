      SUBROUTINE MU_VERTEX_ONE( LPMUO, id_vert,vert_mtc)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods : returns the vertex number and
C-                        the vertex found for the PMUO
C-                        muon candidate in the current event
C-                        indicated by the input LPMUO
C-
C-  INPUT:
C-  LPMUO    - indicates the muon number for which you want the info
C-             (the LPMUO PMUO muon candidate in the event)
C-  OUTPUTS :
C-  ID_VERT  - the number of the vertex (using ZVERTE)
C-             assoiciated with the input PMUO muon candidate
C-             (0 if no vertices are found using zverte)
C-             (-1 if unable to make a vertex determination)
C-             (-2 if abs(zvertex-zvert)>500 cm)
C-  VERT_MTC - the vertex position calculated by this
C-             program for the input PMUO muon candidate
C-             (-200. if no vertices are found using zverte)
C-             (-201. if unable to make a vertex determination)
C-
C-   Created  25-JUL-1995   Elizabeth Gallas
C-   Modified 28-SEP-1995   EG - impose some good muon requirements
C-   Modified 04-OCT-1995   EG - add VERT_MTC to output argument list
C-   Modified 05-OCT-1995   EG - this is a modified version of
C-                          mu_vertex_info to work on a single
C-                          candidate at a time.
C-                          Good muon requirements are removed !
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c- subroutine output
      INTEGER lpmuo,id_vert
      REAL    vert_mtc
C- getting the event vertices
      INTEGER nvert,ivert
      REAL    zvert(10), dzvert(10)
C- local
      INTEGER lmuot,lcaeq,lcaep
      INTEGER gzcaeq,gzcaep
      REAL    dircos(3),point(3),rad, zvertex,hadfrac,enersum,
     &        distmin, dist

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

C- the muon I want is LPMUO, make sure it is ok, then get LMUOT
      IF(lpmuo.LE.0) THEN
        id_vert = -1
        vert_mtc = -201.
        RETURN
      ENDIF
      lmuot = lq(lpmuo-2)

C-no cuts!  C- impose some good muon criteria - first reject A-stubs
C-no cuts!        IF(IQ(LMUOT+4).EQ.5) THEN
C-no cuts!          id_vert  = -1
C-no cuts!          vert_mtc = -201.
C-no cuts!          go to 303
C-no cuts!        ENDIF

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
        id_vert = -1
        vert_mtc = -201.
        go to 303
      ENDIF

C-no cuts!  C- impose more good muon criteria
C-no cuts!        IFW4=IQ(LPMUO+9)
C-no cuts!  C-        for central muons
C-no cuts!        IF(IQ(LPMUO+7).LE.4) then
C-no cuts!          IF(IFW4.GE.2 .OR. HADFRAC.LT.0.66) THEN
C-no cuts!            id_vert = -1
C-no cuts!            vert_mtc = -201.
C-no cuts!            go to 303
C-no cuts!          ENDIF
C-no cuts!        ELSE
C-no cuts!  C-        for forward muons
C-no cuts!          IF(IFW4.GE.1 .OR. HADFRAC.LT.0.79) THEN
C-no cuts!            id_vert = -1
C-no cuts!            vert_mtc = -201.
C-no cuts!            go to 303
C-no cuts!          ENDIF
C-no cuts!        ENDIF

C- good 'vertex' found for this muon - 
C- see which event vertex it is closest to the found vertex ...
      vert_mtc = zvertex
      IF(nvert.EQ.0) THEN
        id_vert = 0
      ELSE
        distmin = 500.
        DO ivert=1,nvert
          dist = abs(zvert(ivert) - zvertex)
          IF(dist.LT.distmin) THEN
            id_vert = ivert
            distmin = dist
          ENDIF
          IF(distmin.EQ.500.) id_vert = -2
        ENDDO
      ENDIF

  303 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
