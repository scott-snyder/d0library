      REAL FUNCTION MUON_VTX_RECALC(PMUO,ZV0,ZV1,PT_OUT,ETA_OUT,
     > THET_OUT,PTKICK)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Compute the Pt for muon in PMUO if the vertex
C-     of the muon is VERT1.  The original muon vertex is VERT0.
C-     This is a simple-minded calculation which assumes the muon points 
C-     directly to a given vertex.  Any problems result 
C-     in MUON_VTX_RECALC < 0.0
C-
C-     Requires position and direction cosines in calorimeter and 
C-     outside of toroid (in PMUO).
C-
C-   Inputs  : PMUO    - The muon bank as an array
C-             ZV0     - The initial-vertex Z position
C-             ZV1     - The new-vertex Z position
C-   Outputs : PT_OUT  - The new Pt
C-             ETA_OUT - The new eta
C-             THET_OUT- The new theta
C-             PTKICK  - BdL computed from PMUO parameters
C-   Controls:
C-
C-   Created   5-Jul-1995   John D. Hobbs
C-   Modified 11-Oct-1995   JDH - Rename to MUON_VTX_RECALC and turn
C-    into standalone routine.  MAJOR MODIFICATIONS TOO NUMEROUS TO
C-    LIST.
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PMUO(*),ZV0,ZV1,PT_OUT,ETA_OUT,THET_OUT,PTKICK
C- Local variables
      INTEGER VFIT,IVTX
      REAL    Z0,GAMMA(3),GAMMA_OUT(3),GAMMA_NEW(3),RP,PT0,DZ,PTNEW,EPS
      REAL    WEIGHT,THETA_NEW,BP,MP,ZP,A0,A1,SCALE_FACTOR,PNEW,PHI
      REAL    COSGAM,COSGAM_NEW,POLD
      PARAMETER(EPS=1.0E-10)
C- External functions
      REAL    VDOT
C-----------------------------------------------------------------------

      PT0=PMUO(14)
      POLD=PMUO(13)
*JDH      CALL UCOPY(PMUO(69),GAMMA,3)          ! Dir. Cosines inside Toroid.
      GAMMA(1)=PMUO(10)/PMUO(13)            ! Dir. Cosines from momentum
      GAMMA(2)=PMUO(11)/PMUO(13)
      GAMMA(3)=PMUO(12)/PMUO(13)
      RP = SQRT(PMUO(66)**2+PMUO(67)**2)    ! Radial position at 'A-layer'
      ZP = PMUO(68)                         ! Z position at 'A-layer'
      CALL UCOPY(PMUO(75),GAMMA_OUT,3)      ! Dir. Cosines after Toroid.

C- Compute BdL for possible cross-check w/MUOT BdL

      COSGAM = VDOT(PMUO(69),PMUO(75),3)
      IF( COSGAM.GE.1.0 ) THEN
        CALL ERRMSG('NO_MU_PT_KICK','MUON_VTX_RECALC',' ','W')
        PTKICK=0.0
      ELSE
        PTKICK = POLD*SQRT(1.0-COSGAM**2)
      ENDIF

C- Get initial muon Z position at beam line...

      CALL UCOPY_i(PMUO(4),VFIT,1)
      VFIT = MOD(VFIT/10,10)         ! Global fit status: Vertex?
      IF( VFIT.EQ.1 ) THEN
        CALL UCOPY_i(PMUO(54),IVTX,1)
        Z0 = ZV0                     ! Get Z of orginally matched vertex
      ELSE
        IVTX = -1
        MP = SQRT(GAMMA(1)**2+GAMMA(2)**2)/(GAMMA(3)+EPS)
        BP = RP - MP*PMUO(68)
        Z0 = -BP/MP                  ! Use projected Z
      ENDIF

C- Compute direction at new vertex. (NB: No change for fitted vtx)

      DZ=ZP-ZV1
      THETA_NEW = ATAN2(RP,DZ)
      THET_OUT = THETA_NEW
      ETA_OUT = -LOG(TAN(THETA_NEW/2.0))

C- Compute the new direction cosines inside the toroid.

      PHI = ATAN2(GAMMA(2),GAMMA(1))
      GAMMA_NEW(1)=COS(PHI)*SIN(THETA_NEW)
      GAMMA_NEW(2)=SIN(PHI)*SIN(THETA_NEW) 
      GAMMA_NEW(3)=COS(THETA_NEW)

C- Compute approximate Pt for this vertex

      COSGAM=VDOT(GAMMA,GAMMA_OUT,3)
      COSGAM_NEW=VDOT(GAMMA_NEW,GAMMA_OUT,3)
      IF( COSGAM_NEW.GE.1.0 ) THEN
        IF( PTKICK.LE.0 ) THEN
          PNEW=0.01
        ELSE
          PNEW=10000.0
        ENDIF
      ELSE
        SCALE_FACTOR = SQRT((1-MIN(COSGAM**2,0.999))/(1-COSGAM_NEW**2))
        PNEW = MIN(POLD*SCALE_FACTOR,10000.0)
      ENDIF
      PT_OUT = PNEW*SIN(THETA_NEW)
      MUON_VTX_RECALC = PT_OUT

C

 999  RETURN
      END
