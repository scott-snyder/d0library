      REAL FUNCTION PTFIX(LPMUO,LVERT0)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Compute the highest Pt for muon LPMUO
C-     allowed by changing vertices.  This is a simple-minded
C-     calculation which assumes the muon points directly to a given
C-     vertex.  Any problems result in PTFIX < 0.0
C-
C-     Requires position and direction cosines in calorimeter and
C-     outside of toroid (in PMUO).
C-
C-   Inputs  : LPMUO  - The muon bank pointer
C-             LVERT0 - The initial vertex pointer.
C-   Outputs :
C-   Controls:
C-
C-   Created   5-Jul-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPMUO,LVERT0
C- Local variables
      INTEGER LVERT,LMUOT,VFIT,IVTX,NA,NB,NC,NLAYERS
      REAL    Z0,GAMMA0,GAMMA(3),R1,PT0,DZ,PTNEW,COSGAM,EPS,B1,M1
      REAL    PTMUOT
      PARAMETER(EPS=1.0E-10)
C- External functions
      REAL    VDOT
      INTEGER GZVERT
C-----------------------------------------------------------------------
      PTFIX=-1.0
      IF( LVERT0.LE.0 ) GOTO 999
      IF( LPMUO.LE.0 ) GOTO 999
C
      PTFIX=Q(LPMUO+14)                     ! Maximum pt is set to original pt
      PT0=Q(LPMUO+14)
      CALL UCOPY(Q(LPMUO+69),GAMMA,3)
C
      GAMMA0=VDOT(GAMMA,Q(LPMUO+75),3)      ! cos(angles before and after bend)
      R1 = SQRT(Q(LPMUO+66)**2+Q(LPMUO+67)**2)
      M1 = SQRT(GAMMA(1)**2+GAMMA(2)**2)/(GAMMA(3)+EPS)
      B1 = R1 - M1*Q(LPMUO+68)
      VFIT = MOD(IQ(LPMUO+4)/10,10)         ! Global fit status: Vertex?
      IF( VFIT.EQ.1 ) THEN
        IVTX = IQ(LPMUO+54)
        Z0 = Q(GZVERT(IVTX)+5)              ! Get Z of orginally matched vertex
      ELSE
        IVTX = -1
        Z0 = -B1/(M1+EPS)                   ! Use projected Z
      ENDIF

C- Loop over all vertices and compute 'Pt'. (NB: No change for fitted vtx)

      LVERT=LVERT0
      DO WHILE( LVERT.GT.0 )
C-   Get z direction cosine for this vertex
        IF(Q(LVERT+5).NE.0.0.OR.R1.NE.0.0) THEN
          GAMMA(3) = R1/SQRT(Q(LVERT+5)**2+R1**2)
        ELSE
          GAMMA(3) = 0.0
        ENDIF
        GAMMA(3) = SQRT(1.0-GAMMA(3)**2)
        IF( Q(LVERT+5).GT.Q(LPMUO+68) ) GAMMA(3) = -GAMMA(3)
        COSGAM = VDOT(GAMMA,Q(LPMUO+75),3)
        IF( COSGAM.LT.GAMMA0 ) GOTO 10      ! Skip if lower pt than original.
C-   Compute approximate Pt for this vertex
        DZ = ABS(Z0-Q(LVERT+5))
        PTNEW = ABS(PT0/(1-(DZ*PT0)/(300.0*0.6)))
        IF( PTNEW.GT.PTFIX ) PTFIX=PTNEW
   10   LVERT=LQ(LVERT)
      ENDDO

C- If it's a 3 layer track, check also MUOT...

      NA=MOD(IQ(LPMUO+47),10)
      NB=MOD(IQ(LPMUO+47)/10,10)
      NC=MOD(IQ(LPMUO+47)/100,10)
      NLAYERS = MIN(NA,1)+MIN(NB,1)+MIN(NC,1)
      LMUOT = LQ(LPMUO-2)
      IF( NLAYERS.GE.3 .AND. LMUOT.GT.0 ) THEN
        PTMUOT = Q(LMUOT+23)*SQRT(GAMMA(1)**2+GAMMA(2)**2)
        PTFIX=MAX(PTFIX,PTMUOT)
      ENDIF
C
  999 RETURN
      END

