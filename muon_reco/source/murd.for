      SUBROUTINE MURD(IS,SD,MSA,MOM,X,MOMER,PHI,THETA,
     &                 PHIMIN,PHIMAX,THEMIN,THEMAX,DELT_R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find muon road for Central tracker
C-
C-   Inputs  :
C-              IS       : Element number in array SD choosing desired SD
C-              SD(*)    : List of standard diviations
C-              MSA(*)   : RMS MS angle array for various standard deviations
C-              X(1:3)   : Starting point coordinates. (vertex for now)
C-              X(4:6)   : Direction cosines of Muon.
C-                 MOM   : Muon momentum.
C-               MOMER   : Error in Muon momentum.
C-           PHI,THETA   : angles of muon direction
C-
C-   Outputs :
C-            PHIMIN,PHIMAX,THEMIN,THEMAX : Road in terms of angles
C-   Controls:
C-
C-   Created  25-APR-1990   SHAHRIAR ABACHI
C-   Modified 09-APR-1991   SHAHRIAR ABACHI
C-   Updated   7-JAN-1993   Jeffrey Bantly
C-   Updated  14-apr-93    Hedin limit adjustment of road by tan(theta)
C-                         to forward only. plus simplify
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IS,IER
      REAL SD(*),MSA(*),MOM,X(*),MOMER,PHI,THETA,ADJPHI
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX, DELT_R
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL DPHI, DTHE, ETAMAX, ETAMIN, DETA, TANG1,TANG2, EPS
      REAL DTCD,DTMS,DTMU,DPCD,DPMS,DPMU,DTEX,DPEX
      REAL THETA_FRAC
      REAL NUMER,DENOM
      LOGICAL FIRST
      SAVE FIRST
      DATA EPS /1.0E-30/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C - CD related errors
C
      DTCD = ABS(THETA - HALFPI) * 0.012 * SD(IS)
      IF (ABS(THETA-HALFPI) .GT. PI/4.0) DTCD = 1.0 * RADIAN * SD(IS)
      DPCD = 0.05 * RADIAN * SD(IS)
C
C - Multiple scattering effect
C
      DTMS = MSA(IS)
      DPMS = MSA(IS)
C
C - Muon system effect
C
      DTMU = 0.04 * SD(IS)
      DPMU = 0.04 * SD(IS)
C
C - Other effects
C
      DTEX = 0.05 * SD(IS)
      DPEX = 0.05 * SD(IS)
C
      DTHE = SQRT(DTCD**2 + DTMS**2 + DTMU**2 + DTEX**2)
      DPHI = SQRT(DPCD**2 + DPMS**2 + DPMU**2 + DPEX**2)
C
C - Widen phi muon road in forward regions
C
      IF ( (THETA.LT.PI*0.25) .OR. (THETA.GT.PI*.75) ) THEN
        DENOM = ABS(TAN(THETA))
        NUMER = ABS(DPHI)
        IF ( DENOM.NE.0.0 ) DPHI = NUMER / DENOM
      ENDIF
C
      PHIMIN = PHI - DPHI
      PHIMAX = PHI + DPHI
      THEMIN = THETA - DTHE
      THEMAX = THETA + DTHE
C
      IF(THEMIN .LT. 0.0) THEMIN = 0.0
      IF(THEMAX .GT. PI) THEMAX = PI
C
      TANG1 = TAN(THEMIN/2.0)
      TANG2 = TAN(THEMAX/2.0)
      IF(TANG1 .GT. 0.) THEN
        ETAMIN = -ALOG(TANG1)
      ELSE
        ETAMIN = -50.
      ENDIF
      IF(TANG2 .GT. 0.) THEN
        ETAMAX = -ALOG(TANG2)
      ELSE
        ETAMAX = -49.
      ENDIF
      DETA = (ETAMAX - ETAMIN)/ 2.0
      DELT_R = SQRT(DETA**2 + DPHI**2)
C
C----------------------------------------------------------------------
  999 RETURN
      END
