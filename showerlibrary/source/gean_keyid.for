      SUBROUTINE GEAN_KEYID(GEAN_ID,KEY_ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a Geant Particle ID number,
C-                         assigns a Key_ID number for showerlibrary
C-
C-   Inputs  : GEAN_ID
C-   Outputs : KEY_ID
C-   Controls: 
C-
C-   Created  24-MAY-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GEAN_ID,KEY_ID
      INTEGER MAP(48)
      DATA MAP/3*1,0,2*2,1,9*3,1,30*3,0/
C----------------------------------------------------------------------
C SCHEME IS AS FOLLOWS
C
C PARTICLE    GEANT_ID        KEY_ID
C
C  Gamma       1              1
C  e+,e-       2,3            1
C  Neutrino    4              0
C  mu+,mu-     5,6            2
C  Pizero      7              1
C  Pi+, Pi-    8,9            3
C  Klong,K+,K- 10,11,12       3
C  n,p         13,14          3
C  pbar        15             3
C  Kshort      16             3
C  Eta         17             1
C  Lambda      18             3
C  rest        19-47          3
C  Geantino    48             0
C----------------------------------------------------------------------
      KEY_ID = 0
      IF(GEAN_ID.GE.1.AND.GEAN_ID.LE.48)THEN
        KEY_ID = MAP(GEAN_ID)
      ENDIF
  999 RETURN
      END
