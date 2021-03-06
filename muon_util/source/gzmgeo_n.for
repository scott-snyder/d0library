      INTEGER FUNCTION GZMGEO_N (NMOD)
C----------------------------------------------------------------------------
C
C Function to return pointer to MGEO geometry bank for module NMOD
C
C  J. Green  April 89   modification of GZMGEO for bank hanging under STPN
C----------------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'        
      INCLUDE 'D0$LINKS:IZSTPN.LINK/LIST'
      INCLUDE 'D0$LINKS:IZSMUO.LINK/LIST'
      INCLUDE 'D0$LINKS:IZMGEH.LINK/LIST'
      INTEGER LSTPN
C
      INTEGER NMOD
C
      GZMGEO_N=0                
C -- go through chain of banks in tree structure... (temporary)
      LSTPN=LC(LSTPH-IZSTPN)
      IF(LSTPN.EQ.0) GO TO 900
      LSMUO=LC(LSTPN-IZSMUO)
      IF(LSMUO.EQ.0) GO TO 900
      LMGEH=LC(LSMUO-IZMGEH)
C -- check if MGEH bank exists... 
      IF(LMGEH.NE.0) THEN
C     -- check the number of structual links...
         IF(IC(LMGEH-2).GE.NMOD) THEN
             GZMGEO_N = LC (LMGEH-NMOD)
        ENDIF
      ENDIF
C                             
900   CONTINUE
      RETURN
      END

