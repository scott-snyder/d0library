      FUNCTION GZJETS()
C
C******************************************************************************
C
C     PURPOSE: GZJETS returns the link to the first JETS bank in
C              the linear chain.
C
C     CREATED: March 20, 1989 by Z. Wolf
C-   Updated   15-MAY-1990   Serban D. Protopopescu   
C
C******************************************************************************
C
      IMPLICIT NONE
      INTEGER GZJETS
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJETS.LINK'
C
C--   INTERNAL VARIABLES
      INTEGER GZCAPH,LCAPH
C
      GZJETS=0
      LCAPH=GZCAPH()
      IF(LCAPH.LE.0) GOTO 999
      GZJETS=LQ(LCAPH-IZJETS)
C
999   RETURN
      END
