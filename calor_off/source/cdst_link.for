      SUBROUTINE CDST_LINK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Setup links in Zlinkc assuming there is 
C-   a DST  
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-SEP-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER GZCAEH,GZCAEP,GZCATE,GZCAPH,GZCACL,GZCACH,GZJETS
      INTEGER GZJTSH,GZJPTS,GZPELC,GZPPHO,GZPNUT
C----------------------------------------------------------------------
      LCAEH = GZCAEH()
      LCAEP = GZCAEP()
      LCATE = GZCATE()
C
      LPELC = GZPELC()
      LPPHO = GZPPHO()
      LPNUT = GZPNUT(0)
C
      LCAPH = LQ(LPROC-IZCAPH) ! TAKE THE FIRST CAPH
      LCACL = GZCACL()
      IF(LCACL.NE.0)LCACH = GZCACH(LCACL)
      LJETS = GZJETS()
      LJTSH = GZJTSH()
      LJPTS = GZJPTS()
C
  999 RETURN
      END
