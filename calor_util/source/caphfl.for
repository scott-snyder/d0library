      SUBROUTINE CAPHFL(NCLS,NJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     CAPHFL fills words 2 and 3 of the CAPH bank with the number
C-     of clusters and the number of jets, respectively.
C-     If either number is zero, it is not filled.
C-
C-   Inputs  : 
C-     NCLS = the number of clusters
C-     NJETS = the number of jets
C-
C-   Outputs : None 
C-   Controls: None
C-
C-   Created  24-APR-1989   Serban D. Protopopescu
C-   Modified  3-MAY-1989   ZACHARY WOLF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   I/O
      INTEGER NCLS,NJETS
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LCAPH
C----------------------------------------------------------------------
C
      CALL BKCAPH(LCAPH)    ! book CAPH bank
C
C--   FILL NCLS AND NJETS
      IF(NCLS.GT.0)IQ(LCAPH+2)=NCLS
      IF(NJETS.GT.0)IQ(LCAPH+3)=NJETS
C
  999 RETURN
      END
