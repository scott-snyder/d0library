      INTEGER FUNCTION MULAYR(NMOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines what layer a muon module is in
C-
C-   Inputs  : NMOD is Module ID (Phil Martin number)
C-
C-   Outputs : A layer is layer 1, B layer 2, C layer 3,4
C-
C-   Created  13-MAR-1986   David Hedin
C-   DH 2/88
C-   DH 3/90 add layer 4
C-   MF 6/94 add SAMUS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD,NTHETA,MLAYR(45)
      DATA MLAYR/1,1,1,0,0,1,0,0,1,2,2,2,2,2,2,2,0,2,2,
     &           3,3,3,3,3,4,4,3,4,4,3,9*0,1,2,4,1,2,4/
      MULAYR = 0
      NTHETA = NMOD/10
      IF (NMOD.GE.10.AND.NMOD.LT.460) MULAYR=MLAYR(NTHETA)
      RETURN
      END
