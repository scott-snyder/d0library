C----------------------------------------------------------------------
C-
C-   Created  31-AUG-1991   K. Wyatt Merritt
C-   Contains pointer to the next primary track on JSTAK.  We must
C-   create this ourselves in GUTRAK in order to know when all the
C-   secondaries of a given track been tracked, so that CALBLD can
C-   be called.  GEANT 3.14 does not provide any way to know this.
C-
C----------------------------------------------------------------------
      INTEGER PTR_NXT_PRIMARY
      COMMON /GUCTRK/ PTR_NXT_PRIMARY
