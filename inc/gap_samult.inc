C----------------------------------------------------------------------
C-
C-   Created  29-FEB-1996   Andrew Brandt (separate from GAP_TRACK)
C-                                                       
C----------------------------------------------------------------------
C
      INTEGER NHITS_SAMUS(6)
      COMMON/GAP_SAMULT/NHITS_SAMUS
      CHARACTER*255 GAP_SAMULT_TAG
      DATA GAP_SAMULT_TAG/'NSAMAS[-1,126],NSAMBS[-1,126],NSAMCS[-1,126],
     &  NSAMAN[-1,126],NSAMBN[-1,126],NSAMCN[-1,126]'/
