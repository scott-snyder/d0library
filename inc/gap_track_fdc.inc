C----------------------------------------------------------------------
C-
C-   Created   29-JAN-1996  Andrew G. Brandt  
C-
C----------------------------------------------------------------------
C
      INTEGER NTK_FDC(4),NBN_FDC(8)

      COMMON/GAP_TRACK_FDC/NTK_FDC,NBN_FDC

      CHARACTER*255 GAP_TRACK_FDC_TAG
      DATA GAP_TRACK_FDC_TAG/'NFDC[0,511]:I,NF_BCUT[0,511]:I,
     &     NF_BTHCUT[0,511]:I,NF_BZTHCUT[0,511]:I,NBN_FDC(8)[0,511]:I'/
