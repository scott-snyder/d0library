C----------------------------------------------------------------------
C-
C-   Created   10-MAR-1996  Andrew G. Brandt  FDC segments
C-
C----------------------------------------------------------------------
C
      INTEGER NFSGIN(4),NFSGON(4),NFSGIS(4),NFSGOS(4)
C
      COMMON /FDCSEG/NFSGIN,NFSGON,NFSGIS,NFSGOS

      CHARACTER*255 GAP_FSEG_TAG
      DATA GAP_FSEG_TAG/'NFSGIN(4)[-1,1026]:I,NFSGON(4)[-1,1026]:I,
     &                   NFSGIS(4)[-1,1026]:I,NFSGOS(4)[-1,1026]:I'/
