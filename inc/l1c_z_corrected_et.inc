C----------------------------------------------------------------------
C-
C-   Created  23-JUL-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LV1_Z_CORRECTED_ET to L1C_Z_CORRECTED_ET 
C-
C----------------------------------------------------------------------
C
C      Store the Z-Corrected EM Et and HD Et
C
C
C       Output of PROMs corrected for the zero energy response. A zero here
C       corresponds to a reading of zero energy. These values may be negative.
C
      INTEGER Z_CORRECTED_ET(POS_ETA:NEG_ETA,
     &                       ETA_MIN:ETA_MAX,
     &                       PHI_MIN:PHI_MAX,
     &                       EM_TOWER:HD_TOWER)
C
      COMMON /L1C_Z_CORRECTED_ET/ Z_CORRECTED_ET
