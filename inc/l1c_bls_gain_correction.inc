C----------------------------------------------------------------------
C-
C-   Created  20-SEP-1991   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-   Updated  24-OCT-1991   Philippe Laurens, Steven Klocek  
C-                              Changed name of common block from
C-                              LV1SIM_BLS_GAIN_CORRECTION to
C-                              L1C_BLS_GAIN_CORRECTION.  
C-
C----------------------------------------------------------------------
      REAL BLS_GAIN_CORRECTION(-NETAL:NETAL,
     &                         1:NPHIL,
     &                         1:NLYRL)
C
      COMMON /L1C_BLS_GAIN_CORRECTION/ BLS_GAIN_CORRECTION
