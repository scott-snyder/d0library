C----------------------------------------------------------------------
C-            LEVEL 1 TRIGGER TOWER ENERGIES
C-              For local DSP clustering
C-
C-   Created   9-MAY-1994   sFahey
C-   Updated  22-MAR-2004   sss - compile with g77
C-
C----------------------------------------------------------------------
C   This is the raw data, i.e. the bytes received by the Local DSP's
C   from the L1 Cal trig
      INTEGER      L15CT_TT_RAW(-ETA_MAX:ETA_MAX,
     &                           PHI_MIN:PHI_MAX,
     &                           L15C_EM_TWR:L15C_TOT_TWR )
C
C   This is the corresponding energy, in GeV, with offset substracted.
      REAL         L15CT_TT_ENERGY(-ETA_MAX:ETA_MAX,
     &                              PHI_MIN:PHI_MAX,
     &                              L15C_EM_TWR:L15C_TOT_TWR)
C
C   EMET and TOTET in block L15_LOCAL_DSP
C
      REAL  EMET( -ETA_MAX:ETA_MAX, PHI_MIN:PHI_MAX)
      REAL  TOTET(-ETA_MAX:ETA_MAX, PHI_MIN:PHI_MAX)
C
      EQUIVALENCE (EMET(-ETA_MAX,PHI_MIN),
     &             L15CT_TT_ENERGY(-ETA_MAX,PHI_MIN,L15C_EM_TWR))
      EQUIVALENCE (TOTET(-ETA_MAX,PHI_MIN),
     &             L15CT_TT_ENERGY(-ETA_MAX,PHI_MIN,L15C_TOT_TWR))
C
      COMMON /L15_LOCAL_DSP_cm/ EMET,TOTET,L15CT_TT_RAW
