C----------------------------------------------------------------------
C-
C-   Created  18-DEC-1995   Dan Owen
C-
C_  COMMON BLOCK containing results calculated by LDSPs and needed by GDSPs
C
C-  L15DSP_INDEX = current LDSP #, set by L15_LOCAL_DSP is seed search loop
C   LDSP_PHI_MASK = array of phi mask words indicating phi possition of of JETS
C   NCANDS_TERM_DSP = Number of candidates found for each EM term
C----------------------------------------------------------------------
      INTEGER 
     &       LDSP_INDEX,
     &       LDSP_PHI_MASK( L15TM_NUM_MIN:L15TM_NUM_MAX,NUM_DSPS ),
     &       NCANDS_TERM_DSP(L15TM_NUM_MIN:L15TM_NUM_MAX,
     &       NUM_DSPS)
      COMMON /L15C_LDSP_RESULTS/ 
     &       LDSP_INDEX,
     &       LDSP_PHI_MASK,
     &       NCANDS_TERM_DSP
