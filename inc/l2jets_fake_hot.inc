C----------------------------------------------------------------------
C- These are 'fake' values to be used to fill D0$INC:L2JETS_HOT.INC
C- in the case the TRGR block will not do it.
C- 
C-   Created   5-AUG-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      INTEGER NJTHOT_FAKE
      LOGICAL JT_COMP_FAKE
      INTEGER IHOT_MSK_JT_FAKE(NHOT_MAX)
      INTEGER IHOT_ADR_JT_FAKE(NHOT_MAX)

      COMMON /L2JETS_FAKE_HOT/ IHOT_ADR_JT_FAKE,IHOT_MSK_JT_FAKE,
     &  JT_COMP_FAKE, NJTHOT_FAKE
