      SUBROUTINE COSMIC_TRD
     &         (PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy first pass at TOOL
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created  22-SEP-93   by the L2STATE program
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:COSMIC_TRD_CUTS.INC'
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL RESULT_FLAG,EXTRA_FLAG
      INTEGER NTRIP,NCEL_COSM
C----------------------------------------------------------------------
      RESULT_FLAG=.TRUE.
      CALL TSETWC_LEV2
      CALL TRDPAT_LEV2(NTRIP,NCEL_COSM)
      IF (NTRIP.LE.NTRIPLET(PARAM_SET_NUMBER)) RESULT_FLAG=.FALSE.
  999 RETURN
      END
