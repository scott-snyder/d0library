      SUBROUTINE FILTER_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Level-2 filter
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   22-NOV-93   by REAL_MAKE_INIT routine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FILTER_COM.INC'
      CHARACTER*(*) NAME_STR
      EXTERNAL L2JETS
      EXTERNAL CDC_L2TRK
      EXTERNAL MUON_L2
      EXTERNAL COSMIC_VTX
      EXTERNAL COSMIC_L2_FDC
      EXTERNAL L2_EM
      EXTERNAL TOOL1
      EXTERNAL L2ETMISS
      EXTERNAL L2ETSUM
      EXTERNAL L2_MU_COSMICS
      EXTERNAL L2SETUP
      EXTERNAL COSMIC_TRD
      DATA TOOL_NAMES/
     &   'L2JETS',
     &   'CDC_L2TRK',
     &   'MUON_L2',
     &   'COSMIC_VTX',
     &   'COSMIC_L2_FDC',
     &   'L2_EM',
     &   'TOOL1',
     &   'L2ETMISS',
     &   'L2ETSUM',
     &   'L2_MU_COSMICS',
     &   'L2SETUP',
     &   'COSMIC_TRD',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' ',
     &   ' '/
C----------------------------------------------------------------------
      MAX_TOOL_USE=0
      TOOL_TABLE(10)=%LOC(L2JETS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(13)=%LOC(CDC_L2TRK)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(12)=%LOC(MUON_L2)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(16)=%LOC(COSMIC_VTX)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(15)=%LOC(COSMIC_L2_FDC)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(17)=%LOC(L2_EM)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(1)=%LOC(TOOL1)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(18)=%LOC(L2ETMISS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(19)=%LOC(L2ETSUM)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(30)=%LOC(L2_MU_COSMICS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(22)=%LOC(L2SETUP)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(31)=%LOC(COSMIC_TRD)
      MAX_TOOL_USE=MAX_TOOL_USE+1
  999 RETURN
      ENTRY GET_L2TYPE_NAME(NAME_STR)
      NAME_STR='COSMIC_L2'
      RETURN
      END
