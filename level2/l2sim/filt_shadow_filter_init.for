      SUBROUTINE FILTER_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Level-2 filter
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created    7-MAR-94   by REAL_MAKE_INIT routine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FILTER_COM.INC'
      CHARACTER*(*) NAME_STR
      EXTERNAL L2JETS
      EXTERNAL L2_EM
      EXTERNAL MUON_L2
      EXTERNAL TOOL1
      EXTERNAL L2ETSUM
      EXTERNAL L2ETMISS
      EXTERNAL L2_PASS_FAIL
      EXTERNAL L2_TEST
      EXTERNAL L2SETUP
      EXTERNAL L2_MIN_BIAS
      EXTERNAL L2_PRESCALE
      EXTERNAL L2_ETACUT
      EXTERNAL L2TAU
      EXTERNAL L2_CONFIRM_L15
      EXTERNAL L2_MASSCUT
      EXTERNAL L2_KEEP_CD_RAW
      EXTERNAL L2_ACOL_JETS
      DATA TOOL_NAMES/
     &   'L2JETS',
     &   'L2_EM',
     &   'MUON_L2',
     &   'TOOL1',
     &   'L2ETSUM',
     &   'L2ETMISS',
     &   'L2_PASS_FAIL',
     &   'L2_TEST',
     &   'L2SETUP',
     &   'L2_MIN_BIAS',
     &   'L2_PRESCALE',
     &   'L2_ETACUT',
     &   'L2TAU',
     &   'L2_CONFIRM_L15',
     &   'L2_MASSCUT',
     &   'L2_KEEP_CD_RAW',
     &   'L2_ACOL_JETS',
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
      TOOL_TABLE(17)=%LOC(L2_EM)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(12)=%LOC(MUON_L2)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(1)=%LOC(TOOL1)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(19)=%LOC(L2ETSUM)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(18)=%LOC(L2ETMISS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(20)=%LOC(L2_PASS_FAIL)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(21)=%LOC(L2_TEST)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(22)=%LOC(L2SETUP)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(25)=%LOC(L2_MIN_BIAS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(26)=%LOC(L2_PRESCALE)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(27)=%LOC(L2_ETACUT)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(28)=%LOC(L2TAU)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(2)=%LOC(L2_CONFIRM_L15)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(32)=%LOC(L2_MASSCUT)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(33)=%LOC(L2_KEEP_CD_RAW)
      MAX_TOOL_USE=MAX_TOOL_USE+1
      TOOL_TABLE(34)=%LOC(L2_ACOL_JETS)
      MAX_TOOL_USE=MAX_TOOL_USE+1
  999 RETURN
      ENTRY GET_L2TYPE_NAME(NAME_STR)
      NAME_STR='FILT_SHADOW'
      RETURN
      END
