      FUNCTION EVT_CUT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allows for user to cut on any event quantites
C-   BESIDES JET INFO and missing ET (see ALLJ_CUT or
C-   INCJ_CUT to cut on jet info or MET)
C-   in order to only create ntuple for events of interest
C-   EX:  Z vertex, N of electrons, MI Tool
C-
C-   Returned value  : TRUE If event is to be written out
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   25-FEB-1994  Andrew G. Brandt
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_ELC.INC/LIST'
      INCLUDE 'D0$INC:QCD_PHO.INC/LIST'
      INCLUDE 'D0$INC:QCD_GLOB.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QCD_JUTL_JETS.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      LOGICAL EVT_CUT
      EVT_CUT = .TRUE.
C
C: User code should set EVT_CUT= .FALSE. if not interested in event
C
      END
