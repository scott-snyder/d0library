      FUNCTION ALLJ_CUT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Allows for user to cut on jet info
C-   or missing ET in order to only create ntuple for events of interest
C-   EX:  NJETS, JET ET, ETA
C-
C-   Returned value  : TRUE If event is to be written out
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   25-FEB-1994  Andrew G. Brandt
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_NUT.INC/LIST'
      LOGICAL ALLJ_CUT
      ALLJ_CUT = .TRUE.
C
C: User code should set ALLJ_CUT= .FALSE. if not interested in event
C
      END
