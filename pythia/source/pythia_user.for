      SUBROUTINE PYTHIA_USER(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called after events have been generated in 
C-                         D0 Pythia and converted to D0 Zebra event structure.
C-                         It allows the user to suppress writeout 
C-                         of "uninteresting" events. It should be noted 
C-                         that this will adversely affect all cross sections
C-                         calculated within PYTHIA and should be used with
C-                         EXTREME CAUTION. For users interested in cutting
C-                         events based solely on kinematic variables, and
C-                         having the cuts reflected in the cross section
C-                         calculations, please use the routine PYKCUT.
C-                         
C-   Inputs  : PYTHIA event common blocks or D0 ZEBRA banks
C-   Outputs : OK    If .TRUE. event will be written out
C-   Controls: USER defined
C-
C-   Created  25-JAN-1993   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
C----------------------------------------------------------------------
      OK = .TRUE.
  999 RETURN
      END
