C----------------------------------------------------------------------
C-
C-    Variables taken from the file L1SIM_RCP which need to be shared among
C-    several routines.
C-
C-    The strings must reside in a seperate common block
C-
C-   Created  22-OCT-1991   Philippe Laurens, Steven Klocek
C-   Updated  10-DEC-1991   K.Bazizi add MUON programming file
C-   Updated   2-MAR-1992   Philippe Laurens, Steven Klocek 
C-                            Add L1SIM_INPUT_SOURCE.
C-   Updated  23-SEP-1992   P.Laurens, S.Klocek, 
C-                          Add variables for Forced Andor Terms
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add variables for andor terms copied from an
C-                      existing TRGR bank found in the input event
C----------------------------------------------------------------------
C
      CHARACTER*132 LOOKUP_TABLE_FILE_NAME
      CHARACTER*132 PROGRAMMING_FILE_NAME
      CHARACTER*132 MU_PROGRAMMING_FILE_NAME
      CHARACTER*132 RESOURCE_FILE_NAME
      CHARACTER*132 BLS_FILE_NAME
      CHARACTER*4   ZEBRA_PATH
      CHARACTER*4   L1SIM_INPUT_SOURCE
      CHARACTER*32  FORCED_TERM_NAMES(1:256)
      CHARACTER*32  COPIED_TERM_NAMES(1:256)
C
      COMMON /L1SIM_CONTROL_STRINGS/ LOOKUP_TABLE_FILE_NAME,
     &  PROGRAMMING_FILE_NAME,
     &  MU_PROGRAMMING_FILE_NAME,
     &  RESOURCE_FILE_NAME,
     &  BLS_FILE_NAME,
     &  ZEBRA_PATH,
     &  L1SIM_INPUT_SOURCE,
     &  FORCED_TERM_NAMES,
     &  COPIED_TERM_NAMES
