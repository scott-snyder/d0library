C----------------------------------------------------------------------
C-
C-    Variables taken from the file L1SIM_RCP which need to be shared among
C-    several routines.
C-
C-   Created  22-OCT-1991   Philippe Laurens, Steven Klocek
C-   Updated  10-DEC-1991   K.Bazizi add MUON programming file
C-   Updated  13-DEC-1991   P.Laurens, S.Klocek, Add Level 1.5 
C-   Updated  24-FEB-1992   P.Laurens, S.Klocek, Add FORCE_VERTEX_CENTER
C-   Updated  23-SEP-1992   P.Laurens, S.Klocek, Add TRIGGER_TOWER_SATURATION
C-                             Add NUM_FORCED_TERMS, FORCED_TERM_STATES
C-                             (Constant-state Andor Term simulation).
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add variables for andor terms copied from an
C-                      existing TRGR bank found in the input event
C-                      Add switch to copy level 0 crate from input event.
C-                      Add switch to create the Jet Lists.
C-   Updated   4-MAR-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch PRESCALER_USE_EVENT_DATA 
C-   Updated  14-NOV-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch CLIP_L1CT_AT_ETA_16 to enable simulation 
C-                      of clipped coverage at |eta| > 16 
C-
C----------------------------------------------------------------------
C
      INTEGER LOOKUP_TABLE_FILE_NAME_LENGTH
      INTEGER PROGRAMMING_FILE_NAME_LENGTH
      INTEGER MU_PROGRAMMING_FILE_NAME_LENGTH
      INTEGER RESOURCE_FILE_NAME_LENGTH
      INTEGER BLS_FILE_NAME_LENGTH
      LOGICAL USE_BLS_GAIN_CORRECTION
      LOGICAL DO_PROGRAMMING_LISTING
      LOGICAL DO_FAST_CALTRIG
      LOGICAL APPLY_NOISE
      LOGICAL APPLY_PRESCALER
      LOGICAL PRESCALER_USE_EVENT_DATA
      LOGICAL FORCE_VERTEX_CENTER
      LOGICAL L15_CERTIFIED
      LOGICAL COPY_L0_CRATE
      LOGICAL CREATE_JET_LISTS
      LOGICAL FORCED_TERM_STATES(1:256)
      INTEGER NUM_FORCED_TERMS
      INTEGER NUM_COPIED_TERMS
      LOGICAL CLIP_L1CT_AT_ETA_16
      REAL    TRIGGER_TOWER_SATURATION(1:2, 1:20)
C
      COMMON /L1SIM_CONTROL/ LOOKUP_TABLE_FILE_NAME_LENGTH,
     &  PROGRAMMING_FILE_NAME_LENGTH,
     &  MU_PROGRAMMING_FILE_NAME_LENGTH,
     &  RESOURCE_FILE_NAME_LENGTH,
     &  BLS_FILE_NAME_LENGTH,
     &  USE_BLS_GAIN_CORRECTION,
     &  DO_PROGRAMMING_LISTING,
     &  DO_FAST_CALTRIG,
     &  APPLY_NOISE,
     &  APPLY_PRESCALER,
     &  FORCE_VERTEX_CENTER,
     &  L15_CERTIFIED,
     &  COPY_L0_CRATE,
     &  CREATE_JET_LISTS,
     &  NUM_FORCED_TERMS, FORCED_TERM_STATES,
     &  NUM_COPIED_TERMS, 
     &  TRIGGER_TOWER_SATURATION,
     &  PRESCALER_USE_EVENT_DATA,
     &  CLIP_L1CT_AT_ETA_16
