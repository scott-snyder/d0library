      SUBROUTINE WRITE_LOGIC(WRITE_THIS_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decide whether the event output stream
C-      is controlled by the user or not, and whether to write this
C-      particular event.
C-          There are 2 flags in CALFRAME_RCP which affect event writing:
C               WRITE_STREAM_STA   write the standard output
C               WRITE_STREAM_DST   Write the DST output
C       In addition, there is the flag WRITE_THIS_EVENT which the
C       user can set to override the CALFRAME selections.  
C
C       The following options exist:
C
C       (1) At least one of WRITE_STREAM_STA, WRITE_STREAM_DST .TRUE.
C           in CALFRAME_RCP.
C               The output file(s) are opened and begin/end run records
C               are always written. 
C               (a) The user touches no flags: All events are written to
C                   the selected stream(s).
C               (b) The user sets the flag 'WRITE_THIS_EVENT' .FALSE. on
C                   the first call to his processing routine. No events
C                   are written except those for which the user sets 
C                   'WRITE_THIS_EVENT .TRUE. in his processing routine
C                   for that event.
C               (c) The user mainpulates the flags WRITE_STREAM_STA and
C                   WRITE_STREAM_DST directly to delete events from the
C                   streams. ** NOTE ** if the user sets these flags
C                   .FALSE. for a particular event, he is responsible
C                   for resetting them .TRUE. in order to get any more
C                   events output.
C       (2) Both WRITE_STREAM_STA and WRITE_STREAM_DST are .FALSE. in
C           CALFRAME_RCP.
C               The output files are NOT opened.  No events of any
C               type are output. The user cannot start an output
C               stream by setting 'WRITE_THIS_EVENT' .TRUE.
C-
C-   Inputs  : None
C-   Outputs : WRITE_THIS_EVENT  .TRUE. if this event should be
C-                               written to the enabled output stream(s)
C-
C-   Created  20-SEP-1990   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL WRITE_THIS_EVENT,DEFAULT,USER_REQUEST,FLGVAL,VALUE
      LOGICAL ORIG_DEFAULT
C----------------------------------------------------------------------
      USER_REQUEST = FLGVAL('WRITE_THIS_EVENT')
      IF ( DEFAULT ) THEN
        IF (.NOT. USER_REQUEST) DEFAULT = .FALSE.
        WRITE_THIS_EVENT = DEFAULT
      ELSE
        IF (.NOT. ORIG_DEFAULT) GO TO 999
        WRITE_THIS_EVENT = USER_REQUEST
        CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
      ENDIF
  999 RETURN
C
      ENTRY SET_WRITE_DEFAULT(VALUE)
C
      DEFAULT = VALUE
      ORIG_DEFAULT = VALUE
      END
