      SUBROUTINE GET_EVENT_TYPE(EVENT_TYPE,STATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return STATE for different EVENT_TYPEs:
C-   
C-                         EVENT_TYPE: DAQ
C-                                     CDAQ
C-                                     ANY -   DAQ or CDAQ
C-                                     ALL -   DAQ and CDAQ
C-              
C-   Use SET_EVENT_TYPE to set the type from DAQ/FILE or CDAQ read routine.
C-   Use CLEAR_EVENT_TYPE BEFORE event read.
C-              
C-   Inputs  : EVENT_TYPE       [I]
C-   Outputs : STATE            [L]
C-   Controls: None
C-
C-   Created  19-OCT-1990   Boaz Klima
C-   Updated   5-APR-1991   Harrison B. Prosper  
C-      Set default event type to DAQ/FILE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) EVENT_TYPE
      LOGICAL STATE
C
      INTEGER I, L
      LOGICAL SAVED_STATE(3)
      SAVE SAVED_STATE
      DATA SAVED_STATE/.TRUE.,.FALSE.,.FALSE./
C----------------------------------------------------------------------
      L = LEN(EVENT_TYPE)
      IF     ( EVENT_TYPE(1:L) .EQ. 'DAQ' ) THEN
        STATE = SAVED_STATE(1)
      ELSEIF ( EVENT_TYPE(1:L) .EQ. 'CDAQ' ) THEN
        STATE = SAVED_STATE(2)
      ELSEIF ( EVENT_TYPE(1:L) .EQ. 'ALL') THEN
        STATE = SAVED_STATE(1) .AND. SAVED_STATE(2)
      ELSEIF ( EVENT_TYPE(1:L) .EQ. 'ANY') THEN
        STATE = SAVED_STATE(1) .OR. SAVED_STATE(2)
      ENDIF
      RETURN
C
      ENTRY SET_EVENT_TYPE (EVENT_TYPE)
      L = LEN(EVENT_TYPE)
      IF     ( EVENT_TYPE(1:L) .EQ. 'DAQ' ) THEN
        I = 1
      ELSEIF ( EVENT_TYPE(1:L) .EQ. 'CDAQ' ) THEN
        I = 2
      ENDIF
      SAVED_STATE(I) = .TRUE.
      RETURN
C
      ENTRY CLEAR_EVENT_TYPE
      DO I = 1, 3
        SAVED_STATE(I) = .FALSE.
      ENDDO
  999 RETURN
      END
