      LOGICAL FUNCTION CD_PULSER_PROC_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inserts PULSER CONTROL into PROCESS menu
C-                         for CD Examines
C-
C-   Returned value  : .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  5-NOV-1990   Susan K. Blessing   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER*2  OFF
      LOGICAL PBD_TEMP_FLAG,ON
      DATA OFF/'X-'/
C----------------------------------------------------------------------
      CD_PULSER_PROC_MENU = .TRUE.
C
      IF (PBD_TEMP_FLAG('ZPULSER',ON)) THEN
        MENNAM = 'Pulser Control'
        MENCOM = 'PULSER_CONTROL'
        IF (.NOT. ON) THEN
          MENNAM = OFF//'Pulser Control'
          MENCOM = OFF//'PULSER_CONTROL'
        END IF
        CALL MENADD('PROCESS',.TRUE.,
     &    MENNAM,
     &    MENCOM,
     &    '      Transfer control to Pulser menu.'//
     &    ' '   )
      END IF
C
  999 RETURN
      END
