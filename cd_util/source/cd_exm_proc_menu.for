      LOGICAL FUNCTION CD_EXM_PROC_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inserts PULSER CONTROL into PROCESS menu
C-                         for CD Electronics Examine
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
      CD_EXM_PROC_MENU = .TRUE.
C
      MENNAM = 'Pulser Control'
      MENCOM = 'PULSER_CONTROL'
      CALL MENADD('PROCESS',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '      Transfer control to Pulser menu.'//
     &   ' '   )
C
  999 RETURN
      END
