      LOGICAL FUNCTION TRD_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inputs TRD extra items into EXAMINE menu
C-
C-   Returned value  : TRUE
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  13-SEP-1990  J-Fr Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER OFF*2
      LOGICAL PBD_TEMP_FLAG,ON
      DATA OFF/'X-'/
      TRD_MENU = .TRUE.
      IF ( PBD_TEMP_FLAG('TRD',ON) ) THEN
        MENNAM = 'TRD user dialog'
        MENCOM = 'TRD USER DIALOG'
        IF ( .NOT. ON ) THEN
C Follows the convention by Chip Stewart
          MENNAM = OFF//'TRD user dialog'
          MENCOM = OFF//'TRD user dialog'
        ENDIF
        CALL MENADD('EXAMINE',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '      Use this command to reselect options'//
     &   'in the TRD RCP file'//
     &   ' '   )
      ENDIF
C
  999 RETURN
      END
