      FUNCTION HPLOT_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inputs HPLOT items into EXAMINE menu
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  17-JUL-1990   Chip Stewart
C-   Updated  22-FEB-1991   Harrison B. Prosper  
C-      Add call to MENU_D0HPLT (taken from MENU_SETUP) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL HPLOT_MENU
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER OFF*2
      LOGICAL PBD_TEMP_FLAG,ON,FIRST
      DATA OFF/'X-'/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MENU_D0HPLT                ! Setup D0HPLT menu.
      ENDIF
C
      HPLOT_MENU = .TRUE.
      IF ( PBD_TEMP_FLAG('HPLOT',ON) ) THEN
        MENNAM = 'Histogram Display'
        MENCOM = 'HISTOGRAM DISPLAY'
        IF ( .NOT. ON ) THEN
          MENNAM = OFF//'Histogram Display'
          MENCOM = OFF//'HISTOGRAM DISPLAY'
        ENDIF
        CALL MENADD('EXAMINE',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '      Switch to the HPLOT menu D0HPLT. Histograms can be'//
     &   ' viewed after, as well as during, data taking.'//
     &   ' '   )
      END IF
  999 RETURN
      END
