      LOGICAL FUNCTION HPLOT_PROC_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inputs HPLOT items into PROCESS menu
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  18-JUL-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER*2  OFF
      LOGICAL PBD_TEMP_FLAG,ON
      DATA OFF/'X-'/
C----------------------------------------------------------------------
      HPLOT_PROC_MENU = .TRUE.
      IF ( PBD_TEMP_FLAG('HPLOT',ON) ) THEN
        MENNAM = 'Histogram Display'
        MENCOM = 'HISTOGRAM_DISPLAY'
        IF ( .NOT. ON ) THEN
          MENNAM = OFF//'Histogram Display'
          MENCOM = OFF//'HISTOGRAM_DISPLAY'
        ENDIF
        CALL MENADD('PROCESS',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '      Switch to the HPLOT menu D0HPLT. Histograms can be'//
     &   ' viewed after, as well as during, data taking.'//
     &   ' '   )
      END IF
  999 RETURN
      END
