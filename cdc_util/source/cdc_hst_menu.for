      FUNCTION CDC_HST_MENU()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Inputs CDC histogram selection items into 
C-                         EXAMINE menu
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  18-JAN-1991   Qizhong Li-Demarteau
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  moved LOGICAL from routine
C-                                         name to Type Declaration Statement
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CDC_HST_MENU
      LOGICAL PBD_TEMP_FLAG,VALUE
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER OFF*2
      DATA OFF/'X-'/
C----------------------------------------------------------------------
      CDC_HST_MENU = .TRUE.

      IF ( PBD_TEMP_FLAG('CDC',VALUE) ) THEN
        MENNAM = 'CDC histograms'
        MENCOM = 'CDC HISTOGRAMS'
        IF ( .NOT. VALUE ) THEN
          MENNAM = OFF//'CDC histograms' 
          MENCOM = OFF//'CDC HISTOGRAMS'
        ENDIF
        CALL MENADD('EXAMINE',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '        Select histograms for CDC.                '//
     &   ' '   )
      END IF
  999 RETURN
      END
