      FUNCTION D0HPLT_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute D0HPLT.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   2-FEB-1990   Boaz Klima
C-   Updated  19-FEB-1990   Harrison B. Prosper
C-      Use non-inrerrupt menu
C-   Updated   1-MAY-1990   Harrison B. Prosper
C-      Force exit from menu if START updating selected.
C-   Updated  20-FEB-1991   Harrison B. Prosper  
C-      Call d0hpld(command) directly 
C-   Updated   8-MAY-1992   Harrison B. Prosper  
C-      Call d0hindex_refresh to refresh listing 
C-   Updated  13-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D0HPLT_EVENT
      LOGICAL D0HPLT_EVENT1
      LOGICAL FLGVAL,INTAST
C
      INTEGER TRULEN,I,J,N,II,JJ,NN
      CHARACTER*40 COMMAND,OLD_COMMAND
C
      CHARACTER*(*) D0HPLT_MENU
      PARAMETER( D0HPLT_MENU  = 'D0HPLT' )
      CHARACTER*(*) D0HPLT_TITLE
      PARAMETER( D0HPLT_TITLE = 'D0HPLT' )
C
      SAVE OLD_COMMAND
C----------------------------------------------------------------------
      ENTRY D0HPLT_EVENT1
C----------------------------------------------------------------------
      D0HPLT_EVENT1= .TRUE.
      D0HPLT_EVENT = .TRUE.
C
      IF ( FLGVAL ('HISTOGRAM_DISPLAY') ) THEN
C
C ****  Cancel interrupt menu if one is active
C
        CALL CANMEN
C
        CALL D0HINDEX_REFRESH(.TRUE.)
C
C ****  Process D0HPLT commands
C
        COMMAND = ' '
        DO WHILE (COMMAND .NE. 'EXIT')
C
C ****  Present D0HPLT menu
C
          CALL MENUDO(D0HPLT_TITLE,D0HPLT_MENU,COMMAND)
C
          IF     (COMMAND .EQ. 'END HIS MENU') THEN
            COMMAND = 'EXIT'
          ELSEIF (COMMAND .EQ. 'STATUS') THEN
            CALL REPORT_STATUS
          ELSE
C
C ****  Execute D0HPLT command
C
            CALL D0HPLD(COMMAND)
            OLD_COMMAND = COMMAND       ! Save command
C
C ****  Check for command UPLOT (START updating)
C
            IF ( COMMAND .EQ. 'UPLOT' ) THEN
              COMMAND = 'EXIT'          ! Force exit
            ENDIF
          ENDIF
        ENDDO
C
C ****  Cancel display histogram button
C
        CALL FLGSET ('HISTOGRAM_DISPLAY',.FALSE.)
C
      ELSE
C
C ****  Check for updating histograms
C
        IF ( OLD_COMMAND .EQ. 'UPLOT' ) THEN
          CALL D0HPLD(OLD_COMMAND)
        ENDIF
      ENDIF
C
  999 RETURN
      END
