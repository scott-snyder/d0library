      SUBROUTINE PXMOD_SCREEN(LAST_RCP,PARA_NAME,PARA_VAL,PARATY,
     &           PARA_REM,TOTPARAM,IDX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LAST_RCP        [C*]: Name of the last RCP file 
C-   
C-   Outputs : PARA_NAME    [C*(*)]: Names of the screen parameters of selected
C-             PARA_PARA_VAL[I(*) ]: Values of the screen parameters
C-             PARATY       [I(*) ]: Types of the parameters' values
C-             TOTPARAM     [I    ]: Total number of screen parameters
C-             IDX             [I ]: Index of the array chosen
C-             
C-
C-   Controls: IER             [I ]: Error flag 0 ok
C-
C-   Created  13-MAR-1991   LUPE HOWELL
C-   Updated  29-MAR-1991   Lupe Howell  Return Index as a parameter 
C-   Updated  15-MAY-1991   Harrison B. Prosper  
C-      Changed arguments in PU_GOTO_SCREEN
C-   Updated  11-OCT-1991   Lupe Howell  If a combined view is been modified 
C-      replace the values of the screen parameters with the ones found in teh
C-      combined view.
C-   Updated  27-NOV-1991   Lupe Howell  input parameter to PX_GET_SCREENS call
C-   modified so it will skip element 'ACTION' in the screen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARA_NAME(*)
      CHARACTER*(*) PARA_REM(*)
      CHARACTER*(*) LAST_RCP
      INTEGER PARA_VAL(*),PARATY(*),TOTPARAM,IDX,IER
C
      CHARACTER*40 SCREENAME,COMBINED_ARRAY
      INTEGER SCRENUM,NSCREEN
      INTEGER TOTPARAMS,I,J,VALTYPE,LSCRE
      LOGICAL COMBINED
      PARAMETER( TOTPARAMS = 20 )
C----------------------------------------------------------------------
C
C ****  Find out if the Modify Screen command comes from 
C ****  a combined view if so get the name of teh combined 
C ****  array
C
      CALL PXCOMB_VIEW(COMBINED,COMBINED_ARRAY)
C
C ****  Display Screens availables in RCP file and get selection
C ****  from the user
C
      IDX = 0
      CALL PX_DISPLAY_SCREEN
     &  (COMBINED,SCRENUM,SCREENAME,NSCREEN,IER)
      IF ( SCRENUM .NE. 0 ) THEN
        CALL PU_GOTO_SCREEN(SCRENUM,IDX)
        PARA_NAME(1) = 'SKIP'
        CALL PX_GET_SCREEN('PXSCREEN',IDX,PARA_NAME,
     &              PARA_VAL,PARATY,PARA_REM,TOTPARAM,IER)
        CALL STAMSG(' '//SCREENAME,.TRUE.)
C
C ****  Getting screen parameter values from the combined view
C ****  if MODIFY_VIEW
C
        IF ( COMBINED ) THEN
          CALL EZPICK(LAST_RCP)
          CALL  PXGET_COMBINED_PARAM(COMBINED_ARRAY,PARA_NAME,
     &        PARA_VAL,TOTPARAM,IER)
          CALL EZRSET
        ENDIF
      ELSE
        IER = -1
      ENDIF
  999 RETURN
      END
