      FUNCTION PCEXEC ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Driving routine for the Calorimeter
C-   EVENT DISPLAY. Interaction is via COMPACK menus and parameters
C-   from the RCP bank PX_CALDIS_RCP.
C-
C-   Created   2-MAY-1990   Lupe Howell
C-   Updated   6-SEP-1990   Harrison B. Prosper
C-      Re-structure
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Put PUPHI code into PCEVEN
C-   Updated   4-DEC-1990   Harrison B. Prosper
C-      Added PCISATRACK and PU3ROT
C-   Modified 18-AUG-1991   Nobuaki Oshima
C-      Two menu names('3D CAL CELLS' & '3D DST DISPLAY' ) were changed.
C-   Updated 17-SEP-1991 Sharon Hagopian
C-      Added PCDST_LEGO
C-   Modified 13-JAN-1992   Nobuaki Oshima
C-      Add LEGO CAL CATD and CATRIG$ and move PCDST_LEGO to PHYDIS.
C-   Modified 13-JUL-1992   Nobuaki Oshima
C-      Add hidden menu 'GET CAL ENERGY' to get a maximum E/Et for CAL.
C-      combined views.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'CALORIMETER Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CALDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PCEXEC = .TRUE.
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'END VIEW CALORIMETER' ) THEN
          CALL PCEVEN
        ELSEIF ( COMMAND .EQ. 'ETA HISTO CALORIMETER' ) THEN
          CALL PCSVHIST
        ELSEIF ( COMMAND .EQ. 'SIDE VIEW CALORIMETER' ) THEN
          CALL PCSVEN
        ELSEIF ( COMMAND .EQ. '3D CAL CELLS'          ) THEN
          CALL PC3DCL
        ELSEIF ( COMMAND .EQ. 'LEGO CAL CAEP'         ) THEN
          CALL PCALEG
        ELSEIF ( COMMAND .EQ. 'EMAP CAL CAEP'         ) THEN
          CALL PCEMAP
        ELSEIF ( COMMAND .EQ. 'CAL LAYER LEGO'        ) THEN
          CALL PCLAYL
        ELSEIF ( COMMAND .EQ. 'LEGO CAL CATD'         ) THEN
          CALL PCATDL
        ELSEIF ( COMMAND .EQ. 'EM LAYER HIST'         ) THEN
          CALL PCEMHST
        ELSEIF ( COMMAND .EQ. 'GET CAL ENERGY'        ) THEN
          CALL PCENERGY
        ELSEIF ( COMMAND .EQ. 'CAJETS$'               ) THEN
          CALL PCAJETS
        ELSEIF ( COMMAND .EQ. 'CAPHEL$'               ) THEN
          CALL PCAPHEL
        ELSEIF ( COMMAND .EQ. 'CATRIG$'               ) THEN
          CALL PCATRIG
        ENDIF
      ENDDO
C
  999 RETURN
      END
