      FUNCTION PCAPHEL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-              CALDIS (called as a sub-menu of CALDIS).
C-   
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   PXBUILD V1.00
C-   Updated  28-JAN-1991   Lupe Howell ISAJETS tracks command was added 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PCAPHEL
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'Electron/Photon Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'CAPHEL' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PCAPHEL = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'TOWER LEGO CATE' )  THEN
          CALL PCATEL
        ELSEIF ( COMMAND .EQ. 'CLUSTR LEGO CACL' ) THEN
          CALL PCJETC
        ELSEIF ( COMMAND .EQ. 'CELL CAL 3D CAEP' ) THEN
          CALL PC3DCL
        ELSEIF ( COMMAND .EQ. 'CLUSTER 3D CACL')   THEN
          CALL PC3DJC
        ELSEIF ( COMMAND .EQ. 'ELEC_LEGO PECL' )   THEN
          CALL PC_PELC_LEGO
        ELSEIF ( COMMAND .EQ. 'ELEC_3D PELC' )     THEN
          CALL PC3DELEC
        ELSEIF ( COMMAND .EQ. 'ELEC_CD (R-PHI)' )  THEN
          CALL PC_ELECD 
        ELSEIF ( COMMAND .EQ. 'ISAJET TRACKS')    THEN
          CALL PCISATRACK
        ENDIF
      ENDDO
  999 RETURN
      END
