      FUNCTION PSEXEC( )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXEC Interface Routine for PIXIE package
C-                         SAMDIS 
C-
C-   Called from the PIXIE hook PXEXEC.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created 11-JUN-1991, S. Hagopian
C-                        based on PMEXEC
C-   Updated   8-NOV-1991   Lupe Howell  Two views were combined  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PSEXEC
C----------------------------------------------------------------------
      CHARACTER*(*) TITLE
      PARAMETER( TITLE   = 'SAMUS Event Display' )
      CHARACTER*(*) MENNAM
      PARAMETER( MENNAM  = 'SAMDIS' )
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      PSEXEC = .TRUE.
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL PUMENUDO(TITLE,MENNAM,COMMAND)
        IF     ( COMMAND .EQ. 'YZ SAMUS N'    ) THEN
          CALL SAMUS_YZ_N
        ELSEIF ( COMMAND .EQ. 'XY N A SAMUS'    ) THEN
          CALL SAMUS_XY_N_A
        ELSEIF ( COMMAND .EQ. 'XY N B SAMUS'    ) THEN
          CALL SAMUS_XY_N_B
        ELSEIF ( COMMAND .EQ. 'XY N C SAMUS'    ) THEN
          CALL SAMUS_XY_N_C
        ELSEIF ( COMMAND .EQ. 'ZX SAMUS N'    ) THEN
          CALL SAMUS_ZX_N
        ELSEIF ( COMMAND .EQ. 'YZ SAMUS S'    ) THEN
          CALL SAMUS_YZ_S
        ELSEIF ( COMMAND .EQ. 'XY S A SAMUS'    ) THEN
          CALL SAMUS_XY_S_A
        ELSEIF ( COMMAND .EQ. 'XY S B SAMUS'    ) THEN
          CALL SAMUS_XY_S_B
        ELSEIF ( COMMAND .EQ. 'XY S C SAMUS'    ) THEN
          CALL SAMUS_XY_S_C
        ELSEIF ( COMMAND .EQ. 'ZX SAMUS S'    ) THEN
          CALL SAMUS_ZX_S
        ELSEIF ( COMMAND .EQ. 'XY TK S SAMUS'    ) THEN
          CALL SAMUS_XY_TK_S
        ELSEIF ( COMMAND .EQ. 'XY TK N SAMUS'    ) THEN
          CALL SAMUS_XY_TK_N
        ENDIF
      ENDDO
  999 RETURN
      END
