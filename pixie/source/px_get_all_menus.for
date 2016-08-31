      SUBROUTINE PX_GET_ALL_MENUS(PACKAGE_NAME,MENU1,MENU2,ACTION,
     &  REMREC,TOTAL_MENUS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gigen a PIXIE package name returns ALL the Menu 
C-   names in that package with their respective action and the number of 
C-   lines in the remarks of the current RCP file.  If the menu name is a 
C-   combined view the action routine name retunred will be the name of the 
C-   combined view.
C-   This routine assumes that the RCP file is of the form PX_*_RCP 
C-
C-   Inputs  : PACKAGE_NAME[C*]: Name of the package
C-
C-   Outputs : MENU1    [C*(*)]: Name of the first menu
C-             MENU2    [C*(*)]: Name of the second menu
C-             ACTION   [C*(*)]: Name of the action that correspomnd to this
C-                               menu
C-             REMREC       [I]: Number of records in the remarks
C-             TOTAL_MENUS  [I]: Total number of menus
C-                               menu
C-             IER         [I ]: 0 if okay
C-
C-   Created   2-DEC-1991   Lupe Howell
C-   
C-      ENTRY PX_GET_MENU_INDEX(MENU_INDEX,POINTER): Returns the index of 
C-      the requested menu.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE_NAME
      CHARACTER*(*)MENU1(*)
      CHARACTER*(*) MENU2(*)
      CHARACTER*(*)ACTION(*)
      INTEGER REMREC(*),TOTAL_MENUS,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      INTEGER PTR,ITYPE,BEG,ENDD,I,IDX,MENU_PTR(800),TEMP_PTR,TOT_MENUS
      LOGICAL ACTIVE
      CHARACTER*80 TEMP,CVAL
C
      INTEGER IVAL
      REAL    RVAL
      LOGICAL LVAL
      EQUIVALENCE(IVAL,RVAL,LVAL)
C
      SAVE MENU_PTR,TOT_MENUS
C
      INTEGER MENU_INDEX,POINTER
C----------------------------------------------------------------------
      PTR = 1
      ACTIVE = .TRUE.
      TOTAL_MENUS = 0
      ITYPE = VTLOG
      TEMP_PTR = PTR
      DO WHILE ( ACTIVE )
C
C ****  Skipping the logical values
C
        DO WHILE ( ( ITYPE .EQ. VTLOG ) .AND.
     &             ( IER   .EQ. 0     ) )
          CALL EZGET_NEXT_VALUE_TYPE
     &      (PACKAGE_NAME,IVAL,CVAL,ITYPE,LVAL,IER,PTR)
        ENDDO
        IF ( IER .EQ. 0 ) THEN
          TOTAL_MENUS = TOTAL_MENUS + 1
          MENU_PTR(TOTAL_MENUS) = TEMP_PTR
C
C ****  Getting first menu
C
          MENU1(TOTAL_MENUS) = CVAL(1:iVAL)
C
C ****  Getting the second menu name
C
          CALL EZGET_NEXT_VALUE_TYPE
     &      (PACKAGE_NAME,IVAL,CVAL,ITYPE,LVAL,IER,PTR)
          MENU2(TOTAL_MENUS) = CVAL(1:iVAL)
C
C ****  Get the action routine.  If the menu item is a combined view
C ****  return the name of the combined view as the action routine
C
          IF ( MENU2(TOTAL_MENUS)(iVAL:iVAL) .EQ. '%' ) THEN
            ACTION(TOTAL_MENUS) = MENU2(TOTAL_MENUS)
          ELSE
C
C ****  If the view is NOT a combined view get the name of the
C ****  action routine form the PXSCREEN array
C
            CALL PU_GET_SCREEN_INDEX(MENU2(TOTAL_MENUS),IDX,IER)
            CALL PU_GET_SCREEN_ACTION(IDX,ACTION(TOTAL_MENUS),I,IER)
          ENDIF
C
C ****  Skipping the remarks and storing the nuber of records in remarks
C
          DO WHILE( ( ITYPE .NE. VTLOG ) .AND. ( IER .NE. 1) )
            TEMP_PTR = PTR
            CALL EZGET_NEXT_VALUE_TYPE(
     &           PACKAGE_NAME,IVAL,CVAL,ITYPE,LVAL,IER,PTR)
            REMREC(TOTAL_MENUS) = REMREC(TOTAL_MENUS) + 1
          ENDDO
          IF ( IER .NE. 1 ) THEN
            REMREC(TOTAL_MENUS) = REMREC(TOTAL_MENUS) - 1
          ENDIF
        ENDIF
        ACTIVE = ( IER .EQ. 0 )
      ENDDO
      TOT_MENUS = TOTAL_MENUS
C
C ****  Clearing IER flag if not an error
C
      IF ( IER .NE. EZS_PARAM_NOTFOUND ) THEN
        IER  = EZS_SUCCESS
      ENDIF
      RETURN
C#######################################################################
      ENTRY PX_GET_MENU_INDEX(MENU_INDEX,POINTER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the pointer to the menu definition 
C-   of the requested index.
C-
C-   Inputs  : MENU_INDEX [I]: Number of the menu that is requested
C-   Outputs : POINTER    [I]: Index of the menu requested in the menu
C-                             array
C-
C-   Created  12-DEC-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IF ( MENU_INDEX .GT. TOTAL_MENUS ) THEN
        CALL OUTMSG(' The index requested out of range ')
      ELSE
        POINTER = MENU_PTR(MENU_INDEX)
      ENDIF
  999 RETURN
      END
