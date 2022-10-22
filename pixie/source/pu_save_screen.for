      SUBROUTINE PU_SAVE_SCREEN (PACKAGE_NAME,IDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save current viewing parameters for the
C-   specified package and command.
C-
C-   Inputs  : PACKAGE_NAME     [C*]    Name of package
C-             IDX              [I]     Screen index
C-             
C-   Outputs : None
C-
C-   ENTRY PU_SAVE_PARAM(PACKAGE_NAME,PARAM_NAME)
C-   ENTRY PU_SAVE_SCREEN_BEGIN
C-   ENTRY PU_RESTORE_SCREEN(BLOCK)
C-   ENTRY PU_RESTORE_PARAM(BLOCK)
C-   ENTRY PU_GET_SAVED_PACKAGE(II,PACAKAGE_NAME)
C-
C-   Created  29-MAY-1991   LUPE HOWELL, Harrison B. Prosper
C-   Updated   3-JUN-1991   Harrison B. Prosper  
C-      Reduce size of param name 
C-   Updated  10-SEP-1991   Lupe Howell  Check if screen parameters already
C-        saved 
C-   Updated  13-JAN-1994   Lupe Howell  PU_RESTOR_PARAM modified so it can
C-      modify character parameters also.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE_NAME
      INTEGER IDX
C----------------------------------------------------------------------
      CHARACTER*(*)PARAM_NAME
      INTEGER BLOCK, BLOCK_COUNT
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER NUMBER_FIELDS
      PARAMETER( NUMBER_FIELDS = 20 )
C
      INTEGER NUMBER_PARAMS,I,J,II,III,JJ,JJJ,OFFSET,VALTYPE,IER,IDDX
      INTEGER TOTAL_BLOCKS ,TOTAL_SCREENS_VALUES
      INTEGER TOTAL_PARAM_VALUES,OLD_TOTAL_BLOCKS 
C
      INTEGER IVALUE
      REAL    RVALUE
      LOGICAL LVALUE
      EQUIVALENCE(IVALUE,RVALUE,LVALUE)
C
      LOGICAL SAVE_IT
C
      INTEGER ERROR
C
C ****  Buffers for saving viewing parameters
C
      INTEGER SAVED_PARAM_COUNT(MAXCVIEW),SAVED_PARAM_TYPE(MAXSAVE_P)
      INTEGER SAVED_PARAM_PTR(MAXCVIEW),SAVED_IDX(MAXCVIEW)
      INTEGER SAVED_ID(MAXCVIEW),PLEN
      REAL SAVED_PARAM_VALUE(MAXSAVE_P),SAVED_SCREEN_VALUE(MAXSAVE_S)
      CHARACTER*32 SAVED_PACKAGE(MAXCVIEW)
      CHARACTER*40 SAVED_PARAM_NAME(MAXSAVE_P)
      CHARACTER*80 SAVED_PARAM_STRING(MAXSAVE_P)
C
      CHARACTER*80 STRING,MESS
      CHARACTER*32 SCREEN_FIELD(NUMBER_FIELDS)
C----------------------------------------------------------------------
      SAVE TOTAL_PARAM_VALUES,TOTAL_SCREENS_VALUES,
     &     TOTAL_BLOCKS ,SAVED_PARAM_VALUE,SAVED_PARAM_TYPE,
     &     OLD_TOTAL_BLOCKS ,SAVED_PARAM_COUNT,SAVED_PACKAGE,
     &     SAVED_PARAM_NAME
C----------------------------------------------------------------------
      DATA SCREEN_FIELD/'PICKABLE' , 'VIEW3D',  'KEEPCIRCLE',
     &            'VPORTXMIN','VPORTXMAX', 'VPORTYMIN', 'VPORTYMAX',
     &            'WINDOWXMIN','WINDOWXMAX','WINDOWYMIN','WINDOWYMAX',
     &            'VIEWREFX',  'VIEWREFY',  'VIEWREFZ', 'UPVECX',
     &            'UPVECY','UPVECZ','CAMX','CAMY','CAMZ'/
C----------------------------------------------------------------------
      CALL WORD(PACKAGE_NAME,I,J,PLEN)
C
C ****  Ckeck if the screen parameters of the requested 
C ****  package has already been saved
C
      I = 1
      DO WHILE ( I .LE. TOTAL_BLOCKS ) 
        IF( (SAVED_PACKAGE(I)(1:PLEN) .EQ. PACKAGE_NAME(1:PLEN) ) .AND.
     &      (SAVED_IDX(I) .EQ. IDX) )THEN
          GO TO 888 
        ENDIF
        I = I + 1
      ENDDO
C
C ****  Save all screen parameters
C
      DO I = 1,  NUMBER_FIELDS
        TOTAL_SCREENS_VALUES = TOTAL_SCREENS_VALUES + 1
        CALL EZ_GET_ELEMENT('PXSCREEN',SCREEN_FIELD(I),
     &     IDX,1,SAVED_SCREEN_VALUE(TOTAL_SCREENS_VALUES),VALTYPE,IER)
      ENDDO
C
C ****  Save package and screen indices
C
      TOTAL_BLOCKS  = TOTAL_BLOCKS  + 1
      SAVED_IDX(TOTAL_BLOCKS ) = IDX
      SAVED_PACKAGE(TOTAL_BLOCKS ) = PACKAGE_NAME
      SAVED_PARAM_COUNT(TOTAL_BLOCKS )  = 0 ! Zero number of parameters saved
      SAVED_PARAM_PTR(TOTAL_BLOCKS )    = 0
  888 RETURN
C
      ENTRY PU_SAVE_PARAM(PACKAGE_NAME,PARAM_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Save parameters for the specified package
C-   and comand
C-
C-   Inputs  : PACKAGE_NAME[C*]:
C-             PARAM_NAME  [C*]: Parameter name to be saved
C-
C-   Outputs : None
C-
C-   Created  29-MAY-1991   LUPE HOWELL
C-
C----------------------------------------------------------------------
C
C ****  Check if current parameter is unique
C
      STRING = PACKAGE_NAME//PARAM_NAME
      CALL LOCSTR1
     &  (STRING,SAVED_PARAM_STRING,SAVED_ID,
     &   TOTAL_PARAM_VALUES,SAVE_IT,II)
C
C ****  Save the parameters which are to be modified
C
      IF ( SAVE_IT ) THEN
        SAVED_PARAM_NAME(TOTAL_PARAM_VALUES) = 
     &    PARAM_NAME(1:LEN(PARAM_NAME))
C
        CALL EZ_GET_ELEMENT('PXPARAMS',PARAM_NAME,
     &      1,1,SAVED_PARAM_VALUE(TOTAL_PARAM_VALUES),VALTYPE,IER)
        SAVED_PARAM_TYPE(TOTAL_PARAM_VALUES)   = VALTYPE
        SAVED_PARAM_COUNT(TOTAL_BLOCKS )  = 
     &  SAVED_PARAM_COUNT(TOTAL_BLOCKS ) + 1
C
C ****  Save the position TOTAL_PARAM_VALUES that points to the 
C ****  first saved parameter
C
        IF ( OLD_TOTAL_BLOCKS  .NE. TOTAL_BLOCKS  ) THEN
          SAVED_PARAM_PTR(TOTAL_BLOCKS ) = TOTAL_PARAM_VALUES      
          OLD_TOTAL_BLOCKS  = TOTAL_BLOCKS 
        ENDIF
                                        
      ENDIF
      RETURN
C
      ENTRY PU_SAVE_SCREEN_BEGIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize variables to start saving parameters
C-   parameters
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  29-MAY-1991   Lupe Howell, Harriosn B. Prosper
C-
C----------------------------------------------------------------------
C
C ****  Set flag to indicate that initialization done.
C
      TOTAL_SCREENS_VALUES  = 0         ! Total number of Screen Values
      TOTAL_PARAM_VALUES    = 0         ! Total number of Parameter Values
      TOTAL_BLOCKS          = 0         ! Total number of Packages
      OLD_TOTAL_BLOCKS      = -1        ! Old total number packages
      RETURN
C
      ENTRY PU_RESTORE_SCREEN(BLOCK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset viewing parameters and values to their
C-   original values.
C-
C-   Inputs  : BLOCK [I]: Loop index of the screen to be restore
C-   Outputs : None
C-
C-   Created  25-SEP-1990   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IDDX = SAVED_IDX(BLOCK)
      OFFSET = NUMBER_FIELDS*(BLOCK-1)  ! Pointer to the data for
                                        ! current block
      DO I = 1,  NUMBER_FIELDS
        JJ = OFFSET + I
        CALL EZ_SET_ELEMENT('PXSCREEN',SCREEN_FIELD(I),
     &     IDDX,1,SAVED_SCREEN_VALUE(JJ),IER)
      ENDDO
      RETURN

      ENTRY PU_RESTORE_PARAM(BLOCK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Restore parameters for a current package.
C-
C-   Inputs  : BLOCK [I]: Loop indexof the parameter to be restor
C-   Outputs : None
C-
C-   Created  29-MAY-1991   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IF ( SAVED_PARAM_COUNT(BLOCK) .LE. 0 ) GOTO 999
C
C ****  Get offset into parameter buffers
C
      OFFSET = SAVED_PARAM_PTR(BLOCK) - 1
      DO I = 1, SAVED_PARAM_COUNT(BLOCK)
        JJ = OFFSET + I
C
C ****  Use equivalences to set correct type
C
        RVALUE = SAVED_PARAM_VALUE(JJ)
        IF     ( SAVED_PARAM_TYPE(JJ) .EQ. VTINT ) THEN
          CALL EZ_SET_ELEMENT_i('PXPARAMS',SAVED_PARAM_NAME(JJ),
     &               1,1,IVALUE,ERROR)
        ELSEIF ( SAVED_PARAM_TYPE(JJ) .EQ. VTREAL ) THEN
          CALL EZ_SET_ELEMENT('PXPARAMS',SAVED_PARAM_NAME(JJ),
     &               1,1,RVALUE,ERROR)
        ELSEIF ( SAVED_PARAM_TYPE(JJ) .EQ. VTLOG ) THEN
          CALL EZ_SET_ELEMENT_l('PXPARAMS',SAVED_PARAM_NAME(JJ),
     &               1,1,LVALUE,ERROR)
        ELSEIF ( SAVED_PARAM_TYPE(JJ) .GE. VTCHAR ) THEN
          CALL EZ_SET_ELEMENT_i('PXPARAMS',SAVED_PARAM_NAME(JJ),
     &               1,1,IVALUE,ERROR)
        ENDIF
        IF ( ERROR .NE. 0 ) THEN
          MESS = ' PU_RESTORE_PARAMS: Unable to set '//
     &           SAVED_PARAM_NAME(JJ) 
          CALL INTMSG(MESS)
        ENDIF
      ENDDO
      RETURN
C
      ENTRY PU_GET_SAVED_PACKAGE(BLOCK,PACKAGE_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retruns the package name corresponding to 
C-   the given index
C-
C-   Inputs  : BLOCK        [I]: Index
C-   
C-   Outputs : PACKAGE_NAME [C*]: Package name corresponding to the index
C-
C-   Created  29-MAY-1991   Lupe Howell, Harrison B. Prospoer
C-
C----------------------------------------------------------------------
      PACKAGE_NAME = SAVED_PACKAGE(BLOCK)
      RETURN
C
      ENTRY PU_GET_SAVED_BLOCK_COUNT(BLOCK_COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Retruns the total number of blocks 
C-
C-   Inputs  : None 
C-   
C-   Outputs : BLOCK_COUNT [I]: Total Blocks saved
C-
C-   Created  29-MAY-1991   Lupe Howell, Harrison B. Prospoer
C-
C----------------------------------------------------------------------
      BLOCK_COUNT = TOTAL_BLOCKS
  999 RETURN
      END
