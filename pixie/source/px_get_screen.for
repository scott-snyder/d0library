      SUBROUTINE PX_GET_SCREEN(ARRAY_NAME,IDX,ARRAY_ELEMENT,
     &            VAL,VALTYPE,REM,TOTPARAM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the all the screen parameters of a given
C-   screen that starts at IDX. The PXSCREEN array is defined in screen groups
C-   that are defined by the triplet
C-   (param-name,value,remark) the array has the following
C-   structure:
C-
C-   \ARRAY array-name
C-      'NAME'           'scren_name1'   'Remark'
C-      'screen_element1' value          'remark'
C-      'screen_element2' value          'remark'
C-              :       :       :
C-      'NAME'           'scren_name2'   'Remark'
C-      'screen_element1' value          'remark'
C-      'screen_element2' value          'remark'
C-              :       :       :
C-   \END
C-
C-   Inputs  : ARRAY_NAME    [C*]: Name of the array
C-             IDX           [I ]: Index in the array where to start the
C-                                 getting the screen parameters
C-             ARRAY_ELEMENT[C*(*) ]: If the first element of this input
C-                                 paramere equals 'SKIP' the element 'ACTION'
C-                                 will be left out of the list.
C-
C-   Outputs : ARRAY_ELEMENT [C*(*)]: Array containing the names of the screen
c-                                    parameters
C-             VAL           [I (*)]: Array containg the Values of the screen
C-                                    parameters 
C-             VALTYPE       [I (*)]: Array containg the Types of the value
C-             IER           [I ]     If it does not equal to 0 the element was
C-                                    not found
C-             TOTPARAM         [I ]: Total number of screen parameters found
C-
C-   Created  13-MAR-1991   LUPE HOWELL   
C-   Updated  27-NOV-1991   Lupe Howell  Exclude the element 'ACTION' from the 
C-   list if ARRAY_ELEMENT(1)='SKIP'
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT(*)
      CHARACTER*(*) REM(*)
      INTEGER VALTYPE(*),IDX,VAL(*),TOTPARAM,IER

      INTEGER MAXSIZE,ID
      PARAMETER ( MAXSIZE=4000 )
      INTEGER LOCAL(MAXSIZE), TOTAL, ITYPE(MAXSIZE), IPOINT,SKIP
      INTEGER LENGTH
      CHARACTER*32 NAME,CHARVAL
      INTEGER VTCHR
      PARAMETER( VTCHR = 10 )
C----------------------------------------------------------------------
      IPOINT = IDX
      IER    = 0
      TOTPARAM = 0
C
C ****  Check if skip ACTION element
C
      SKIP = .FALSE.
      IF ( ARRAY_ELEMENT(1) .EQ. 'SKIP' ) THEN
        SKIP =.TRUE.
      ENDIF
C
C ****  Getting the values and types into local array
C
      CALL EZGET_VALUE_TYPE(ARRAY_NAME,LOCAL,ITYPE,TOTAL,IER)
      CALL EZGETI (ARRAY_NAME,ID,IER)
      IF( IER  .NE. 0 ) GOTO 999
C
C ****  Skipping the Name of the screen array and its remarks
C
      CALL EZGETC1 (LOCAL,ITYPE,IPOINT,NAME,LENGTH)
      IF( ITYPE(IPOINT) .GT. VTCHR ) THEN          ! Skipping the value
        CALL EZGETC1(LOCAL,ITYPE,IPOINT,CHARVAL,LENGTH)
      ELSE
        IPOINT = IPOINT + 1
      ENDIF
      CALL EZGETC1(LOCAL,ITYPE,IPOINT,CHARVAL,LENGTH)      ! Getting remark
      CALL EZGETC1 (LOCAL,ITYPE,IPOINT,NAME,LENGTH) ! Getting the next name
C
C ****  Getting all the screen parameters
C ****  Check is skip 'ACTION' element 
C
      DO WHILE(( NAME(1:LENGTH) .NE. 'NAME' ).AND.( IPOINT .LT. TOTAL))
        IF( ( SKIP ) .AND. ( NAME(1:LENGTH)  .EQ. 'ACTION' )) THEN
          CALL EZGETC1(LOCAL,ITYPE,IPOINT,CHARVAL,LENGTH) ! Skipping the val
          CALL EZGETC1(LOCAL,ITYPE,IPOINT,CHARVAL,LENGTH) ! Skippin Remark
          CALL EZGETC1 (LOCAL,ITYPE,IPOINT,NAME,LENGTH)   ! Getting next name
        ELSE
          TOTPARAM = TOTPARAM + 1
          ARRAY_ELEMENT(TOTPARAM) = NAME(1:LENGTH)
          VAL(TOTPARAM)     = LOCAL(IPOINT)
          VALTYPE(TOTPARAM) = ITYPE(IPOINT)
          IF( ITYPE(IPOINT) .GT. VTCHR ) THEN          ! Skipping the value
            CALL EZGETC1(LOCAL,ITYPE,IPOINT,CHARVAL,LENGTH)
          ELSE
            IPOINT = IPOINT + 1
          ENDIF
          CALL EZGETC1(LOCAL,ITYPE,IPOINT,REM(TOTPARAM),LENGTH) ! Getting remark
          CALL EZGETC1 (LOCAL,ITYPE,IPOINT,NAME,LENGTH)   ! Getting the name
        ENDIF
      ENDDO
  999 RETURN
      END
