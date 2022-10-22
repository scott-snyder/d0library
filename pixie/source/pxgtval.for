      SUBROUTINE PXGTVAL(NUMBER, TYPE, STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks the type of NUMBER and write its value
C-   acording to it in STRING variable.
C-
C-   Inputs  : NUMBER - Number that has to be converted to a string
C-             TYPE   - Type of NUMBER
C-
C-   Outputs : STRING - String that contains the number
C-
C-   Created   9-APR-1990   Lupe Howell
C-   Updated   4-FEB-1991   Lupe Howell  The hollorith conversion was debugged
C-      to make possible the conversion when character parameters are used
C-   Updated  29-MAR-1991   Lupe Howell  Using EZ_SET_ELEMENT and IDX for
C-   seting parameters in PXRDVAL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      REAL    NUMBER, VALU
      INTEGER TYPE, LLL, IVAL, II, JJ, LL, J, K
      LOGICAL LVAL
      CHARACTER*(*) STRING
      CHARACTER*23 STRG
      EQUIVALENCE ( IVAL, VALU, LVAL )
      CHARACTER*10 TEMP
C
      CHARACTER*(*)NAME
      CHARACTER*(*)VALUE
      CHARACTER*(*) ARRAY
      CHARACTER*4 CVAL,TEMP_STRG
      CHARACTER*20 RESP
      INTEGER PARTYPE, IER2, START,END,LENGTH, IER, CHAR_SIZE,IDX
      DATA CHAR_SIZE/4/
C----------------------------------------------------------------------
C
C ****  Checking the type of number
C
      VALU = NUMBER
      IF (     TYPE .EQ. VTINT ) THEN           ! Integer
        WRITE(TEMP,100) IVAL
  100   FORMAT(I10)
      ELSEIF ( TYPE .EQ. VTREAL ) THEN          ! Real
        WRITE(TEMP,110) NUMBER
  110   FORMAT(F10.4)
      ELSEIF ( TYPE .EQ. VTLOG ) THEN           ! Logical
        WRITE(TEMP(1:10),120) LVAL
  120   FORMAT(L10)
      ELSEIF ( TYPE .GT. VTCHR ) THEN           ! Character
        CALL DHTOC(CHAR_SIZE,IVAL,TEMP_STRG)
        TEMP = TEMP_STRG(1:CHAR_SIZE)
      ENDIF
      CALL SWORDS(TEMP,II,JJ,LL)
      STRING = TEMP(II:JJ)
      RETURN
C------------------------------------------------------------
C
C ****  Entry PXRDVAL - It places a string value and stores it as a 
C ****  RCP array element according to its type.
C
C  Input: NAME     - Name of the element in the array that is going to be set
C         VALUE    - String containg the value of the parameter that 
C                    is going to be change in the format
C         PARTYPE  - Type of the parameter that is going to be changed
C         ARRAY    - Name of the array in the RCP file that has the element
C                    that is to be changed
C         IDX      - Index in the array where to start the search to set the
C                    element
C
C  Output: IER2    - If nothing was entered IER2 = 2 otherwise, =0
C
C-------------------------------------------------------------
      ENTRY PXRDVAL(NAME,VALUE,PARTYPE,ARRAY,IDX,IER)
      CALL SWORDS(VALUE,START,END,LENGTH)
        IER2 = 0
        IF (     PARTYPE .EQ. VTINT ) THEN        ! Integer
          READ( VALUE(1:LENGTH), * ) IVAL 
          CALL EZ_SET_ELEMENT_i(ARRAY,NAME,IDX,1,IVAL,IER)
        ELSEIF ( PARTYPE .EQ. VTREAL ) THEN       ! Real
          READ( VALUE(1:LENGTH), * ) VALU  
          CALL EZ_SET_ELEMENT(ARRAY,NAME,IDX,1,VALU,IER)
        ELSEIF ( PARTYPE .EQ. VTLOG ) THEN        ! Logical
          READ( VALUE(1:LENGTH), * ) LVAL   
          CALL EZ_SET_ELEMENT_l(ARRAY,NAME,IDX,1,LVAL,IER)
        ELSEIF ( PARTYPE .GT. VTCHR ) THEN        ! Character
          CVAL='    '
          READ( VALUE(1:LENGTH), 2120 ) CVAL
 2120     FORMAT(A4)
          CALL DCTOH (4,CVAL,IVAL,LLL)    ! Convert to hollarit
          CALL EZ_SET_ELEMENT(ARRAY,NAME,IDX,1,VALU,IER)
        ENDIF
  999 RETURN
      END
