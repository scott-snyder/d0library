      SUBROUTINE PU_GET_SCREEN_PARAM( IDX, PARNAM, IVAL , IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the value of specified parameter in the
C-   current PXSCREEN array. Use PU_SET_SCREEN_PARAM to set the value of the 
C-   parameter. Use PU_GET_SCREEN_INDEX to return the index IDX 
C-   corresponding to the screen.
C-   
C-   Make sure the correct RCP bank has been selected with 
C-   EZPICK before calling these routines.
C-
C-   Inputs  : IDX      [I]     Index of screen (command)
C-             PARNAM   [C*]    Parameter name
C-            
C-   Outputs : IVAL     [I,L,R] Value
C-             IER      [I]     0 - OK
C-   
C-   Controls: none
C-
C-   Created   5-DEC-1990   Harrison B. Prosper   
C-   Updated  12-DEC-1990   Harrison B. Prosper   
C-   Updated  29-JAN-1991   Lupe Howell   
C-   Updated   6-MAY-1991   Harrison B. Prosper  
C-      Tidy up a little 
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX
      CHARACTER*(*) PARNAM
      INTEGER IVAL
      INTEGER IER
C
      CHARACTER*32 BANK_NAME
      CHARACTER*80 ERRSTR1,ERRSTR2,ERRSTR3
      CHARACTER*1  ERRSTR4
      CHARACTER*(*) ARRAY_NAME
      PARAMETER( ARRAY_NAME = 'PXSCREEN' )
      INTEGER LENGTH,ITYPE
      LOGICAL GET, EZERROR
C----------------------------------------------------------------------
      GET   = .TRUE.
      GOTO 1
C
C ****  ENTRY point to GET parameter
C
      ENTRY PU_SET_SCREEN_PARAM ( IDX, PARNAM, IVAL , IER )
      GET   = .FALSE.
      GOTO 1
C
    1 CONTINUE
C
      IER = 0
      CALL EZTELL(BANK_NAME,LENGTH)     ! For debugging purposes
C
C ****  Get/Set value
C
      IF ( GET ) THEN
        CALL EZ_GET_ELEMENT(ARRAY_NAME,PARNAM,IDX,1,IVAL,ITYPE,IER)
      ELSE
        CALL EZ_SET_ELEMENT(ARRAY_NAME,PARNAM,IDX,1,IVAL,IER)
      ENDIF
C
C ****  Check error code
C
      IF ( IER .NE. 0 ) THEN
        ERRSTR1 = 'NOTFOUND'
        ERRSTR2 = 'PU_GET_SCREEN_PARAM'
        ERRSTR4 = 'W'
        IF ( GET ) THEN
          ERRSTR3 = ' Error getting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG(ERRSTR1,ERRSTR2,ERRSTR3,ERRSTR4)
        ELSE
          ERRSTR3 = ' Error setting parameter '//PARNAM(1:LEN(PARNAM))
          CALL ERRMSG(ERRSTR1,ERRSTR2,ERRSTR3,ERRSTR4)
        ENDIF
      ENDIF
  999 RETURN
      END
