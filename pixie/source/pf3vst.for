      SUBROUTINE PF3VST( XPOS, YPOS, ZPOS, PRCENT, YXRATI, TEXT )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a string at the given position.
C-               The quality of the output depends on the parameter
C-               STRING QUALITY and is 1,2,3,4 for the JxSTRG DI3000 routine
C-               ( 4 for JHSTRG )
C-
C-   Inputs  : XPOS   [F] is the x position where to output the string
C-             YPOS   [F] is the y position where to output the string
C-             ZPOS   [F] is the z position where to output the string
C-             PRCENT [F] is the percentage of window size used for
C-                        character size, default=1.
C-             YXRATI [F] is the Y / X character size ratio, default=2.
C-             TEXT  [C*] is the text to be output
C-   Outputs :
C-
C-   Created  15-MAY-1989   Jeffrey Bantly
C-   Updated   7-AUG-1990   Jeffrey Bantly   use for 3-D
C-   Updated  16-MAY-1991   Susan K. Blessing  Fix EZRSET error. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    XPOS, YPOS, ZPOS, XSIZE, YSIZE, PRCENT, YXRATI
      REAL    XW1, XW2, YW1, YW2
      INTEGER NUSTR,OLD_NUSTR
      CHARACTER*(*) TEXT
      CHARACTER*4 CVAL, REM
      INTEGER TYP,IVAL,IER
      LOGICAL EZERROR
C----------------------------------------------------------------------
      CALL J4RGET(1,XW1,XW2,YW1,YW2)
      IF( PRCENT .LE. 0. .OR. PRCENT .GT. 100. ) PRCENT = 1.
      PRCENT = PRCENT / 100.
      IF( YXRATI .LE. 0. ) YXRATI = 2.
      XSIZE = ( XW2 - XW1 ) * PRCENT
      YSIZE = ( YW2 - YW1 ) * PRCENT * YXRATI
      CALL JSIZE( XSIZE, YSIZE )
      CALL J3MOVE( XPOS, YPOS, ZPOS )
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3VTS','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
      CALL EZ_GET_ARRAY('PXPARAMS','STRING QUALITY',1,NUSTR,CVAL,
     &       TYP,REM,IER)
      IF ( NUSTR .LE. 1 ) THEN
        CALL J1STRG( TEXT )
      ELSEIF ( NUSTR .EQ. 2 ) THEN
        CALL J2STRG( TEXT )
      ELSEIF ( NUSTR .EQ. 3 ) THEN
        CALL J3STRG( TEXT )
      ELSE
        CALL JHSTRG( TEXT )
      ENDIF
C----------------------------------------------------------------------
C
      CALL EZRSET
C
  999 RETURN
      END
