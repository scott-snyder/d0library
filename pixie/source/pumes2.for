      SUBROUTINE PUMES2( TEXTE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output a message on a new line on the screen
C-
C-   Inputs  : TEXTE [C*] : Message to be displayed ( space only means just
C-                              reset the line counter )
C-   Outputs :
C-   Controls:
C-
C-   Created   8-JUL-1988   Olivier Callot
C-   Updated  16-NOV-1988   Olivier Callot  Use retained segment
C-   Updated   1-NOV-1989   Lupe Rosas  xsize and ysize calculated without 
C-                          PUCSIZ
C-   Borrowed 13-JUN-1990   C.E.Cretsinger--nonretained segment
C-   Modified 20-MAR-1991   Nobu. (Remove 'DI3RDY' check)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXTE, STRING
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER CURLIN, LON, NCH
      INTEGER SEGNUM
      REAL    XV1, XV2, YV1, YV2, XVP, YVP
      REAL    XW1, XW2, YW1, YW2
      REAL    SIZ, XSIZ, YSIZ
      LOGICAL READFL
C----------------------------------------------------------------------
C
C ****  Initialisation only: clear the line counter
C
      IF ( TEXTE .EQ. ' ' ) THEN
        CURLIN = 0
        RETURN
      ENDIF
C
      READFL = .FALSE.
      GOTO 10
C
C ****  ENTRY PURST2( texte, string, lon )
C
      ENTRY PURST2( TEXTE, STRING, LON )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a string, with a prompt at the actual message
C-              position
C-
C-   Inputs  : TEXTE  [C*] : prompt string
C-   Outputs : STRING [C*] : string read
C-             LON    [I]  : length of string read, 0 if none
C-
C----------------------------------------------------------------------
      READFL = .TRUE.
   10 CONTINUE
      CALL J4RGET( 1, XW1, XW2, YW1, YW2 )
      CALL J4RGET( 2, XV1, XV2, YV1, YV2 )

      SIZ = LEN( TEXTE )
      XSIZ = ((XV2-XV1)/3.)/30.
      YSIZ = (YV2-YV1)/26.
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &             YCVPRT+YMVPRT- (CURLIN+1)*YSIZ,
     &             YCVPRT+YMVPRT- (CURLIN  )*YSIZ)
      CALL JWINDO( 0., (2*XMVPRT)/XSIZ , -.5, .5 )
C
C
      CALL J1IGET(1,SEGNUM)
C If no segment is open, then open one
      IF(SEGNUM.LT.0)THEN
        CALL JOPEN
      ENDIF
      CALL JJUST( 1, 2 )
      CALL JSIZE( 1., .8 )
      CALL PUSTRG( 0., 0., TEXTE )
      CURLIN = CURLIN + 1
C close segment if it was not open when entering this subroutine
      IF(SEGNUM.LT.0)THEN
         CALL JCLOSE
      ENDIF
      IF ( READFL ) THEN
C
C ****  Echo is not centered on the character cell. The following value is
C ****  experimental result on HDS terminal, and should works elsewhere
C
        CALL JCONWV( SIZ+1., -.3 , 0., XVP, YVP )
        IF( XVP .GT. XCVPRT+XMVPRT ) XVP = XCVPRT       ! protect 
        CALL JPECHO( 1, 4, 1, XVP, YVP )
        CALL JIENAB( 1, 4, 1 )
        NCH = LEN( STRING )
        CALL JKEYBS( 1, 1, 1, NCH, STRING, LON )
        CALL JIDISA( 1, 4, 1 )
        IF( LON .LE. 0 ) LON = 0
      ENDIF
      CALL JWINDO( XW1, XW2, YW1, YW2 )
      CALL JVPORT( XV1, XV2, YV1, YV2 )
  999 RETURN
      END
