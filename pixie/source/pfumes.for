      SUBROUTINE PFUMES( TEXTE )
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
C-   Updated   6-FEB-1990   Jeffrey Bantly  altered for improved use 
C-   Updated  30-APR-1991   Jeffrey Bantly  cleanup and add checks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER CURLIN, LON, NCH
      INTEGER SEGNUM
C
      REAL    XV1, XV2, YV1, YV2, XVP, YVP
      REAL    XW1, XW2, YW1, YW2, XP1,YP1,ZP1
      REAL    SIZ, XSIZ, YSIZ, YXRAT, LINDEP
C
      LOGICAL READFL
C
      CHARACTER*(*) TEXTE, STRING
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
C ****  ENTRY PFRSTR( texte, string, lon )
C
      ENTRY PFRSTR( TEXTE, STRING, LON )
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
C      IF ( .NOT. DI3RDY ) THEN
C        CALL OUTMSG( ' '//TEXTE )
C        RETURN
C      ENDIF
      CALL J4RGET( 1, XW1, XW2, YW1, YW2 )
      CALL J4RGET( 2, XV1, XV2, YV1, YV2 )

      SIZ = LEN( TEXTE )
      SIZ = SIZ * (XW2-XW1) / 100.
C
C ****  Use retained segment to allow users hardcopy...
C
      CALL J1IGET(1,SEGNUM)
C If no segment is open, then open one
      IF(SEGNUM.LT.0)THEN
        CALL PUOPEN
      ENDIF
      CALL JJUST( 1, 2 )
      CURLIN = CURLIN + 1
      XSIZ = 1.*(2./(xv2-xv1))
      YXRAT = 1.5*(2./(yv2-yv1))
      IF(XSIZ.GT. 1.2) THEN
        XSIZ=1.5
        YXRAT=1.5
      ENDIF
      LINDEP = (YW2-YW1)*(YXRAT+1.)*XSIZ/100.
      IF( (YW2-(CURLIN*LINDEP)) .GT. YW1 ) THEN
        CALL PUVSTR( XW1, (YW2-(CURLIN*LINDEP)), XSIZ, YXRAT, TEXTE )
        CALL JCP(XP1,YP1,ZP1)
        CALL JJUST(2,2)
        CALL JRMARK(0.,0.)
      ENDIF
C close segment if it was not open when entering this subroutine
      IF(SEGNUM.LT.0)THEN
         CALL JRCLOS
      ENDIF
      IF ( READFL ) THEN
C
C ****  Echo is not centered on the character cell. The following value is
C ****  experimental result on HDS terminal, and should works elsewhere
C
        CALL JCONWV( (XW1+SIZ+1.),(YW2-(CURLIN*LINDEP)), 0., XVP, YVP )
        IF( XVP .GT. XV2 ) XVP = XV2*0.98            ! protect 
        IF( XVP .LT. XV1 ) XVP=XV1*1.02              ! protect
        CALL JPECHO( 1, 4, 1, XVP, YVP )
        CALL JIENAB( 1, 4, 1 )
        NCH = LEN( STRING )
        CALL JKEYBS( 1, 1, 1, NCH, STRING, LON )
        CALL JIDISA( 1, 4, 1 )
        IF( LON .LE. 0 ) LON = 0
      ENDIF
C--------------------------------------------------------------------------
  999 RETURN
      END
