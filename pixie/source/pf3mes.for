      SUBROUTINE PF3MES( TEXTE )
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
C-   Updated   6-FEB-1990   Jeffrey Bantly  altered for 3-D improved use 
C-   Updated   5-NOV-1990   Jeffrey Bantly  continuing attempts for 3-D use 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TEXTE, STRING
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER CURLIN, LON, NCH
      INTEGER SEGNUM
      REAL    A,B,C,B1, XU, YU, ZU, MAG
      REAL    XV1, XV2, YV1, YV2, XVP, YVP, I
      REAL    XW1, XW2, YW1, YW2, XP1,YP1,ZP1
      REAL    SIZ, XSIZ, YSIZ, YXRAT, LINDEP
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
C ****  ENTRY PFRSTR( texte, string, lon )
C
      ENTRY PF3RST( TEXTE, STRING, LON )
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
C
C  set character text and positioning parameters
C
      CALL J3RGET(8,A,B,C)
      IF(A.EQ.0.0) A=1.0
      IF(C.EQ.0.0) C=1.0
      MAG = SQRT(A**2. + C**2.)
      IF(MAG.EQ. 0.0) MAG=1.0
      CALL JBASE(-C/MAG,0.,A/MAG)
      IF(B.EQ. 0.0) THEN
        B1=(A**2. + C**2.) / .001
      ELSE
        B1= - (A**2. + C**2.) / B
      ENDIF
      MAG = SQRT(A**2. + B1**2. + C**2.)
      IF(MAG.EQ. 0.0) MAG=1.0
      CALL JPLANE(A/MAG,B1/MAG,C/MAG)
C
      CALL J4RGET( 1, XW1, XW2, YW1, YW2 )
      CALL J4RGET( 2, XV1, XV2, YV1, YV2 )
c
      SIZ = LEN( TEXTE )
      SIZ = SIZ * (XW2-XW1) / 100.
C
C ****  Use retained segment to allow users hardcopy...
C
      CALL J1IGET(1,SEGNUM)
      IF(SEGNUM.LT.0)THEN       ! If no segment is open, then open one
        CALL PUOPEN
      ENDIF
      CALL JJUST( 1, 2 )
      CURLIN = CURLIN + 1
      XSIZ = 0.8*(2./(XV2-XV1))
      YXRAT = 1.3*(2./(YV2-YV1))
      LINDEP = (YW2-YW1)*(YXRAT+1.)*XSIZ/100.
      CALL J3RGET(7,XU,YU,ZU)
      CALL JCONVW(-.95,.70,XW1,YW2,ZU)
      IF(YW2.GT. 105.) YW2 = 105.
      CALL PF3VST( XW1, (YW2-(CURLIN*LINDEP)-0.20*(YW2-YW1)),
     &                            ZU, XSIZ, YXRAT, TEXTE )
      CALL JCP(XP1,YP1,ZP1)
      CALL JJUST(2,2)
      CALL J3MARK(0.,0.,0.)
      IF(SEGNUM.LT.0)THEN     ! Close segment if not open when entering subrtn
         CALL JRCLOS
      ENDIF
      IF ( READFL ) THEN
C
C ****  Echo is not centered on the character cell. The following value is
C ****  experimental result on HDS terminal, and should work elsewhere
C
        CALL JCONWV( (XW1+SIZ+1.),(YW2-(CURLIN*LINDEP)), 0., XVP, YVP )
        IF( XVP .GT. XV2 ) XVP = XV2*0.98            ! protect 
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
