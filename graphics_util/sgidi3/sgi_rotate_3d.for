      SUBROUTINE SGI_ROTATE_3D(NSEG,LIST,COMMAND)
C----------------------------------------------------------------------
C-   Purpose and Methods : Initialize screen parameters to rotate
C-   the current view(s).  VERSION FOR DI3GL (SGI)
C-   Inputs  : NSEG             Number of segments
C-             LIST     [I*]    Rotatable segment flags (-1)
C-             COMMAND  [C*]    Screen Command
C
C---------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:HDRINX.INC'
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C---------------------------------------------------------------------
      INTEGER NSEG,LIST(*)
      CHARACTER*(*) COMMAND
      INTEGER COLORS(5)
      CHARACTER*18 STR1
      CHARACTER*15 LABELS(5)
      INTEGER RUNNO,EVONUM
      REAL LEFT,VWSAVE(85)
      CHARACTER*38 BEAMXN
      CHARACTER*30 TIMEIN
      CHARACTER*24 IBLANK
      CHARACTER*16 LOCTIT
      LOGICAL FIRST,EZERROR
      LOGICAL ELABEL
      DATA POSTIT / .92 /
      DATA IBLANK/'                        '/
      DATA FIRST /.TRUE./
C---------------------------------------------------------------------
C
      NONROT=1                    ! ROUTINE BELOW HAS BEEN PROVIDED
C
C  CALL THE FAST MANIPULATION MENU
      CALL J_MENU3D(NSEG,LIST)
C
      RETURN
C----------------------------------------------------------------------
      ENTRY NON_ROTATABLE_OBJECTS
C-
      CALL EZPICK('PX_SYSTEM_RCP')          ! Selecting SYSTEM bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PUHEAD','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')
        ELABEL=.FALSE.
      ELSE
        CALL PUGETV('BEAM X-ING NUM',ELABEL)
        CALL EZRSET
      ENDIF
C
C ****Do the legend.
      CALL PCELAB(COLORS,LABELS)
      CALL LEGEND3D(COLORS,LABELS,5)
C
C ****  In the full viewport, draw the frame, with upper line for text
      CALL JVSAVE(VWSAVE)
      CALL JASPEK(1,RR)
      CALL JVPORT(-1.,1.,-RR,RR)
      CALL JUPVEC(0.,1.,0.)
      CALL JNORML(0.,0.,-1.)
      CALL J4RGET(1,LEFT,RIGHT,BOTTOM,TOP)
      CALL JOPEN
      CALL PXCOLR('FOR')
      CALL JRECT(LEFT,BOTTOM,RIGHT,TOP)
      POSTI=TOP*POSTIT
      CALL JMOVE(LEFT,POSTI)
      CALL JDRAW(RIGHT,POSTI)
C
C ****  Character size
      DELX=RIGHT-LEFT
      CALL JSIZE(DELX/80. , .7*(TOP-POSTI) )
      CALL JJUST( 1, 2 )
      CENTIT = .5 * (TOP + POSTI )
      MM=LEN(COMMAND)
      IF(MM.GT.16) MM=16
      type *,' MM,COMMAND:',mm,command(1:mm)
c      LOCTIT(1:MM)=COMMAND(1:MM)
      CALL PUSTRG( -.97*LEFT, CENTIT, LOCTIT//' '//TIMEIN(1:17) )
C
C ****  A vertical bar to separate the event information
      CALL JMOVE( -.20*LEFT , POSTI )
      CALL JDRAW( -.20*LEFT , TOP )
      WRITE( TIMEIN, 1000 ) RUNNO(), EVONUM()
 1000 FORMAT('Run',I8,' Event',I8)
      CALL PUSTRG( -.08*LEFT, CENTIT, TIMEIN(1:30) )
C
C ****  Now, the production date and time of this event
      CALL JJUST(3,2)
      TIMEIN=IBLANK
C-
C--- Draw Beam X-ing Number
      IF(ELABEL) THEN
        CALL EVXNUMS(IEVN1,IEVN2)
        IEVN1 = ABS(IEVN1)
        IEVN2 = ABS(IEVN2)
        WRITE( BEAMXN,1010 ) IEVN2,IEVN1
 1010   FORMAT('[BEAM X-ING No.:',2I10,']')
        CALL PUSTRG( .98*RIGHT, .89*TOP, BEAMXN(1:37) )
      ENDIF
C
      CALL JCLOSE
      CALL JVLOAD(VWSAVE)
      END

