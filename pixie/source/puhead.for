      SUBROUTINE PUHEAD( TITLE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates the main frame, i.e. the title line with
C-                      comments, date, run, ... infos, and sets the default
C-                      viewport and 3-D parameters
C-
C-   Inputs  : TITLE [C*] : Title of the display, 16 characters are printed on
C-                          the upper left corner
C-
C-   Outputs : on the screen, and as viewport parameters
C-   Controls:
C-
C-   Created   7-JUL-1988   Olivier Callot
C-   Modified  4-JUN-1990   Nobu Oshima( Change the logic for MAXSEG = 0 )
C-   Updated  29-OCT-1990   Susan K. Blessing  Put date event was taken
C-                                             in and adjust spacing.
C-   Modified 18-DEC-1990   Nobu Oshima( Retained to Temporary segment )
C-   Modified 20-APR-1991   Nobu Oshima( Add a beam X-ing Number option )
C-   Modified 16-MAY-1991   Nobu Oshima( Add 'CALL PUPHI_RESET' )
C-   Updated  18-JAN-1992   Nobu Oshima( Activate a beam X-ing Number )
C-   Updated  17-AUG-1992   Lupe Howell  Entry RETAIN_PUHEAD created for 
C-                hardcopy of a superimposed view 
C-   Updated  20-JUL-1993   Lupe Howell  Added GET_TITLE Entry 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      CHARACTER*(*) VIEW_TITLE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:HDRINX.INC'

C&IF VAXVMS
      INTEGER SYS$GETTIM, SYS$ASCTIM, STATUS, ITODAY(2), LON
C&ENDIF
      INTEGER RUNNO,EVONUM
      INTEGER IEVN1,IEVN2
      INTEGER IER
      REAL    POSTIT,CENTIT,CDEL
      CHARACTER*38 BEAMXN
      CHARACTER*30 TIMEIN
      CHARACTER*24 IBLANK
      CHARACTER*16 LOCTIT
      CHARACTER*80 REAL_TITLE
      LOGICAL FIRST,EZERROR,RETAIN
      LOGICAL ELABEL
      DATA      POSTIT / .92 /
      DATA CDEL /.55/
      DATA IBLANK/'                        '/
      DATA FIRST /.TRUE./
      SAVE REAL_TITLE
C----------------------------------------------------------------------
      RETAIN = .FALSE.
      CALL PURSET
C
C ****  Clear the screen and the retained segments, clear the segment number.
C
      CALL JCLEAR
      CALL JFRAME
C
C.N.O.- for EAS.
      IF (FIRST) THEN
        FIRST  = .FALSE.
      ELSE
        MAXSEG = 0
      ENDIF
      GOTO 10
C
      ENTRY RETAIN_PUHEAD
C----------------------------------------------------------------------
C-   ENTRY RETAIN_PUHEAD
C----------------------------------------------------------------------
      RETAIN = .TRUE.
      CALL JVPORT( -XDVMAG,XDVMAG,-YDVMAG,YDVMAG)
   10 CALL VZERO(IRTSEG,10)
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
C-
C
C ****  In the full viewport, draw the frame, with upper line for text
C
C.N.O.- for EAS.
      CALL JWINDO (-1.0001, 1.0001,  -1.0001,  1.0001)
      IF ( RETAIN ) THEN
        CALL PUOPEN
      ELSE
        CALL JOPEN
      ENDIF
      CALL JRECT( -1., -1.,  1.,  1. )
      CALL JMOVE( -1.,  POSTIT )
      CALL JDRAW(  1.,  POSTIT )
C&IF VAXVMS
      STATUS = SYS$GETTIM( ITODAY )
      STATUS = SYS$ASCTIM( LON, TIMEIN, ITODAY, 0 )
C&ENDIF
C
C ****  Character size
C
      CALL JSIZE(  1./40. , .7*(1.-POSTIT) )
      CALL JJUST( 1, 2 )
      CENTIT = .5 * ( 1. + POSTIT )
      LOCTIT = TITLE
      REAL_TITLE = TITLE
      CALL PUSTRG( -.97, CENTIT, LOCTIT//' '//TIMEIN(1:17) )
C
C ****  A vertical bar to separate the event information
C
      CALL JMOVE( -.10 , POSTIT )
      CALL JDRAW( -.10 , 1. )
      WRITE( TIMEIN, 1000 ) RUNNO(), EVONUM()
 1000 FORMAT('Run',I8,' Event',I8)
      CALL PUSTRG( -.08, CENTIT, TIMEIN(1:30) )
C
C ****  A vertical bar to separate PROD. DATAE/TIME
C
      CALL JMOVE( CDEL , POSTIT )
      CALL JDRAW( CDEL , 1. )
C
C ****  Now, the production date and time of this event
C
      CALL JJUST(3,2)
      TIMEIN=IBLANK
C&IF VAXVMS
      IF(IQ(LHEAD+DATMX).NE.0)THEN
        STATUS = SYS$ASCTIM( LON, TIMEIN, IQ(LHEAD+DATMX),0)
        CALL PUSTRG( .98, CENTIT, TIMEIN(1:17) )
      ENDIF
C&ENDIF
C--
C--- Draw Beam X-ing Number
C-
      IF(ELABEL) THEN
        CALL EVXNUMS(IEVN1,IEVN2)
C--- There are no guaranteed positive by Level 1 now !!!(Nobu. 09-APR-92)
        IEVN1 = ABS(IEVN1)
        IEVN2 = ABS(IEVN2)
        WRITE( BEAMXN, 1010 ) IEVN2,IEVN1
 1010   FORMAT('[BEAM X-ING No.:',2I10,']')
        CALL PUSTRG( .98, .89, BEAMXN(1:37) )
      ENDIF
  900 IF ( RETAIN ) THEN
        CALL PUCLOSE
      ELSE
        CALL JCLOSE
      ENDIF
C
C ****  Sets the useful viewport limits
C
      XCVPRT = 0.
      XMVPRT = XDVMAG
      YCVPRT = YDVMAG * ( POSTIT-1 )*.5
      YMVPRT = YDVMAG * ( POSTIT+1 )*.5
      CALL JVPORT( XCVPRT-XMVPRT, XCVPRT+XMVPRT,
     &             YCVPRT-YMVPRT, YCVPRT+YMVPRT )
      CALL PUMESS(' ')          ! to position the first message
C-
C--- Until trigger info. is available in Bank...
C-
      IF(ELABEL .AND. TITLE.NE.'D0 Event Display') THEN
        CALL PU_GET_TRIG_INFO
      ENDIF
C-
C--- CHECK RESET PHI OR NOT RESET, WHEN CHENGE EVENTS
C-
      CALL PUPHI_RESET
C-
      GOTO 999

      ENTRY GET_TITLE(VIEW_TITLE)
C----------------------------------------------------------------------
C-
C- Purpose: Returns the title of the last view
C- 
C- Inputs : None
C- Outputs: VIEW_TITLE[C*]: Returns the title of the current view
C- 
C- Created 20-JUL-1993 Lupe Howell
C- 
C----------------------------------------------------------------------
      VIEW_TITLE = REAL_TITLE
  999 RETURN
      END
