      SUBROUTINE PUINIT(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the PIXIE environment.
C-
C-   Inputs  : None
C-   Outputs : OK       [L]     TRUE if initialization is sucessful
C-   Controls:
C-
C-   Created  26-APR-1990   Lupe Howell Based in original PUINIT and PXMAIN
C-                          by Olivier Callot
C-   Updated   7-SEP-1990   Harrison B. Prosper
C-      Tidied up a little (well a lot!)
C-   Updated  13-NOV-1990   Harrison B. Prosper
C-      Make calls to HIGZ routines IGINIT, IGSSE
C-   Updated   4-DEC-1990   Harrison B. Prosper
C-      reset superimpose flag
C-   Updated  14-JAN-1991   Harrison B. Prosper
C-      Add calls to PUOPEN_CHECK_VIEW3D
C-   Updated  20-FEB-1991   Harrison B. Prosper
C-      Use new routine DI3_START
C-   Updated   5-APR-1991   Harrison B. Prosper
C-      Add PX_CLOSE_OPEN_SEGMENT
C-   Updated  10-APR-1991   Lupe Howell   The color table setting taken out
C-   Updated  13-MAY-1991   Harrison B. Prosper
C-      Add call to read PX_SYSTEM.RCP file
C-   Updated  18-JUN-1991   Nobuaki Oshima, Harrison B. Prosper
C-      Add PICKING flag
C-   Updated  28-AUG-1992   Nobuaki Oshima
C-      Add HARDWARE_ROTATE flag
C-   Updated  25-SEP-1992   Nobuaki Oshima
C-      Add GOTO_EVENT flag
C-   Updated  14-DEC-1992   Nobuaki Oshima
C-      Add PX_WRITE_EVENT flag
C-   Updated  13-JAN-1993   Vipin Bhatnagar
C-      Add PX_WRITE_SCAN flag & ACTIVE_SCAN flag
C-   Updated  23-MAR-1993   Lupe Howell   Batch mode set
C-   Updated  11-OCT-1995   Nobuaki Oshima
C-      Add the device viewport definition in XWNDI3 mode
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      INTEGER CTRL_X
      PARAMETER( CTRL_X = 24 )
C
      REAL    RATIO, FRSCRE, RLEVEL,DX,DY
      INTEGER FOUR_BIT_COLOR,DEVCOL,STATUS,IER
      PARAMETER( FOUR_BIT_COLOR = 11 )
      CHARACTER*72 V,VPIXIE
      CHARACTER*4 CHOPTN(1)
      CHARACTER*3 DRIVER
      LOGICAL FLGVAL,EZERROR
C----------------------------------------------------------------------
      LOGICAL FIRST, ONCE,OKAY
      DATA    FIRST / .TRUE. /
      DATA    ONCE  / .FALSE./
      SAVE FIRST,OKAY
C----------------------------------------------------------------------
C
C ****  Disable checking of VIEW3D in PUOPEN
C
      CALL PUOPEN_CHECK_VIEW3D(.FALSE.)
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        OKAY  = .TRUE.
C
C ****  Book system flags
C
        CALL FLGBK('ACTIVE_SCAN',1)
        CALL FLGBK('AUTO_DISPLAY',1)
        CALL FLGBK('COMBINED_MODE',1)
        CALL FLGBK('EXIT_PIXIE',1)
        CALL FLGBK('GOTO_EVENT',1)
        CALL FLGBK('HARDCOPY',1)
        CALL FLGBK('HARDWARE_ROTATE',1)
        CALL FLGBK('NEXT_EVENT',1)
        CALL FLGBK('PICKING',1)
        CALL FLGBK('PX_WRITE_EVENT',1)
        CALL FLGBK('PX_WRITE_SCAN',1)
        CALL FLGBK('ROTATING',1)
        CALL FLGBK('SEQUENCE_MODE',1)
        CALL FLGBK('SUPERIMPOSE',1)
        CALL FLGBK('USE_COMMAND_FIFO',1)
        CALL FLGBK('USE_SEQUENC_FIFO',1)
        CALL FLGBK('ZOOMING',1)
        CALL FLGBK('BATCH',1)
        CALL FLGBK('DI3_INI',1)
C
C ****  Identify program version
C
        V = VPIXIE()
        CALL INTMSG(V)
        DI3RDY = .FALSE.
C
C ****  Test if there is a DI3000 driver defined for unit 1
C
        CALL D0HDRV( 1, DRIVER )
        IF( DRIVER .EQ. '   ' .OR. DRIVER .EQ. 'DRV' ) THEN
          CALL INTMSG(' DI3000 cannot work without a driver.')
          CALL INTMSG(' Use SETDRV before running this program.')
          OKAY = .FALSE.
          OK   = OKAY
          GOTO 999
        ENDIF
C
C ****  Check if the primary driver is a hardcopy device
C ****  If so turn BATCH flag on 
C
        IF(( DRIVER .EQ. 'PST' ) .OR. (DRIVER .EQ. 'LN3' ) )THEN
          IDEV =2
          CALL FLGSET('BATCH',.TRUE.)
          CALL FLGSET('DI3_INI',.FALSE.)
C
C ****  Setting the display device to the screen if
C ****  the primary driver is not a hardcopy one
C
        ELSE
          IDEV = 1
        ENDIF
C
C ****  Set up top level menu PIXIE
C
        CALL PU_SETUP_TOP_MENU(OKAY)
C
C ****  Now read in system rcp file
C
        CALL INRCP('PX_SYSTEM_RCP',STATUS)
        IF ( STATUS .EQ. 0 ) THEN
C          CALL EZ_SETUP_COMPACK('PX_SYSTEM_RCP',STATUS)
        ELSE
          CALL ERRMSG('NOTFOUND','PUINIT',
     &      'Cannot open file PX_SYSTEM_RCP','W')
        ENDIF
      ENDIF
      OK = OKAY
C
C ****  Initialize the PIXIE command fifo
C
      CALL PU_SETUP_COMMAND_FIFO
C
C ****  Check for NEXT event
C
      CALL FLGSET('EXIT_PIXIE',.FALSE.)
      CALL FLGSET('GOTO_EVENT',.FALSE.)
      IF ( FLGVAL('NEXT_EVENT') ) GOTO 999
C
C ****  Reset a few flags
C
      CALL FLGSET('SUPERIMPOSE',.FALSE.)
      CALL FLGSET('ZOOMING',.FALSE.)
      CALL FLGSET('ROTATING',.FALSE.)
      CALL FLGSET('HARDCOPY',.FALSE.)
      CALL FLGSET('PICK_MODE',.FALSE.)
C
C ****  Setup the DI3000 configuration, the segment handling, and
C ****  write the header ONLY if NOT in BATCH
C
      IF ( .NOT. FLGVAL('BATCH') ) THEN
        CALL JIQDIL(RLEVEL)
C-
C--- Do XWNDI3 mode only (RLEVEL=5)
C-
        IF (RLEVEL .EQ. 5.) THEN
          CALL EZPICK('PX_SYSTEM_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('PIXIE','PUINIT','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')            
          ELSE
            CALL PUGETV('XWINDOW',DX)
            CALL PUGETV('YWINDOW',DY)
            CALL JDEVVP(0,0.,DX,0.,DY)
            CALL EZRSET
          ENDIF          
        ENDIF
C-
        CALL FLGSET('DI3_INI',.TRUE.)
        CALL DI3_START(IDEV)
C
        CALL JSETER(7)
        CALL JASPEK(IDEV,RATIO)
C
C **** Setting colors for the color table
C
        CALL JIQDEV(IDEV, 1, DEVCOL)
        IF ( DEVCOL .GT. FOUR_BIT_COLOR ) THEN  ! Color or intensity devices
          CALL SETCOLTB(DRIVER,DEVCOL)          ! Setting color table
        ENDIF
C
C ****  Calculating the aspect ratio to map the device window
C
        FRSCRE = 1.
        XCVPRT = 0.
        YCVPRT = 0.
        XMVPRT = 1.
        YMVPRT = 1.
        XDVMAG = FRSCRE
        YDVMAG = FRSCRE
        IF ( RATIO .LT. 1. ) THEN
          YDVMAG = FRSCRE * RATIO
          YMVPRT = 1.     * RATIO
        ELSEIF ( RATIO .GT. 1. ) THEN
          XDVMAG = FRSCRE / RATIO
          XMVPRT = 1.     / RATIO
        ENDIF
C
C ****  Map the device window taking account of the aspect ratio
C
        CALL JDEVWN( IDEV, -XMVPRT, XMVPRT, -YMVPRT, YMVPRT)
        CALL JTTYPE( 2 )
        CALL JRIGHT(.TRUE.)
        CALL JWCLIP( .TRUE. )
C
C ****  Draw the header if not in the loop (i.e. if the menu may be changed )
C
        MAXSEG = 0  ! for E&S (Nobu. 10-DEC-1990)
C
        CALL PX_CLOSE_OPEN_SEGMENT
        CALL PUHEAD('D0 Event Display')
      ENDIF

C
C ****  Disable checking of VIEW3D in PUOPEN
C
  999 CONTINUE
      CALL PUOPEN_CHECK_VIEW3D(.TRUE.)
C
      RETURN
C----------------------------------------------------------------------
C
C ****  Entry for exiting PIXIE
C
      ENTRY PUEXIT
      CALL JCLEAR
C
C ****  This is a trick to switch HDS to alpha mode, with CTRL-X character
C
      WRITE( 6, 1000 ) CTRL_X
 1000 FORMAT(' ',A1 )
      RETURN
      END
