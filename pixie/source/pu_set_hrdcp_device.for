      SUBROUTINE PU_SET_HRDCP_DEVICE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes DI3000, assuming the active device,
C-   IDEV, is activated.  This routine is used when PIXIE is run in 
C-   BATCH mode.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  16-FEB-1993   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER DEVCOL
      CHARACTER*3 DRIVER
      REAL    RATIO,FRSCRE
      LOGICAL FIRST
C
      INTEGER FOUR_BIT_COLOR
      PARAMETER( FOUR_BIT_COLOR = 11 )
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Setup the DI3000 configuration, the segment handling, and
C ****  write the header
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL FLGSET('DI3_INI',.TRUE.)
        CALL DI3_START(IDEV)
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
        MAXSEG = 0  ! for E&S (Nobu. 10-DEC-1990)
C
        CALL PX_CLOSE_OPEN_SEGMENT
      ENDIF
  999 RETURN
      END
