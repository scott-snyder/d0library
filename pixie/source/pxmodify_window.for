      SUBROUTINE PXMODIFY_WINDOW(RCPFILE,SCREEN,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifies the window parameters of the given
C-   SCREEN in the given RCPFILE on the viewport specified
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Screen name
C-             NPORT   [I]: total number of ports in current screen
C-
C-   Outputs : None
C-
C-   Created  17-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-   Updated  26-JUL-1991   Lupe Howell  Fix to handle the combined view with 
C-                          a single viewport 
C-   Updated  20-DEC-1991   Lupe Howell  Modify PX_*_RCP ONLY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER OCURR,IER,WTYPE,ISCREEN,NSCREEN,I,J,L,ALEN,VIEWPORT
      INTEGER IDX(MAXCVIEW),UNIQUEPORT,TOTAL_PORT
      INTEGER II,JJ,KK,LL,MODIFY_VIEWS
      REAL    XMINPRT(MAXCVIEW),XMAXPRT(MAXCVIEW),YMINPRT(MAXCVIEW)
      REAL    YMAXPRT(MAXCVIEW)
      REAL    XMIN,XMAX,YMIN,YMAX,PXMIN,PXMAX,PYMIN,PYMAX

      LOGICAL CENTER,ALL_VIEWS
      CHARACTER*40 ARRAY
      CHARACTER*10 CVAL,TEMP
      CHARACTER*32 ACTION(MAXCVIEW),PACKAGE(MAXCVIEW),CURRENT_PACKAGE
C
      REAL    XSIZE,YSIZE,WSIZE,XWINCEN,YWINCEN,VXMIN,VXMAX,VYMIN,VYMAX
      INTEGER LON,K
      CHARACTER*10 DEFSIZE
      CHARACTER*80 STRING
C----------------------------------------------------------------------
      CENTER = .FALSE.
      GOTO 10
      ENTRY PXMODIFY_CENTER(RCPFILE,SCREEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Changes the WINDOW parameters by giving the
C-   center and sizeof the window
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Screen Name
C-             VIEWPORT [I]: Viewport number
C-
C-   Outputs : None
C-
C-   Created  20-MAY-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      CENTER = .TRUE.
C
C ****  Select port that is going to be modify if the
C ****  number of viewports is greather than 1
C
   10 IF ( NPORT .GT. 1 ) THEN
        CALL PU_SELECT_PORT(NPORT,VIEWPORT)
      ELSE
        VIEWPORT = NPORT
      ENDIF

      OCURR = VIEWPORT
C
C ****  Determine if combined or single view
C
      CALL SWORDS(SCREEN,I,J,L)
      MODIFY_VIEWS = 1
      ALL_VIEWS = .FALSE.
      LL = INDEX(RCPFILE,'PX_')
      KK = INDEX(RCPFILE,'_RCP')
      CURRENT_PACKAGE = RCPFILE(LL+3:KK-1)
      IF ( SCREEN(J:J) .EQ. '%' ) THEN
        CALL PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN,
     &  XMINPRT,XMAXPRT,YMINPRT,YMAXPRT,PACKAGE,ACTION,
     &  IDX,UNIQUEPORT,TOTAL_PORT)
        ARRAY = SCREEN(I:J)
        IF( (NPORT .EQ. 1) .AND. (TOTAL_PORT .GT. 1) ) THEN
          MODIFY_VIEWS = TOTAL_PORT
          ALL_VIEWS = .TRUE.
        ELSE
          IDX(1) = 1
        ENDIF
      ELSE
        ARRAY = 'PXSCREEN'
        CALL PU_GET_SCREEN_NUMBER(SCREEN,ISCREEN,NSCREEN,
     &          IER)
        CALL PU_GOTO_SCREEN(ISCREEN,IDX(1))
      ENDIF
      CALL OUTMSG('1')
C
C ****  Getting the current window values
C
      CALL EZ_GET_ELEMENT
     &  (ARRAY,'WINDOWXMIN',IDX(1),OCURR,XMIN,WTYPE,IER)
      CALL EZ_GET_ELEMENT
     &  (ARRAY,'WINDOWXMAX',IDX(1),OCURR,XMAX,WTYPE,IER)
      CALL EZ_GET_ELEMENT
     &  (ARRAY,'WINDOWYMIN',IDX(1),OCURR,YMIN,WTYPE,IER)
      CALL EZ_GET_ELEMENT
     &  (ARRAY,'WINDOWYMAX',IDX(1),OCURR,YMAX,WTYPE,IER)
      IF ( CENTER ) GOTO 40
C
C ****  Input X window values
C
      WRITE(TEMP,'(F10.2)')XMIN
   20 CALL GETPAR(1,' Enter Window lower X value ['//TEMP//']:',
     &     'U',CVAL)
      CALL SWORDS(CVAL,I,J,ALEN)
      IF ( ALEN .NE. 0 ) THEN
        READ(CVAL(1:ALEN),*) XMIN
      ENDIF
      WRITE(TEMP,'(F10.2)')XMAX
      CALL GETPAR(1,' Enter Window upper X value ['//TEMP//']:',
     &     'U',CVAL)
      CALL SWORDS(CVAL,I,J,ALEN)
      IF ( ALEN .NE. 0 ) THEN
        READ(CVAL(1:ALEN),*) XMAX
      ENDIF
C
C ****  Checking the X window values are valid
C
      IF( XMAX .LE. XMIN ) THEN
        CALL OUTMSG(' ***** X upper must be greater that X lower ****')
        GOTO 20
      ENDIF
C
C ****  Check if the Y coordenates were a conform viewport
C
      IF ( YMAX .LT. YMIN ) THEN
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTYMIN',IDX(1),OCURR,PYMIN,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTYMAX',IDX(1),OCURR,PYMAX,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTXMIN',IDX(1),OCURR,PXMIN,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTXMAX',IDX(1),OCURR,PXMAX,WTYPE,IER)
        YMAX = (PYMAX-PYMIN) * (XMAX-XMIN)/(PXMAX-PXMIN)
        YMIN = YMIN - 0.5 * YMAX
        YMAX = YMIN + 0.5 * YMAX
      ENDIF
C
C ****  Input Y window values
C
      WRITE(TEMP,'(F10.2)')YMIN
   30 CALL GETPAR(1,' Enter Window lower Y value ['//TEMP//']:',
     &     'U',CVAL)
      CALL SWORDS(CVAL,I,J,ALEN)
      IF ( ALEN .NE. 0 ) THEN
        READ(CVAL(1:ALEN),*) YMIN
      ENDIF
      WRITE(TEMP,'(F10.2)')YMAX
      CALL GETPAR(1,' Enter Window upper Y value ['//TEMP//']:',
     &     'U',CVAL)
      CALL SWORDS(CVAL,I,J,ALEN)
      IF ( ALEN .NE. 0 ) THEN
        READ(CVAL(1:ALEN),*) YMAX
      ENDIF
      IF( YMAX .LE. YMIN ) THEN
        CALL OUTMSG(' ***** Y upper must be greater that Y lower ****')
        GOTO 30
      ENDIF
   40 IF (  CENTER ) THEN
C
C ****  Get the viewport values to calculate the window center
C
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTXMIN',IDX(1),OCURR,VXMIN,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTXMAX',IDX(1),OCURR,VXMAX,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTYMIN',IDX(1),OCURR,VYMIN,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VPORTYMAX',IDX(1),OCURR,VYMAX,WTYPE,IER)
        XSIZE = VXMAX - VXMIN
        YSIZE = VYMAX - VYMIN
        XWINCEN = 0.5 * ( XMAX + XMIN )
        IF ( YMAX .GT. YMIN ) THEN
          YWINCEN = 0.5 * ( YMIN + YMAX )
        ELSE
          YWINCEN = YMIN
        ENDIF
C
C ****  The size is the dimention of the X side
C
        WSIZE = XMAX - XMIN
        WRITE(DEFSIZE,'(F10.2)') XWINCEN
        CALL GETPAR(1,'Enter the window X center ['//DEFSIZE//'] : ',
     &             'U',STRING)
        CALL WORD(STRING,I,J,LON)
        IF( LON .NE. 0 ) READ(STRING(1:LON),*) XWINCEN
        WRITE(DEFSIZE,'(F10.2)') YWINCEN
        CALL GETPAR(1,'Enter the window Y center ['//DEFSIZE//'] : ',
     &             'U',STRING)
        CALL WORD(STRING,I,J,LON)
        IF( LON .NE. 0 ) READ(STRING(1:LON),*) YWINCEN
        WRITE(DEFSIZE,'(F10.2)') WSIZE

   50   CALL GETPAR(1,'Enter the window X size   ['//DEFSIZE//'] : ',
     &             'U',STRING)
        CALL WORD(STRING,I,J,LON)
        IF( LON .NE. 0 ) READ(STRING(1:LON),*) WSIZE
        IF( WSIZE .LE.  0. ) GOTO 50
        CALL OUTMSG(' This window will always have the good aspect ')
        CALL OUTMSG(' ratio, i.e. a circle will be a circle')
        XMIN = XWINCEN - .5 * WSIZE
        XMAX = XWINCEN + .5 * WSIZE
C
C ****  parameters are now center and smaller number, to ask for good aspect
C ****  ratio when drawing this viewport
C
        YMIN = YWINCEN
        YMAX = YWINCEN - 1.
      ENDIF
C
C ****  Setting the new window values in the RCP bank
C
      JJ = OCURR
      DO II = 1, MODIFY_VIEWS
  500   CALL EZ_SET_ELEMENT(ARRAY,'WINDOWXMIN',IDX(II),OCURR,XMIN,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'WINDOWXMAX',IDX(II),OCURR,XMAX,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'WINDOWYMIN',IDX(II),OCURR,YMIN,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'WINDOWYMAX',IDX(II),OCURR,YMAX,IER)
      ENDDO
  999 RETURN
      END
