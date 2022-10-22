      SUBROUTINE PXMODIFY_3D(RCPFILE,SCREEN,NPORT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Updates 3-D parameters for the viewport VIEWPORT
C-   of the screen SCREEN
C-
C-   Inputs  : RCPFILE [C*]: RCP file name
C-             SCREEN  [C*]: Screen name
C-             NPORT    [I]: Number of ports in current screen
C-
C-   Outputs : None
C-
C-   Created  20-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-   Updated  26-JUL-1991   Lupe Howell  Fix to handle the combined view
C-                          with a single viewport
C-   Updated  20-DEC-1991   Lupe Howell  Modify PX_*_RCP ONLY 
c-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCPFILE
      CHARACTER*(*) SCREEN
      INTEGER NPORT
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER OCURR,WTYPE,IER,I,J,K,L,LEN,CONTROL,ISCREEN,NSCREEN
      INTEGER VIEWPORT,UNIQUEPORT,TOTAL_PORT,MODIFY_VIEWS,IDX(MAXCVIEW)
      INTEGER II,JJ,KK,LL
      REAL    CAMX,CAMY,CAMZ,REFX,REFY,REFZ,UPX,UPY,UPZ
      REAL    VCAMER(3),VECTOR(3),DISCAM,VMOD,XTEMP,YTEMP,ZTEMP
      REAL    XMIN(MAXCVIEW),XMAX(MAXCVIEW),YMIN(MAXCVIEW)
      REAL    YMAX(MAXCVIEW)
      CHARACTER*1 ANSW
      CHARACTER*40 ARRAY
      CHARACTER*24 DEFAULT
      CHARACTER*80 STRING
      CHARACTER*32 ACTION(MAXCVIEW),PACKAGE(MAXCVIEW),CURRENT_PACKAGE
      LOGICAL VAL3D,LAST,ALL_VIEWS
C---------------------------------------------------------------------
C
C ****  Select port that is going to be modify if the
C ****  number of viewports is greather than 1
C
      IF ( NPORT .GT. 1 ) THEN
        CALL PU_SELECT_PORT(NPORT,VIEWPORT)
      ELSE
        VIEWPORT = NPORT
      ENDIF
C
C ****  Setting up array names and index
C
      OCURR = VIEWPORT
      MODIFY_VIEWS = 1
      ALL_VIEWS = .FALSE.
      LL = INDEX(RCPFILE,'PX_')
      KK = INDEX(RCPFILE,'_RCP')
      CURRENT_PACKAGE = RCPFILE(LL+3:KK-1)
      CALL SWORDS(SCREEN,I,J,LEN)
      IF ( SCREEN(LEN:LEN) .EQ. '%' ) THEN
        CALL PU_GET_UNIQUE_PORTS(CURRENT_PACKAGE,SCREEN,
     &  XMIN,XMAX,YMIN,YMAX,PACKAGE,ACTION,IDX,UNIQUEPORT,TOTAL_PORT)
        ARRAY = SCREEN(1:LEN)
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
C ****  Getting VIEW3D value
C
      CALL GETPAR(1,' Do you want 3-D parameters (Y/N)?','U',ANSW)
      CALL WORD(ANSW,I,J,L)
      IF ( L .EQ. 0 ) THEN
        VAL3D = .FALSE.
      ELSEIF ( ANSW .EQ. 'y' .OR. ANSW .EQ. 'Y' ) THEN
        VAL3D = .TRUE.
      ELSE
        VAL3D = .FALSE.
      ENDIF
C
C ****  If 3-D get the new 3-D values
C
      IF (  VAL3D ) THEN
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VIEWREFX',IDX(1),OCURR,XTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VIEWREFY',IDX(1),OCURR,YTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT
     &    (ARRAY,'VIEWREFX',IDX(1),OCURR,ZTEMP,WTYPE,IER)
C
C ****  Get View refernces
C
   30   WRITE(DEFAULT,100)XTEMP,YTEMP,ZTEMP
        CALL GETPAR(1,'Enter the view ref. point ['//DEFAULT//']:',
     &  'U',STRING)
        CALL SWORDS(STRING,I,J,K)
        IF ( K .NE. 0 ) THEN
          READ(STRING(1:K),*,ERR=30)REFX,REFY,REFZ
        ENDIF
C
C ****  Get Up vector values
C
   40   CALL EZ_GET_ELEMENT(ARRAY,'UPVECX',IDX(1),OCURR,XTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT(ARRAY,'UPVECY',IDX(1),OCURR,YTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT(ARRAY,'UPVECZ',IDX(1),OCURR,ZTEMP,WTYPE,IER)
        WRITE(DEFAULT,100)XTEMP,YTEMP,ZTEMP
        CALL GETPAR(1,'Enter the up vector values ['//DEFAULT//']:',
     &  'U',STRING)
        CALL SWORDS(STRING,I,J,K)
        IF ( K .NE. 0 ) THEN
          READ(STRING(1:K),*,ERR=40)UPX,UPY,UPZ
        ENDIF
C
C ****  Get and set camera point
C
   50   CALL EZ_GET_ELEMENT(ARRAY,'CAMX',IDX(1),OCURR,XTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT(ARRAY,'CAMY',IDX(1),OCURR,YTEMP,WTYPE,IER)
        CALL EZ_GET_ELEMENT(ARRAY,'CAMZ',IDX(1),OCURR,ZTEMP,WTYPE,IER)
        WRITE(DEFAULT,100)XTEMP,YTEMP,ZTEMP
        CALL GETPAR(1,'Enter the camera point ['//DEFAULT//']:',
     &  'U',STRING)
        CALL SWORDS(STRING,I,J,K)
        IF ( K .NE. 0 ) THEN
          READ(STRING(1:K),*,ERR=50)CAMX,CAMY,CAMZ
        ENDIF
C
C ****  Check the distance Camera/View point
C
        VCAMER(1) = CAMX - REFX
        VCAMER(2) = CAMY - REFY
        VCAMER(3) = CAMZ - REFZ
        DISCAM = VMOD( VCAMER, 3 )
        IF( DISCAM .EQ. 0. ) THEN
          CALL OUTMSG('**** The Camera can not be at the View '//
     &                'Reference Point. ****')
          STRING = 'Y'
          CALL GETPAR(1,
     &      'Do you want to continue entering values[Y]? N-for exit',
     &      'U',STRING)
          IF ( STRING(1:1) .EQ. 'Y' ) THEN
            GOTO 50
          ELSE
            GOTO 999
          ENDIF
        ENDIF
C
C ****  Check the up vector
C
        CALL CROSS( VCAMER, UPX, VECTOR )
        IF( VMOD(VECTOR,3) .EQ. 0. ) THEN
          CALL OUTMSG('**** The Up vector can not be on the Camera '//
     &                'View Ref. Point... ***')
          STRING = 'Y'
          CALL GETPAR(1,
     &      'Do you want to continue entering values[Y]? N-for exit',
     &      'U',STRING)
          IF ( STRING(1:1) .EQ. 'Y' ) THEN
            GOTO 40
          ELSE
            GOTO 999
          ENDIF
        ENDIF
      ELSE
C
C ****  If NOT 3-D, restore default values
C
        REFX = 0.0
        REFY = 0.0
        REFZ = 0.0
        UPX  = 0.0
        UPY  = 1.0
        UPZ  = 0.0
        CAMX = 0.0
        CAMY = 0.0
        CAMZ = 1.0
      ENDIF
C
C ****  Setting the new 3D values in the RCP banks
C
      JJ = OCURR
      DO II = 1, MODIFY_VIEWS
        IF ( ALL_VIEWS ) JJ = II
C
C ****  Setting VIEW3D new value
C
        CALL EZ_SET_ELEMENT_l(ARRAY,'VIEW3D',IDX(II),OCURR,VAL3D,IER)
        CALL PXMODIFY_ACTIONS(RCPFILE,SCREEN,'VIEW3D',OCURR,VAL3D,IER)
C
C ****  Setting new 3-D values
C
        CALL EZ_SET_ELEMENT(ARRAY,'VIEWREFX',IDX(II),OCURR,REFX,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'VIEWREFY',IDX(II),OCURR,REFY,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'VIEWREFZ',IDX(II),OCURR,REFZ,IER)
C
        CALL EZ_SET_ELEMENT(ARRAY,'UPVECX',IDX(II),OCURR,UPX,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'UPVECY',IDX(II),OCURR,UPY,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'UPVECZ',IDX(II),OCURR,UPZ,IER)
C
        CALL EZ_SET_ELEMENT(ARRAY,'CAMX',IDX(II),OCURR,CAMX,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'CAMY',IDX(II),OCURR,CAMY,IER)
        CALL EZ_SET_ELEMENT(ARRAY,'CAMZ',IDX(II),OCURR,CAMZ,IER)
      ENDDO
  100 FORMAT(3F8.2)
  999 RETURN
      END
