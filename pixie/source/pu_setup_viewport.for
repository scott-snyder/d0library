      SUBROUTINE PU_SETUP_VIEWPORT(IDX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define the viewport and its world coordinates
C-   for the screen specified by the index IDX. Return the viewport
C-   dimensions using entry point PU_GET_VIEWPORT_SIZE.
C-
C-   Inputs  : IDX      [I]     Pointer to screen in RCP-bank
C-   Outputs : IER      [I]     0 - ok
C-   Controls: None
C-
C-   Modified 19-JUN-1991   Nobuaki Oshima
C-                      Doesn't draw RECT when rotating/picking mode.
C-   Created  18-MAY-1991   Lupe Howell, Harrison B. Prosper
C-   Updated  23-MAR-1993   Lupe Howell  Replace 1 for IDEV in the JIQDEV call 
C-                     to avoid problems when in batch mode
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      INTEGER I, J, K, L, II, JJ, LL, JER
      REAL    XMAG, VECTOR(3), TESTV(3), VMOD, XNORM
      REAL    TEMP_UPVEC(3)
      REAL    VXMIN, VXMAX, VYMIN, VYMAX
      REAL    XMIN,   XMAX,  YMIN,  YMAX
      CHARACTER*3 COLOR
      LOGICAL PU_PICKED,ELABEL,EZERROR
C-
      DATA COLOR /'FOR'/
C----------------------------------------------------------------------
C-
C--- Get ELABEL for the special VPORTXMIN to display Trigger List
C-
      CALL EZPICK('PX_SYSTEM_RCP')
      IF ( EZERROR(JER) ) THEN
        CALL ERRMSG('PIXIE','PUHEAD','Bank PX_SYSTEM_RCP NOT FOUND',
     &     'W')
        ELABEL=.FALSE.
      ELSE
        CALL PUGETV('BEAM X-ING NUM',ELABEL)
        CALL EZRSET
      ENDIF
C
C ****  Get screen parameters from RCP
C
      CALL PU_GET_SCREEN_PARAM(IDX,'VIEW3D',VIEW3D,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('NOTFOUND','PU_SETUP_VIEWPORT',
     &    'Could not find parameter VIEW3D','W')
        GOTO 999
      ENDIF
      CALL PU_GET_SCREEN_PARAM(IDX,'VPORTXMIN',XMINPRT,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VPORTXMAX',XMAXPRT,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VPORTYMIN',YMINPRT,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VPORTYMAX',YMAXPRT,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMIN',XMINWID,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMAX',XMAXWID,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMIN',YMINWID,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMAX',YMAXWID,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VIEWREFX',VIEWREFX,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VIEWREFY',VIEWREFY,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'VIEWREFZ',VIEWREFZ,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'UPVECX',TEMP_UPVEC(1),IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'UPVECY',TEMP_UPVEC(2),IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'UPVECZ',TEMP_UPVEC(3),IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'CAMX',CAMX,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'CAMY',CAMY,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'CAMZ',CAMZ,IER)
      IF ( ELABEL ) THEN
        XNORM = XMAXPRT(1) - XMINPRT(1)
        XNORM = XNORM*.15
        XMINPRT(1) = XMINPRT(1) + XNORM
        IF (XMAXPRT(1) .LT. .9) THEN
          XMAXPRT(1) = XMAXPRT(1) + XNORM
        ENDIF
      ENDIF
C
C ****  Setup VIEWPORT
C
      XVPRT1 = XMINPRT(1) * XMVPRT + XCVPRT
      XVPRT2 = XMAXPRT(1) * XMVPRT + XCVPRT
      YVPRT1 = YMINPRT(1) * YMVPRT + YCVPRT
      YVPRT2 = YMAXPRT(1) * YMVPRT + YCVPRT
C
      CALL JVPORT( XVPRT1, XVPRT2, YVPRT1, YVPRT2 )
C
C ****  Setup WINDOW
C
      XWIND1 = XMINWID(1)
      XWIND2 = XMAXWID(1)
      YWIND1 = YMINWID(1)
      YWIND2 = YMAXWID(1)
      IF ( YWIND2 .LT .YWIND1 ) THEN  ! Defined by center and x-size
        XMAG = ( YVPRT2 - YVPRT1 ) / ( XVPRT2 - XVPRT1 ) *
     &           ( XMAXWID(1) - XMINWID(1) )
        YWIND2 = YWIND1 + .5 * XMAG
        YWIND1 = YWIND1 - .5 * XMAG
C
C ****  Update Window parameters in Y
C
        CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWYMIN',YWIND1,IER)
        CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWYMAX',YWIND2,IER)
      ENDIF
C
      CALL JWINDO( XWIND1, XWIND2, YWIND1, YWIND2 )
C
C ****  Draw a rectangle around the viewport
C
      CALL PU_2DRECT(XWIND1,YWIND1,XWIND2,YWIND2,COLOR)
C
C ****  Setup 3D transformation
C
      IF ( VIEW3D(1) ) THEN
        CALL JIQDEV( IDEV, 17, I )             ! Real 3-D driver ?
        IF( I .NE. 0 ) CALL JDD3D( .TRUE. )
        CALL JPARAL
      ENDIF
C
      CALL JVUPNT(VIEWREFX(1),VIEWREFY(1),VIEWREFZ(1))
C
      IF( (TEMP_UPVEC(1) .EQ. 0.0) .AND.
     &    (TEMP_UPVEC(2) .EQ. 0.0) .AND.
     &    (TEMP_UPVEC(3) .EQ. 0.0) )THEN
        CALL ERRMSG('NULL_UP','PU_SETUP_VIEWPORT',
     &    ' Up vector is null','W')
        IER =-1
        GOTO 999
      ENDIF
C
      UPVEC(1,1) = TEMP_UPVEC(1)
      UPVEC(2,1) = TEMP_UPVEC(2)
      UPVEC(3,1) = TEMP_UPVEC(3)
C
      CALL JUPVEC( UPVEC(1,1), UPVEC(2,1), UPVEC(3,1) )
C
      VECTOR(1) = VIEWREFX(1) - CAMX(1)
      VECTOR(2) = VIEWREFY(1) - CAMY(1)
      VECTOR(3) = VIEWREFZ(1) - CAMZ(1)
C
      IF( VMOD( VECTOR,3 ) .EQ. 0. ) THEN
        CALL ERRMSG('BAD_REF_POINT','PU_SETUP_VIEWPORT',
     &      ' Camera is on the View Reference Point ','W')
        IER =-2
        GOTO 999
      ENDIF
C
      CALL CROSS( VECTOR, TEMP_UPVEC, TESTV )
      IF( VMOD(TESTV,3) .EQ. 0. ) THEN
        CALL ERRMSG('BAD_UP_VECTOR','PU_SETUP_VIEWPORT',
     &      ' The UP vector is on the Camera line ','W')
        IER =-3
        GOTO 999
      ENDIF
C
      CALL JNORML( VECTOR(1), VECTOR(2), VECTOR(3) )
C
      IER = 0
      RETURN
C
C
C ****  Return size set by PU_SETUP_VIEWPORT
C
      ENTRY PU_GET_VIEWPORT_SIZE
     &  (VXMIN,VXMAX,VYMIN,VYMAX,XMIN,XMAX,YMIN,YMAX)
C
C ****  VIEWPORT values
C
      VXMIN = XVPRT1
      VXMAX = XVPRT2
      VYMIN = YVPRT1
      VYMAX = YVPRT2
C
C ****  WINDOW values
C
      XMIN = XWIND1
      XMAX = XWIND2
      YMIN = YWIND1
      YMAX = YWIND2
  999 RETURN
      END
