      SUBROUTINE JIQDEV(DSPDEV,ICODE,LIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return an integer charcteristic of a specified
C-   device.   Properties of the display device.
C-
C-   Inputs  : DSPDEV   [I]:  Display device
C-             ICODE    [I]:  Code thta determines what information is being
C-                            requested.
C-   Outputs : LIST     [I]:  Integer with the characteristic asked
C-   Controls: None
C----------------------------------------------------------------------
      INCLUDE 'SYS$LIBRARY:DECW$XLIBDEF.FOR'
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      RECORD /X$VISUAL/ VISUAL            ! visual type
      record /X$WINDOW_ATTRIBUTES/ WATT    ! Window attribute type
      INTEGER SCREEN                      ! screen id
      INTEGER DEPTH                       ! number of planes
C----------------------------------------------------------------------
      DATA IQD/12,1,38,10,0,5,14,32767,0,60,
     &         445,1,1,2,0,2,0,1,2,0,
     &         0,0,0,0,1,0,0,0,0,0,
     &         0,0,0,0,0,0,0,0,0,0,
     &         1,1,1,2,1,1,1/
      INTEGER DSPDEV,NVINFO_TEMPLATE
      DIMENSION LIST(4),IDIMNS(4,4)
      DATA IDIMNS/ 0,913,0,761,
     &             76,836,0,761,
     &             0,229,0,249,
     &             25,274,0,249 /
C----------------------------------------------------------------------
C
C ****  Finding out the number of planes
C
      SCREEN=X$DEFAULT_SCREEN_OF_DISPLAY(VD_ID)
      DEPTH=X$DEFAULT_DEPTH_OF_SCREEN(SCREEN)
C
C ****  Getting Visual information
C
      CALL X$DEFAULT_VISUAL_OF_SCREEN(SCREEN,VISUAL)
C
C ****  Finding out if the monitor is color, gray scale,
C ****  or black and white
C
      IF ( ICODE .EQ. 1 ) THEN
C
C ****  If here, this has a color monitor
C ****  If DEPTH is 4 the device has four bit planes so it only has 11 colors
C ****  If DEPTH is 8 the device has eight bit planes, it has 256 colors.
C
        IF (VISUAL.X$L_VISU_CLASS .EQ. X$C_TRUE_COLOR .OR.
     &     VISUAL.X$L_VISU_CLASS .EQ. X$C_PSEUDO_COLOR .OR.
     &     VISUAL.X$L_VISU_CLASS .EQ. X$C_DIRECT_COLOR .OR.
     &     VISUAL.X$L_VISU_CLASS .EQ. X$C_STATIC_COLOR) THEN
          IF ( DEPTH .EQ. 4 ) THEN 
            LIST(1) = 11 
          ELSEIF ( DEPTH .EQ. 8 ) THEN
            LIST(1) = 256
          ENDIF

C...      WRITE(6,*)' COLOR DEVICE !!'
C
C ****  If here, this has gray scales
C
        ELSEIF (VISUAL.X$L_VISU_CLASS .EQ. X$C_GRAY_SCALE .OR.
     &     VISUAL.X$L_VISU_CLASS .EQ. X$C_STATIC_GRAY) THEN
          LIST(1) = 2
          WRITE(6,*)' INTENSITY DEVICE !!'
        ELSE
C
C ****  If here, this is black and white
C
          LIST(1) = 2
C...      WRITE(6,*)' BLACK/WHITE DEVICE !!'
        ENDIF
C
C ****  Exit if the code asked is out of range
C
        ELSEIF (ICODE.LT.1.OR.ICODE.GT.47) THEN
          GOTO 999
C
C ****  Request resolution and dimension of display
C
        ELSEIF( ( ICODE.GE.20 ) .AND. ( ICODE.LE.23 ) )THEN
          J=ICODE-19
          DO I=1,4
            LIST(I)=IDIMNS(I,J)
          ENDDO
      ENDIF
 999  RETURN
      END
