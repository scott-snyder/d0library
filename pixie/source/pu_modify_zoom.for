      SUBROUTINE PU_MODIFY_ZOOM(IDX,XMIN,XMAX,YMIN,YMAX,
     &                              VXMIN,VXMAX,VYMIN,VYMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-    1) Get the current window parameter values from the PXSCREEN array.
C-
C-    2) Calculate the new window parameters and update the PXSCREEN array.
C-
C-   Inputs  : IDX      [I]     Index of the screen to be modified
C-
C-             XMIN     [R]     Boundary of area to be zoomed in
C-             XMAX             viewport coordinates.
C-             YMIN
C-             YMAX
C-
C-             VXMIN    [R]     Boundary of viewport to be zoomed in
C-             VXMAX            viewport coordinates.
C-             VYMIN
C-             VYMAX
C-
C-   Outputs : None
C-   Controls:
C-
C-   Created  18-OCT-1990   Lupe Howell, Harrison B. Prosper
C-   Updated  14-JAN-1991   Harrison B. Prosper
C-      Removed first argument; no longer needed
C-   Updated   7-MAY-1991   Harrison B. Prosper
C-      Use XMIN, XMAX etc. in arguments, re-write
C-   Updated   8-MAY-1991   Harrison B. Prosper
C-      Fix aspect ratio 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDX
      REAL    XMIN,XMAX,YMIN,YMAX
      REAL    VXMIN,VXMAX,VYMIN,VYMAX
C----------------------------------------------------------------------
      REAL    WXMIN,WXMAX,WYMIN,WYMAX,DWX,DVX,DWY,DVY,DX,DY
      REAL    WXWID,WYWID,WXCEN,WYCEN,XSCALE,YSCALE
      INTEGER IER
C----------------------------------------------------------------------
C
C ****  Get the CURRENT window values
C
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMIN',WXMIN,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWXMAX',WXMAX,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMIN',WYMIN,IER)
      CALL PU_GET_SCREEN_PARAM(IDX,'WINDOWYMAX',WYMAX,IER)
C
C ****  Compute current widths in window coordinates
C
      DWX = WXMAX - WXMIN
      DWY = WYMAX - WYMIN
C
C ****  Compute current widths in viewport coordinates
C
      DVX = VXMAX - VXMIN
      DVY = VYMAX - VYMIN
C
      IF ( (DWX.LE.0.0) .OR.
     &     (DWY.LE.0.0) .OR.
     &     (DVX.LE.0.0) .OR.
     &     (DVY.LE.0.0) ) THEN
        CALL ERRMSG('BAD_COORD','PU_MODIFY_ZOOM',
     &    'Bad viewport or window values','W')
        GOTO 999
      ENDIF
C
C ****  Compute minimum width and height which bounds the zoom area
C
      DX = XMAX - XMIN
      DY = YMAX - YMIN
      IF ( DY .GT. DX ) THEN
        DX = DVX*(DY/DVY)               ! Adjust X-width
      ELSE
        DY = DVY*(DX/DVX)               ! Adjust Y-width
      ENDIF
C
C ****  Compute X-center and width of area to be zoomed
C
      XSCALE= DWX/DVX                   ! Conversion from viewport to window
      WXWID = XSCALE*DX
      WXCEN = WXMIN + XSCALE*(0.5*(XMAX+XMIN)-VXMIN)
C
C ****  Compute Y-center and width of area to be zoomed
C
      YSCALE= DWY/DVY
      WYWID = YSCALE*DY
      WYCEN = WYMIN + YSCALE*(0.5*(YMAX+YMIN)-VYMIN)
C
C ****  Compute new min, max in window coordinates
C
      WXMIN = WXCEN - 0.5*WXWID
      WXMAX = WXCEN + 0.5*WXWID
      WYMIN = WYCEN - 0.5*WYWID
      WYMAX = WYCEN + 0.5*WYWID
C
C **** Store NEW window values
C
      CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWXMIN',WXMIN,IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWXMAX',WXMAX,IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWYMIN',WYMIN,IER)
      CALL PU_SET_SCREEN_PARAM(IDX,'WINDOWYMAX',WYMAX,IER)
  999 RETURN
      END
