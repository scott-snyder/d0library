      SUBROUTINE CHECK_INTENSITY(DEVCOL,INTENSITY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines if the the device used is an
C-   intensity station or a 4 bit plane color station by using the 
C-   PX_SYSTEM_RCP file parameter DEFTERM.
C-   If DEFTERM = 1 Uses the total number of colors in the device to determine
C-                  if on a color or B/W device.  
C-                  If DEVCOL >  11 (8 bit color dev) color INTENSITY = .FALSE.
C-                     DEVCOL <= 11 (4 bit dev) Black/white INTENSITY = .TRUE.
C-      DEFTERM = 2 forces the assumtion of the terminal is black/white
C-                  INTENSITY = .TRUE.
C-      DEFTERM = 3 forces the assumtion of the terminal is color 
C-                  INTENSITY = .FALSE.
C-      DEFTERM = 4 Uses the total number of colors in the device to determine
C-                  terminal type, like in option 1, but assumes grey scale 
C-                  device when hardcopy device is on
C-
C-   Inputs  : DEVCOL    [I]: Number of colors in the device
C-
C-   Outputs : INTENSITY [L]: Logical value that represents if a Intensity
C-                            VAX station is used (Grey shades) Black/White
C-                           .TRUE. The device used is an Intensity VAX station
C-                                  Black/white
C-                           .FALSE.The device is a color station
C-
C-   Created 10-APR-1991   Lupe Howell
C-   Updated  9-JUN-1992   Lupe Howell  Using DEFTERM to determine if it
C-                is an intensity terminal.
C-   Updated  22-FEB-1993   Lupe Howell  Adding DEFTERM 4
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  DEVCOL
      LOGICAL INTENSITY, DEFAULT 
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXCOMK.INC/LIST'
      INTEGER DEFTERM,FOUR_BIT_PLANE_COLORS
      PARAMETER( FOUR_BIT_PLANE_COLORS = 10 )
C----------------------------------------------------------------------
      DEFTERM = 0
      DEFAULT = .FALSE.
C
C ****  Getting the value of DEFTERM from RCP file
C ****  If there was no DEFTERM in rcp file use default value 1
C
      CALL EZPICK('PX_SYSTEM_RCP')
      CALL PUGETV('DEFTERM',DEFTERM)
      CALL EZRSET
      IF ( DEFTERM .EQ. 0 ) DEFTERM = 1
C
C ****  Using DEVCOL to determine the type of device
C ****  If number of colors in device >= 10 color else
C ****  assume B/W
C
      IF ( DEFTERM .EQ. 1 ) THEN
        DEFAULT = .TRUE.
C
C ****  Forcing assumption of Black/White device
C
      ELSEIF ( DEFTERM .EQ. 2 ) THEN
        INTENSITY = .TRUE.
C
C ****  Use grey scale for hardcopy, if NOT hardcopy
C ****  assume not intensity
C
      ELSEIF ( DEFTERM .EQ. 4 ) THEN
C
C ****  Check if the HARDCOPY device is ON if so 
C ****  assume hardcopy using intensity, if not use default 
C ****  intensity
C
        IF ( IDEV .EQ. 2 ) THEN
          INTENSITY = .TRUE.
C           CALL SETCOLTB
        ELSE
          DEFAULT = .TRUE.
        ENDIF
C
C ****  If none of the above assume Black/White for safety
C
      ELSE
        INTENSITY = .TRUE.
      ENDIF
C
C ****  Default setting for intensity
C ****  Using DEVCOL to determine the type of device
C ****  If number of colors in device >= 10 color else
C ****  assume B/W
C
      IF ( DEFAULT ) THEN
        IF ( DEVCOL .GE. FOUR_BIT_PLANE_COLORS ) THEN
          INTENSITY = .FALSE.
        ELSE
          INTENSITY = .TRUE.
        ENDIF
      ENDIF
  999 RETURN
      END
