      SUBROUTINE ECMH_UNIT(XINC,YINC,XOUC,YOUC,XTOP,YTOP,
     &                     YCURS,THICK,FLAG,
     &                     PARAM,XPOS,YPOS,ZPOS,VOLUME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EC Plate/Board/Gap/Mother is generated.
C-                         Units are converted to centimeters.
C-   *****************************************************************
C-   *    WARNING >>>----> All inputs are in inches                  *
C-   *                     All outputs are in cm !                   *
C-   *****************************************************************
C-
C-   The plate volume will be two 'TRD1' volumes.
C-
C-   Inputs  : X,Y,Z of 3 corners and the thickness (units = inches)
C-   Outputs : 4 parameters for the dunce-cap TRD1 (units = cm)
C-             4 parameters for the base TRD1 (units = cm)
C-             the X,Y,Z center coordinates (unit = cm)
C-             the volume of the plate (units = cm**3).
C-   Controls: none
C-
C-   Created  12-DEC-1989   Norman A. Amos
C-   Modified 26-JAN-1990   Changed from TRAP to TRD1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER FLAG
      REAL PARAM(4),XPOS,YPOS,ZPOS
      REAL XINC,YINC,XOUC,YOUC,XTOP,YTOP,YCURS,THICK
      REAL CAP_VOLUME,BASE_VOLUME,TOTAL_VOLUME,VOLUME
      REAL CM_PER_INCH
      PARAMETER( CM_PER_INCH = 2.54 )
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C- SET THE GEANT SRCP PARAMETERS FOR THE EC PLATES AS 2 TRAPEZOIDS.
C
C          Z                     * -----------+
C                            *       *        +- CAP
C          ^             *               * ---+
C          |              *             *     |
C          |               *           *      |
C          |                *         *       +- BASE
C          |                 *       *        |
C          +---------->  X    ******* --------+
C
C       NUMBER OF PARAMETERS = 4 (FOR 'TRD1')
C       PARAMETERS:
C               1 = X HALF LENGTH AT LOWER Z SURFACE
C               2 = X HALF LENGTH AT HIGHER Z SURFACE
C               3 = Y HALF LENGTH
C               4 = Z HALF LENGTH
C----------------------------------------------------------------------
      GOTO (100,200,300) FLAG
C----------------------------------------------------------------------
C- GENERATE THE CAP.
C----------------------------------------------------------------------
100   PARAM(1) = XOUC*CM_PER_INCH
      PARAM(2) = 0.
      PARAM(3) = 0.5*THICK*CM_PER_INCH
      PARAM(4) = 0.5*ABS(YTOP-YOUC)*CM_PER_INCH
      XPOS=0.
      YPOS=(YCURS+0.5*THICK)*CM_PER_INCH
      ZPOS=0.5*(YTOP+YOUC)*CM_PER_INCH
      CAP_VOLUME = (YTOP-YOUC)*XOUC*THICK*CM_PER_INCH**3
      VOLUME = CAP_VOLUME
      RETURN
C----------------------------------------------------------------------
C- GENERATE THE BASE.
C----------------------------------------------------------------------
200   PARAM(1) = XINC*CM_PER_INCH
      PARAM(2) = XOUC*CM_PER_INCH
      PARAM(3) = 0.5*THICK*CM_PER_INCH
      PARAM(4) = 0.5*ABS(YOUC-YINC)*CM_PER_INCH
      XPOS=0.
      YPOS=(YCURS+0.5*THICK)*CM_PER_INCH
      ZPOS=0.5*(YOUC+YINC)*CM_PER_INCH
      BASE_VOLUME = (2.*XINC+ABS(XOUC-XINC))
     &              *ABS(YOUC-YINC)*THICK*CM_PER_INCH**3
      VOLUME = BASE_VOLUME
      RETURN
C----------------------------------------------------------------------
C- GENERATE THE TOTAL VOLUME.
C----------------------------------------------------------------------
300   TOTAL_VOLUME = CAP_VOLUME + BASE_VOLUME
      VOLUME = TOTAL_VOLUME
      RETURN
      END
