C=======================================================================
      SUBROUTINE ISZOPT(ID)
C=======================================================================
C
C  Description:  Decides on a color and linestyle, depending on the
C  ============  ID of the particle.
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - December 30, 1987
C  Updated Jan 4, 1990 - LUpe Howell Implementing color table
C
C=================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
C
C  Local Declarations:
C  ====================
C
      INTEGER I,ID
      INTEGER TERCOD
      LOGICAL LASER,LOGSCA,NEUTLS,NEUSON
      DATA LASER/.FALSE./
      DATA TERCOD/4/
C
C  Executable Code:
C  =================
C
C
C  Check to see if the particle is a neutrino..If it is, and the 
C  neutrino flag is set to TRUE, then plot the track, if not skip
C  the track plotting code. (go to 46)
C  ==============================================================
C
      CALL PUGETV('NEUTRINOS PLOTTED',NEUSON)
      IF((ABS(ID) .EQ. 11).OR.(ABS(ID).EQ.13)
     X     .OR.(ABS(ID) .EQ. 15)) THEN
        IF (NEUSON) THEN
           IF(LASER) THEN
              CALL PXLNSTYL(3)
           ELSE
              CALL PXCOLR('YEL')
              IF (TERCOD .EQ. 4) CALL PXLNSTYL(2)
              IF (TERCOD .EQ. 17) CALL PXLNSTYL(4)
           ENDIF
         ENDIF
      ENDIF
C
C  Check to see if the particle is an electron... change colors if
C  it is... and plot the track.
C  ==================================================================
C
      IF (ABS(ID) .EQ. 12) THEN
         IF (LASER) THEN
            CALL PXLNSTYL(2)
         ELSE
            CALL PXCOLR('WHI')
            IF (TERCOD .EQ. 4) CALL PXLNSTYL(6)
            IF (TERCOD .EQ. 17) CALL PXLNSTYL(2)
         ENDIF
      ENDIF
C
C  Check to see if the particle is a muon. If so, set the color to red
C  and plot the track.
C  ====================================================================
C
      IF (ABS(ID) .EQ. 14) THEN
         IF (LASER) THEN
            CALL PXLNSTYL(2)
         ELSE
            CALL PXCOLR('RED')
            IF (TERCOD .EQ. 4) CALL PXLNSTYL(6)
            IF (TERCOD .EQ. 17) CALL PXLNSTYL(2)
         ENDIF
      ENDIF
C
      RETURN
      END
