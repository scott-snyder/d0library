      SUBROUTINE JCLEAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   This module is used to delete all retained segments.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  03-OCT-1988   A. VIRGO
C-   UPDATED  19-AUG-1990   S. ABACHI      SEGINF(1,1) replaced by SEGINF(1,I)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      EXTERNAL ERRHND
      INTEGER I, NAMSEG
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
      IF (SEGOPN) THEN
         CALL ERROR('JCLEAR: A SEGMENT IS OPEN')
      ENDIF
      IF (NSEGS .GT. 0) THEN
         DO 10 I=NSEGS,1,-1
            NAMSEG = SEGINF(1,I)
            IF (NAMSEG .GT. 0) 
     +             CALL KDELS(NAMSEG) 
   10    CONTINUE
      ENDIF
      NSEGS = 0
      NRSEG = 0
      NPRIM = 0
C-
C=== Disconnects Hardware Dial and Clear the EAS Memory.
C-
      CALL HDIAL_OFF
C-
C
      RETURN
      END
