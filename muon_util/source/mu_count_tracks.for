      SUBROUTINE MU_COUNT_TRACKS(NMUOT,NASTUB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : count the number of MUOT tracks, and the 
C-                         number which are A stubs
C-
C-   Inputs  : None
C-   Outputs : NMUOT   total number of MUOT tracks
C-             NASTUB  total number of A stub tracks in MUOT banks
C-
C-   Created  18-APR-1994   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUOT.LINK'
C
      INTEGER NMUOT,NASTUB,IQUAD
      INTEGER LMTRH,GZMTRH,LMUOT,IFW1
C----------------------------------------------------------------------
      NMUOT = 0
      NASTUB = 0
      LMTRH = GZMTRH(0)
      IF (LMTRH .EQ. 0) GO TO 999
      LMUOT = LQ(LMTRH - IZMUOT)
   10 CONTINUE
      IF (LMUOT .EQ. 0) GO TO 999
      IFW1 = IQ(LMUOT + 4)
      IQUAD = IQ(LMUOT + 3)
      IF (IQUAD .LE. 12) NMUOT = NMUOT + 1
      IF (IFW1.EQ.5 .OR. IFW1.EQ.15) NASTUB = NASTUB + 1
      LMUOT = LQ(LMUOT)
      GO TO 10
  999 RETURN
      END
