      LOGICAL FUNCTION HLPSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print MENU ID
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0COM.INC'
C
      CHARACTER*2 ID
C----------------------------------------------------------------------
      HLPSAM = .TRUE.
      WRITE(UNIT=ID,FMT='(I2)') IDSAM
      WRITE(LOUT,*) ' '//ID//' - SAM'
C
  999 RETURN
      END
