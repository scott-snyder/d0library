      LOGICAL FUNCTION HLPD0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Provide on-line help for D0 package.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-   Calledby: LUIGET
C-   Created   6-JUN-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0COM.INC'
      CHARACTER*2 ID
C----------------------------------------------------------------------
      HLPD0 = .TRUE.
      WRITE(UNIT=ID,FMT='(I2)') IDZERO
      WRITE(LOUT,*) ' '//ID//' - DZERO'
  999 RETURN
      END
