      LOGICAL FUNCTION HLPTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give on-line help on TRD package.
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
      HLPTRD = .TRUE.
      WRITE(UNIT=ID,FMT='(I2)') IDTRD
      WRITE(LOUT,*) ' '//ID//' - TRD'
  999 RETURN
      END
