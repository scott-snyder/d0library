      SUBROUTINE D0XINT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Controls interactive processing
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-DEC-1988   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCTIME.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
C----------------------------------------------------------------------
C&IF DI3000
C&      CALL GDINTR   !Replaced from GINTRI
C&ELSE
      CALL GINTRI
C&ENDIF
      CALL TIMEX(TIMINT)   !     KEEP STARTING TIME
      WRITE(LOUT,*)
     & ' =====> GEANT ready, Type your commands in UPPER CASE'
      IEVENT=0
C&IF DI3000
C&      CALL GDINT   !Replaced from GINTER
C&ELSE
      CALL GINTER
C&ENDIF
  999 RETURN
      END
