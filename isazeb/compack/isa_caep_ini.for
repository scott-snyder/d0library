      FUNCTION ISA_CAEP_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Initialize for generating CAEP bank directly from ISAJET data
C-
C-   Returned value  : true if succesfull
C-
C-   Created  29-JUN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL ISA_CAEP_INI,CALOR_INI
C----------------------------------------------------------------------
      ISA_CAEP_INI=CALOR_INI()       ! get calorimeter geometry
      CALL PATHDF('FAKE')         ! set path default to fake data
      CALL PATHRS
      CALL INTMSG(' Default PATH set to FAKE by ISA_CAEP_INI')
  999 RETURN
      END
