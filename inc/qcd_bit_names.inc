C----------------------------------------------------------------------
C-
C-   Created   9-DEC-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
C: QCD names
      INTEGER QCD_BIT_MAX
      PARAMETER( QCD_BIT_MAX = 64 )
      CHARACTER*40 QCD_BIT_NAME( 0:63)   
      COMMON /QCD_BIT_NAMES/ QCD_BIT_NAME
