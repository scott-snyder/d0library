C----------------------------------------------------------------------
C-
C-   Created  24-JAN-1991   Harrison B. Prosper; Hmatrix stack
C-
C----------------------------------------------------------------------
      INTEGER HMTR_STACK_MAX
      PARAMETER( HMTR_STACK_MAX = 10 )
      INTEGER HMTR_STACK_PTR,HMTR_STACK(HMTR_STACK_MAX)
      COMMON /IHMATRIX/ HMTR_STACK_PTR,HMTR_STACK
