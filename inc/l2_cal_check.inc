C----------------------------------------------------------------------
C-
C-   Created  20-JUL-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      INTEGER PPAT, LAST_PPAT         
      LOGICAL OK, OK_MASK             
      LOGICAL TRIED_MASK              
      INTEGER NEVENTS                 
      LOGICAL LAST_FOR_THIS_PATTERN   
      INTEGER NCMAX
      PARAMETER( NCMAX = 156*NPRBOX + 156) 
C     Declare output list of channels. Each channel is described by
C     the five numbers needed for the ADC labeling scheme:
      INTEGER LCRATE(NCMAX), LADC(NCMAX), LBLS(NCMAX), LROTOW(NCMAX)
      INTEGER LDEPTH(NCMAX)
      COMMON /L2_CAL_CHECK/ PPAT, LAST_PPAT, OK, OK_MASK, NEVENTS,
     &  LAST_FOR_THIS_PATTERN, LCRATE, LADC, LBLS, LROTOW, LDEPTH,
     &  TRIED_MASK
