C----------------------------------------------------------------------
C-
C-   Created   2-OCT-1990   DrMikie
C-
C----------------------------------------------------------------------
      REAL XTUPLE
      INTEGER NTUPLEN
      PARAMETER (NTUPLEN=80)
C&IF VAXVMS
C&      COMMON /TB90_NTUPLE/ 
C&ELSE
      COMMON /TB90_NTUPLE_INC/ 
C&ENDIF
     &  XTUPLE(NTUPLEN)
