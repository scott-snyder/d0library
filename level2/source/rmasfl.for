      SUBROUTINE RMASFL(PARAM_NUM,LRMAS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Fills the Inv Mass result bank 
C-
C-   Inputs  : Values of pair to be booked, and number of parameter sets
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-DEC-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:RMAS.PARAMS'   ! params for RMAS
      INCLUDE 'D0$INC:RMAS_VALUES.INC'  ! values for RMAS
      INTEGER POS_SET
      INTEGER LRMAS,PARAM_NUM
C----------------------------------------------------------------------
C  The position of the beginning of the value set for the current 
C     paramter set is the position of the RMAS bank, plus the number 
C     of initial words, plus (the number of repeated times the 
C     parameter set minus 1)
C
      POS_SET = LRMAS+NMAIN_RMAS+(NREP_RMAS*(PARAM_NUM-1))
C
C For each parameter set, one pair is filled into the bank. If the event passed
C   the parameter set's cuts, then it is the pair that passed that is filled.
C   If the event failed, then it is the largest mass pair that is filled if
C   there is no mass maximum, and if there is a mass maximum it is the largest
C   mass pair provided that its mass is less than the mass maximum.
C
      IQ(POS_SET+PSTAT) = PAIR_STATUS
      IQ(POS_SET+POBJT) = PAIR_TYPE
      IQ(POS_SET+POBJN) = OBJN_VAL
C
      Q(POS_SET+PMASS)  = PAIR_MASS
      Q(POS_SET+PETAB)  = PAIR_BOOST
      Q(POS_SET+PET1)   = PAIR_VAL(1,1)
      Q(POS_SET+PETA1)  = PAIR_VAL(1,2)
      Q(POS_SET+PPHI1)  = PAIR_VAL(1,3)
      Q(POS_SET+PET2)   = PAIR_VAL(2,1)
      Q(POS_SET+PETA2)  = PAIR_VAL(2,2)
      Q(POS_SET+PPHI2)  = PAIR_VAL(2,3)
C
  999 RETURN
      END
