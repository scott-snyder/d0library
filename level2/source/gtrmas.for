      SUBROUTINE GTRMAS(PARAM_NUM,STATUS,OBJECT,NUM_OBJ,MASS,
     &        ETABOOST,ET1,ETA1,PHI1,ET2,ETA2,PHI2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To get the values of the Inv Mass result bank
C-               for a particular paramset
C-
C-   Inputs  : Parameter set number
C-   Outputs : STATUS      -1 if pair passed event
C-                          0 if param set never looked at event
C-         Otherwise to determine how many objects failed each cut, the 
C-         status word is the sum of the following:
C-                        A*1 + B*100 + C*10000 = STATUS
C-         Where A is the number of pairs that failed the 
C-         eta range cut, 
C-               B is the number of pairs that failed the
C-         eta boost cut,
C-               C is the number of pairs that failed the 
C-         mass cut.
C-         EXAMPLE:  STATUS=30205  3 pairs failed the mass cut
C-                                 2 pairs failed the eta boost cut  
C-                                 5 pairs failed the eta range cut
C-
C-             OBJECT   1=jet, 2=electron or photon, 3 = muon
C-             NUM_OBJ  number of objects in the event, number
C-                      of pairs is NUM_OBJ*(NUM_OBJ-1)/2
C-             MASS     of pair
C-             ETABOOST of pair
C-             ET1,ETA1,PHI1,ET2,ETA2,PHI2 values of each object in pair
C-             
C-         The pair filled for the parameter set is either:
C-             - If the event passed, it is the pair that passed the event.
C-         OR  - If the event failed, it is the pair with the largest
C-                mass, or if there is a MASS MAXIMUM, it is the pair with
C-                the largest mass provided that it is less than the MASS
C-                MAXIMUM.    
C-              
C-   Controls: 
C-
C-   Created  15-DEC-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:RMAS.PARAMS'
      INTEGER POS_SET,GZRMAS
      INTEGER LRMAS,PARAM_NUM
      INTEGER STATUS,OBJECT,NUM_OBJ
      REAL MASS,ETABOOST,ET1,ETA1,PHI1,ET2,ETA2,PHI2
C----------------------------------------------------------------------
C  The position of the beginning of the value set for the desired
C     parameter set is the position of the RMAS bank, plus the number
C     of initial words, plus (the number of repeated times the
C     parameter set minus 1)
C
      LRMAS = GZRMAS()
      POS_SET = LRMAS+NMAIN_RMAS+(NREP_RMAS*(PARAM_NUM-1))
C----------------------------------------------------------------------
C
      STATUS   = IQ(POS_SET+PSTAT) 
      OBJECT   = IQ(POS_SET+POBJT) 
      NUM_OBJ  = IQ(POS_SET+POBJN) 
      MASS     = Q(POS_SET+PMASS)                   
      ETABOOST = Q(POS_SET+PETAB)  
      ET1      = Q(POS_SET+PET1)   
      ETA1     = Q(POS_SET+PETA1)  
      PHI1     = Q(POS_SET+PPHI1)  
      ET2      = Q(POS_SET+PET2)   
      ETA2     = Q(POS_SET+PETA2)  
      PHI2     = Q(POS_SET+PPHI2)  
C
  999 RETURN
      END
