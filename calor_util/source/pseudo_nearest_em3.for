      LOGICAL FUNCTION PSEUDO_NEAREST_EM3(IETA1,IPHI1,ILYR1,IETA2,IPHI2,
     +  ILYR2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns True if two em3 cells are next to each
C-   other, or catercorner
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-AUG-1995   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Integer IETA1,IPHI1,ILYR1,IETA2,IPHI2,ILYR2
      Real RIETA1,RIPHI1,RIETA2,RIPHI2
C----------------------------------------------------------------------
      PSEUDO_NEAREST_EM3 = .FALSE.
      Call Get_EM3_RIETA_RIPHI(IETA1,IPHI1,ILYR1,RIETA1,RIPHI1) 
      Call Get_EM3_RIETA_RIPHI(IETA2,IPHI2,ILYR2,RIETA2,RIPHI2)
      If ((abs(rieta1-rieta2).lt.0.7).or.
     +  ((abs(rieta1).lt.1).and.(abs(rieta2).lt.1))) Then
        If (abs(riphi1-riphi2).lt.0.7) then 
          PSEUDO_NEAREST_EM3 = .TRUE.  
        Else If ((MAX(riphi1,riphi2).gt.64).and.
     +    (MIN(riphi1,riphi2).lt.1.)) then 
          PSEUDO_NEAREST_EM3 = .TRUE.
        Endif
      Endif
  999 RETURN
      END
