      SUBROUTINE CL2_GAPXST(IETAC,IPHIC,CCMGXST,ECMGXST,ICDXST,OR)
C----------------------------------------------------------------------
C-
C-   CL2_GAPXST = (Calorimeter) L-2 GAP eXiST
C-
C-   Purpose and Methods : For a given (IETAC,IPHIC) we return whether
C-                         if there is a MG or ICD at the location.
C-                         Useage is similar to CL1_GAPXST. Depends on
C-                         SP's routine CEXIST
C-
C-   Inputs  : IETAC,IPHIC give the L-2 location.
C-
C-   Outputs : CCMGXST,ECMGXST,ICDXST are true if there is a CC MG,
C-             EC MG, or ICD here. OR is the .OR. of these three
C-             values.
C-
C-   Controls: None.
C-
C-   Created  23-AUG-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
*
      IMPLICIT NONE!
*
C     Passed Variables:
*
        LOGICAL CCMGXST   ! = Central Calorimeter MG eXiST.
        LOGICAL ECMGXST   ! = End Cap calorimeter MG eXiST.
        LOGICAL ICDXST    ! = ICD eXiST.
        INTEGER IETAC     !_
        INTEGER IPHIC     ! |-->The usual L-2 2d indices.
        LOGICAL OR        ! = ccmgxst.OR.ecmgxst.OR.icdxst
*
C     Local Variables:
*
        LOGICAL CEXIST
*
C     _______________________________________________________
C     -------------------------------------------------------
*
      IF (CEXIST(IETAC,IPHIC,8))  CCMGXST = .TRUE.
      IF (CEXIST(IETAC,IPHIC,9))  ICDXST  = .TRUE.
      IF (CEXIST(IETAC,IPHIC,10)) ECMGXST = .TRUE.
      OR = CCMGXST .OR. ICDXST .OR. ECMGXST

  999 RETURN
      END
