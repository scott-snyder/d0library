      SUBROUTINE CL1_GAPXST(L1ETAC,L1PHIC,CCMGXST,ECMGXST,ICDXST,OR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns if there is a MG or ICD anywhere
C-                         within the region of the TT specified by
C-                         (L1ETAC,L1PHIC). 
C-
C-   Inputs  : L1ETAC,L1PHIC give the location of the TT.
C-
C-   Outputs : CCMGXST,ECMGXST,ICDXST individualy return .TRUE. if there
C-             are CCmg's,ECmg's,or ICD's in the trigger tower (otherwise
C-             .FALSE.). "OR" = ccmgxst.OR.ecmgxst.OR.icdxst.
C-
C-   Controls: NONE.
C-
C-   Created  26-JUL-1989   Dale A. Ross, MSU
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
        INTEGER L1ETAC    !_
        INTEGER L1PHIC    ! |-->The usual L-1 2d indices.
        LOGICAL OR        ! = ccmgxst.OR.ecmgxst.OR.icdxst
*
C     Local Variables:
*
        LOGICAL CEXIST
        INTEGER IETAC1,IETAC2
        INTEGER IETAC,IPHIC
        LOGICAL ITSOK
*
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
*
C     _______________________________________________________
C     -------------------------------------------------------
*
C     --Convert the L-1 indices to L-2 but keeping and listing 
C       (any) two eta canadates. Here we are only interested in existance.
C       If a gap detector exists there be a detector for all phi. Within
C       a trigger tower there is up to two divisions in eta. Thus, for
C       any (fixed) phi we only have to check the the address at the eta(s).
*
      ITSOK = .TRUE.
      IPHIC = 2*L1PHIC
*
C     ----List the two possible IETAC values:
*
      IF (ABS(L1ETAC) .LT. MNCTTE) THEN
           IETAC1 = SIGN((ABS(2*L1ETAC) - 1),L1ETAC)
C                                           ! This gives the lower
C                                           !  of the two possible index
C                                       values;
           IETAC2 = SIGN(ABS(2*L1ETAC),L1ETAC) ! 2*l1etac gives the
C                                        ! other.
         ELSE IF (ABS(L1ETAC) .LE. NETAL1) THEN
           IETAC1 = SIGN((ABS(L1ETAC) + MNCTTE-1), L1ETAC)
           IETAC2 = 0  !  ...since there is only one possible index
C                      !  in this case.
         ELSE
           IETAC1 = 0  !  For lack of knowing what to do with invalid codes.
           IETAC2 = 0
           ITSOK = .FALSE.
      ENDIF
*
      IF (ITSOK) THEN
         DO IETAC = IETAC1,IETAC2,ABS(IETAC1-IETAC2)
           IF (CEXIST(IETAC,IPHIC,8))  CCMGXST = .TRUE.
           IF (CEXIST(IETAC,IPHIC,9))  ICDXST  = .TRUE.
           IF (CEXIST(IETAC,IPHIC,10)) ECMGXST = .TRUE.
         END DO
      ENDIF
      OR = CCMGXST .OR. ICDXST .OR. ECMGXST
*
  999 RETURN
      END
