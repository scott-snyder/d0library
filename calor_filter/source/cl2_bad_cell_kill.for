      SUBROUTINE CL2_BAD_CELL_KILL(CAEP_REMOVE,PNUT_REMOVE)
C
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drop bad cells
C-
C-   Inputs  : existing CAEP, PNUT, PTR2, CL2_BIG_CELLS
C-   Outputs : possibly modified version of above
C-   Controls: CAEP_REMOVE  if bad, set PTCAEP2 to zero so, while it's still in
C-                                PNUT, it's invisible
C-             PNUT_REMOVE  if bad, subtract out of PNUT (2)
C-
C-   Created  10-SEP-1992   James T. Linnemann
C-   Updated  27-OCT-1992   James T. Linnemann  produce PNUT(2)
C-
C----------------------------------------------------------------------
C-
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CL2_LINK.INC'
      INCLUDE 'D0$INC:CL2_STP_LINK.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:CL2_ETMISS_GEOM.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTCAEP2.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER ICOARSE
      REAL    EX,EY,ET,ET_BIG
      INTEGER IETA,IPHI,LYR,NBIG,I,L2PNUT_1,GZPNUT
      LOGICAL PNUT_REMOVE,CAEP_REMOVE,IN_CAEP,IN_PNUT
      LOGICAL CL2_CELL_ISOLATED,BAD_CELL
      INTEGER IBAD_CELL,NBAD
C----------------------------------------------------------------------
C
C...separate flags allow em, jets, and CAEPFL to have different options and
C   to call for removal separately
C
C...loop over all bad candidates
      CALL CL2_GET_N_BIG_CELL(NBIG)
      IF ((NBIG.GE.0).AND.PNUT_REMOVE)  THEN
        L2PNUT = GZPNUT(2)
        IF (L2PNUT.LE.0) THEN
          L2PNUT_1 = GZPNUT(1)    !copy original bank; new one is default
          IF (L2PNUT_1.GT.0) THEN
            CALL BKPNUT(2)
            L2PNUT = GZPNUT(2)
            IF (L2PNUT.GT.0) THEN
              DO I = 1,14
                Q(L2PNUT+I) = Q(L2PNUT_1+I)
              ENDDO
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C...check if already dealt with this cell, or no candidate
      NBAD = 0
      DO I = 1,NBIG
        CALL CL2_GET_BIG_CELL(I,ET_BIG,IETA,IPHI,LYR,IN_CAEP,IN_PNUT)
        IF (IETA.NE.0) THEN
          BAD_CELL = CL2_CELL_ISOLATED(IETA,IPHI,LYR)
          IF (BAD_CELL) THEN  ! Kill it if it hasn't been done already
            NBAD = NBAD + 1
            IF (CAEP_REMOVE.AND.IN_CAEP) THEN
              PTR2(LYR,IPHI,IETA) = 0
              IN_CAEP = .FALSE.
            ENDIF
            IF (PNUT_REMOVE.AND.IN_PNUT) THEN
              IF (L2PNUT.GT.0) THEN   !this protects against calls from jets, em
                EX = -Q(L2PNUT+3)
                EY = -Q(L2PNUT+4)
                ICOARSE = 1
                IF (IABS(IETA).GT.32) ICOARSE = 2
                EX = EX -ET_BIG*ET_CORR(IETA)*CS(IPHI,ICOARSE)
                EY = EY -ET_BIG*ET_CORR(IETA)*SN(IPHI,ICOARSE)
                ET = Q(L2PNUT+14) - ET_BIG*ET_CORR(IETA)
                Q(L2PNUT+3) = -EX               ! missing Ex
                Q(L2PNUT+4) = -EY               ! missing Ey
                Q(L2PNUT+7) = SQRT(EX**2 + EY**2 + .000001)     ! Missing ET
                Q(L2PNUT+14) = ET               ! sum ET
                Q(L2PNUT+10) = ATAN2(-EY,-EX+.0000001)  ! PHI of missing ET
                IF(Q(L2PNUT+10).LT.0)Q(L2PNUT+10)= Q(L2PNUT+10)+TWOPI
                Q(L2PNUT+5) = 0                 ! missing Ez
                Q(L2PNUT+6) = ET                ! sum E
                IN_PNUT = .FALSE.
              ENDIF
            ENDIF
C
C...record new status
            CALL CL2_SET_BIG_CELL(I,ET_BIG,IETA,IPHI,LYR,IN_CAEP,
     &        IN_PNUT)
          ENDIF
        ENDIF
      ENDDO
      RETURN
      ENTRY CL2_BAD_CELL(IBAD_CELL)
      IBAD_CELL=NBAD
      RETURN
      END
