      SUBROUTINE MUOT_CAL_MATCH1(LMUOT,TOT_E,HAD_E,TOT_E_OPP,HAD_E_OPP,
     &  IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate energy deposition in calorimeter
C-                         by extrapolating the line segment made by 
C-                         the MUOT BC line segment.  Also return energy
C-                         deposition on opposite side of calorimeter by
C-                         extending same line for cosmic ray rejection
C-
C-   Inputs  : LMUOT address of LMUOT bank for a given muon track
C-   Outputs : TOT_E(4) -Real- Total energy left in calorimeter by muon 
C-                      in: (1) - Cells supposedly hit by muon
C-                          (2) - Hit cells and nearest meighbors
C-                          (3) - Hit cells and 2 nearest neighbors
C-                          (4) - Hit cells and three nearest neighbors
C-             HAD_E(4) -Real- Same as Tot_E except only hadronic energy
C-             TOT_E_OPP(4)  - Same as Tot_E except it is for the 
C-                             oppositeside of the calorimeter
C-             HAD_E_OPP(4)  - Same as HAD_E except is is calculated for
C-                             opposite dside of calorimeter
C-             IER -Integer- = 0 when everything is fine with the data
C-                            >1 if a problem was encountered.
C-                            Ignore results if IER > 1
C-   Controls: None
C-
C-   Created  22-NOV-1992   joey thompson
C-            22-DEC-1992   S. Kunori    Change the name to 
C-              MUOT_CAL_MATCH1 from MUOT_CAL_MATCH in order to move
C-              this routine from MUON_UTIL to MUON_RECO library.
C-              The rouitne in MUON_UTIL will be removed in future.
C-   Updated  20-JUL-1993   K. Wyatt Merritt  Abort routine if A-stub track 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL TOT_E(4),HAD_E(4),TOT_E_OPP(4),HAD_E_OPP(4)
      INTEGER LMUOT,IER
C
      INTEGER I,K,IFW1
      REAL MU_TRK_POINT(3),DIRC(3),PI
      REAL MU_THETA,MU_PHI,MU_ETA,MU_ETA_NEG,MU_PHI_180
C
      REAL X(6),V(3),IMPACT_PARAMETER,MU_TRK_PT(3)
      REAL ZMAX,RMAX
C     
      REAL EM_E(4),EM_E_OPP(4),ET
      INTEGER ERR
      INTEGER NC
      DATA PI/3.14159/
      DATA V/0.,0.,0./
      DATA ZMAX/100./
      DATA RMAX/72./
C----------------------------------------------------------------------
C
C ****  Set up variables in default state of -1.0 for failures
C
      IER = 0
      DO I = 1,4
        TOT_E(I) = -1.0
        HAD_E(I) = -1.0
        TOT_E_OPP(I) = -1.0
        HAD_E_OPP(I) = -1.0
        EM_E(I) = -1.0
        EM_E_OPP(I) = -1.0
      ENDDO
      IFW1 = IQ(LMUOT + 4)
c      IF (IFW1 .EQ. 5) GO TO 999
C
C ****  Get info from MUOT BC Layer track
C
      IFW1 = IQ(LMUOT + 4)
      IF (IFW1 .NE. 5) then  !not Tom Diehl Astub
        MU_TRK_POINT(1) = Q(LMUOT + 11)   !Point is INSIDE magnet volume
        MU_TRK_POINT(2) = Q(LMUOT + 12)
        MU_TRK_POINT(3) = Q(LMUOT + 13)
        DIRC(1) = Q(LMUOT + 17)
        DIRC(2) = Q(LMUOT + 18)
        DIRC(3) = Q(LMUOT + 19)
      else                  !Tom Diehl Astub
        MU_TRK_POINT(1) = Q(LMUOT + 8)   !Point is INSIDE magnet volume
        MU_TRK_POINT(2) = Q(LMUOT + 9)
        MU_TRK_POINT(3) = Q(LMUOT + 10)
        DIRC(1) = Q(LMUOT + 14)
        DIRC(2) = Q(LMUOT + 15)
        DIRC(3) = Q(LMUOT + 16)
      endif
        MU_THETA = ACOS(DIRC(3))
        MU_PHI = ATAN2 (DIRC(2),DIRC(1))
        MU_ETA = 0.
        IF(MU_THETA.GT.1.0E-5) MU_ETA=-LOG(TAN(MU_THETA/2.))
        MU_ETA_NEG = -1. * MU_ETA
        MU_PHI_180 = MU_PHI + PI
        DO K = 1,3
          X(K) = MU_TRK_POINT(K)
          X(K+3) = DIRC(K)
        ENDDO
C
C ****  Get impact parameter to V = (0,0,0)
C ****  Verify that point of closest approach is within CD volume
C ****  Abort processing if it isn't
C
        CALL MUIMPP(V,X,MU_TRK_PT,IMPACT_PARAMETER)
        IF (ABS(MU_TRK_PT(3)).GT.ZMAX .AND.
     &    SQRT( (MU_TRK_PT(1))**2 + (MU_TRK_PT(2))**2 ) .GT. RMAX) THEN
          IER = 1
          GO TO 999
        ENDIF
C
C ****  Look at energy in hit cells about MUOT segment
C
        DO I = 1,4
          NC = I-1   !Number of cells around MUOT to sum energy
          CALL CAL_ECELL(MU_TRK_PT,MU_ETA,MU_PHI,NC,
     &      EM_E(I),TOT_E(I),ET,ERR)
          IER = IER + ERR
          HAD_E(I) = TOT_E(I) - EM_E(I)
        ENDDO
C
C ****  Look at energy deposited in opposite side of calorimeter
C ****  Useful for Cosmic Ray rejection...
C
        DO I = 1,4
          NC = I-1   !Number of cells around MUOT to sum energy
          CALL CAL_ECELL(MU_TRK_PT,MU_ETA_NEG,MU_PHI_180,NC,
     &      EM_E_OPP(I),TOT_E_OPP(I),ET,ERR)
          IER = IER + ERR
          HAD_E_OPP(I) = TOT_E_OPP(I) - EM_E_OPP(I)
        ENDDO
  999 RETURN
      END
