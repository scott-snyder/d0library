      SUBROUTINE COVEP_JETFIX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : this is used by the JETFIX package.  the
C-   *_INI routine initializes, the *_loop routine calculates, and
C-   the *_FIN routine finishes up
C-
C-   Inputs  :  LJETS = pointer to the JETS bank
C               POINT = pointer to the CAEH hit
C-   Outputs :  RESULT = Et-weighted eta-phi covariance of the JETS cells
C-   Controls:  COVEP_CELL_ETMIN = min. Et threshold for a cell
C-
C-   Created  11-JUL-1995   Drew Baden
C-   Updated  12-OCT-1995   Qizhong Li-Demarteau, Dhiman Chakraborty
C-                          *_INI,*_LOOP and *_FIN brought in as entries,
C-                          some streamlining done
C----------------------------------------------------------------------
C     note:  in the JETS bank as of 7/95, the 12th and 13th
C            elements are the ETA and PHI widths respectively.
C            these are calculated by CONCLU, and are basically
C            the RMS eta and phi relative to the jet axis.  note
C            that they are RMS quantities (i.e. sqrt(...))
C            in this routine, i return "result" which is NOT the
C            rms quantity, but is the variance in eta-phi.
C            specifically, it is:
C
C            sum_towers (eta-<eta>)*(phi-<phi>)*et
C            -------------------------------------
C                   sum_towers et
C
C            when you use this quantity to calculate the 2-d
C            shape of the jet, you will have to use the SQUARE
C            of the 12th and 13th element in the JETS bank.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  LJETS,POINT
      REAL     RESULT
      REAL     PROXIM
      EXTERNAL PROXIM
C
      REAL     PHIJ,ETAPHI,ETSUM,ET,ETA,PHI,THETA,E(4)
      REAL     ETASUM,PHISUM,COVEP_CELL_ETMIN,XEE,XEP
      INTEGER  KJETS
C
      SAVE     ETAPHI,ETSUM,PHIJ,KJETS,ETASUM,PHISUM
      COMMON /COVEP/COVEP_CELL_ETMIN
C-------------------------------------------------------------------------
      ENTRY COVEP_JETFIX_INI(LJETS)
C
      IF (LJETS.LE.0) THEN
        CALL ERRMSG('CAL_FIX','COVEP_JETFIX_INI',
     &    'Incvalid LJETS','F')
        RETURN
      ENDIF
C
C       initialize
C
      ETAPHI = 0.
      ETSUM = 0.
      ETASUM = 0.
      PHISUM = 0.
      PHIJ = Q(LJETS+8)
      KJETS = LJETS
C
      RETURN
C
      ENTRY COVEP_JETFIX_LOOP(LJETS,POINT)
C
C       running calculation
C
      IF (LJETS.NE.KJETS) THEN
        CALL ERRMSG('CAL_FIX','COVEP_JETFIX_LOOP',
     &    'Change of JETS pointer in midstream','F')
        RETURN
      ENDIF
      E(1) = Q(POINT+4)
      E(2) = Q(POINT+5)
      E(3) = Q(POINT+6)
      E(4) = Q(POINT+7)
      ET = Q(POINT+8)
      CALL ETOETA(E,PHI,THETA,ETA)
      PHI = PROXIM(PHI,PHIJ)  ! take care of wraparound!
      IF (ET.GT.COVEP_CELL_ETMIN) THEN
        ETSUM = ETSUM + ET
        XEE = ET*ETA
        ETASUM = ETASUM + XEE
        XEP = ET*PHI
        PHISUM = PHISUM + XEP
        ETAPHI = ETAPHI + ETA*XEP
      ENDIF
      RETURN
C
      ENTRY COVEP_JETFIX_FIN(LJETS,RESULT)
C
C       all done
C
      IF (LJETS.NE.KJETS) THEN
        CALL ERRMSG('CAL_FIX','COVEP_JETFIX_FIN',
     &    'Change of JETS pointer in midstream','F')
        RETURN
      ENDIF
      IF (ETSUM.LT.0.001) THEN
        CALL ERRMSG('JETFIX','COVEP_JETFIX_DONE',
     &    'Jet has ETSUM=0 causing divide by 0','E')
        RESULT = 999.
        RETURN
      ENDIF
      ETASUM = ETASUM/ETSUM
      PHISUM = PHISUM/ETSUM
      RESULT = ETAPHI/ETSUM - ETASUM*PHISUM
C
      RETURN
C
      END
