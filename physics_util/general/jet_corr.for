      SUBROUTINE JET_CORR(LJETS, ISYS, EOUT, ETOUT, ETAOUT, PHIOUT,
     &  PXOUT, PYOUT, PZOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct an arbitrary jet (Monte Carlo or Data)
C-                         of any algorithm by calling JET_CONE_CORR or
C-                         MC_ET_CORR.  All corrections are turned on.
C-
C-   Inputs  : LJETS  : Pointer to jets bank
C-             ISYS   : 0- Nominal correction, 1-Low correction, 2-High
C-                      Used to calculate errors
C-
C-   Outputs : EOUT   : corrected energy
C-             ETOUT  : corrected Et
C-             ETAOUT : corrected Eta
C-             PHIOUT : corrected phi
C-             PXOUT  : corrected PX
C-             PYOUT  : corrected PY
C-             PZOUT  : corrected PZ
C-
C-   Created   1-Jan-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
C- Arguments
      INTEGER LJETS, ISYS
      REAL EOUT, ETOUT, ETAOUT, PHIOUT, PXOUT, PYOUT, PZOUT
C- Static data
      LOGICAL DO_ZSP, DO_UND, DO_OOC
      REAL CAL_ESCALE(3)
C- Event temporary variables
      LOGICAL MCDATA, MATCH
      INTEGER JET_ALG
      INTEGER LVERH, LVERT
      REAL E5VECIN(5),  ETAIN,  PHIIN, ZVERT, EMFRAC
      REAL E5VECOUT(5), DE(5), OUTVEC(3)
C- Functions
      INTEGER GET_CAPH_ALG, GZVERH
C- Initialization variables
      INTEGER IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA DO_ZSP, DO_UND, DO_OOC/3*.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
C-
C- Read EM scale parameters from CORRECTEM_RCP
C-
        CALL EZPICK_NOMSG('CORRECTEM_RCP',IER)
        IF(IER.NE.0)THEN
          CALL INRCP('CORRECTEM_RCP', IER)
          CALL EZPICK_NOMSG('CORRECTEM_RCP', IER)
        ENDIF
        IF(IER.EQ.0)CALL EZGET('ECEMN_BOOST', CAL_ESCALE(1), IER)
        IF(IER.EQ.0)CALL EZGET('CCEM_BOOST', CAL_ESCALE(2), IER)
        IF(IER.EQ.0)CALL EZGET('ECEMS_BOOST', CAL_ESCALE(3), IER)
        IF (IER.NE.0)CALL ERRMSG(
     &    'Error getting correctem rcp parameters',
     &    'EZ_CORRECT_MISS_ET',' ','F')
        CALL EZRSET
      ENDIF
C-
C- Get uncorrected jet parameters.
C-
      E5VECIN(1) = Q(LJETS+2)    ! PX
      E5VECIN(2) = Q(LJETS+3)    ! PY
      E5VECIN(3) = Q(LJETS+4)    ! PZ
      E5VECIN(4) = Q(LJETS+5)    ! E
      E5VECIN(5) = Q(LJETS+6)    ! ET
      ETAIN      = Q(LJETS+9)
      PHIIN      = Q(LJETS+8)
      EMFRAC     = Q(LJETS+14)
      JET_ALG    = GET_CAPH_ALG(LJETS)
C-
C- Is this Monte Carlo?
C-
      MCDATA = IQ(LHEAD+1).GT.1000
      IF(MCDATA)THEN
C-
C- Do MC correction
C-
        CALL MC_ET_CORR(E5VECIN, ETAIN, PHIIN, EMFRAC, JET_ALG,
     &    E5VECOUT, ETAOUT, PHIOUT, DE, MATCH)
        EOUT  = E5VECOUT(4)
        ETOUT = E5VECOUT(5)
        PXOUT = E5VECOUT(1)
        PYOUT = E5VECOUT(2)
        PZOUT = E5VECOUT(3)
      ELSE
C-
C- Do data correction
C-
        ZVERT=0.
        LVERH=GZVERH()
        IF(LVERH.GT.0)THEN
          LVERT=LQ(LVERH-IZVERT)
          IF(LVERT.GT.0)THEN
            ZVERT=Q(LVERT+5)
          ENDIF
        ENDIF
        CALL JET_CONE_CORR(LJETS, DO_ZSP, DO_UND, DO_OOC, CAL_ESCALE,
     &    ZVERT, ISYS, EOUT, ETOUT, ETAOUT, PHIOUT, OUTVEC, MATCH)
        PXOUT = OUTVEC(1)
        PYOUT = OUTVEC(2)
        PZOUT = OUTVEC(3)
      ENDIF
  999 RETURN
      END
