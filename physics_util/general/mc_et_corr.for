      SUBROUTINE MC_ET_CORR(OLDE,OLDETA,OLDPHI,EMF,ICHOICE,
     &     NEWE,NEWETA,NEWPHI,DE,FOUND_MATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Correct MC jets matching jets to 0.7 jets
C-      If no match use 0.7 cone correction
C-
C-   Inputs  : OLDE      : Measured, uncorrected jet E 5-vec
C-             OLDETA    : Measured physics eta of jet.
C-             OLDPHI    : Measured phi of jet
C-             ICHOICE   : 1 cone 0.7, others treated equally
C-
C-   Outputs : NEWE   : corrected Jet E 5-vec
C-             NEWETA : New eta
C-             NEWPHI : New phi
C-             DE: correction to missing Et for this jet
C-       (value of px, py to be subtracted from missing px,py, not factor)
C-             FOUND_MATCH : If a matching 0.7 cone jet was found
C-              If FOUND_MATCH, then eta and phi correspond to that jet.
C-
C-   Created   3-MAY-1993   Serban D. Protopopescu
C-   Updated  23-JUL-1993   Dhiman Chakraborty
C-                          Now returns eta, phi of the matching 0.7
C-                          jet if such is found (i.e. if FOUND_MATCH)
C-                 ******* Note: I/O list has changed *******
C-   Updated   7-OCT-1993   Dhiman Chakraborty
C-                          Switched to FCN_CORR_MC a la Meenakshi Narain
C-   Updated  29-OCT-1993   Dhiman Chakraborty
C-                          No FCN_CORR_MC for jets with EMF>0.9
C-                          Look for match in circular cone instead of square
C-                 ******* Note: I/O list has changed *******
C-   Updated  21-FEB-1994   Dhiman Chakraborty   
C-                          Bug fixed for em jet overcorrection
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICHOICE
      INTEGER GZJETS,LJETS,IER
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      LOGICAL FIRST,FOUND_MATCH
      REAL OLDE(5),OLDETA,OLDPHI,NEWE(5),NEWETA,NEWPHI,EMF
      REAL    TEMPLATE(3),XOLDE(5),DPHI,DETA,DR,DE(5),CORR
      DATA FIRST /.TRUE./
      DATA TEMPLATE/ 1.,6.,0.7/     ! CONE R=0.7
      REAL FCN_CORR_MC, X
C----------------------------------------------------------------------
      FCN_CORR_MC(X) = (0.07036 + 0.9415/X)
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL ERRMSG('Using MC Jet Et correction',
     &    'MC_ET_CORR',' ','S')
        FIRST = .FALSE.
      END IF
C
      FOUND_MATCH = .FALSE.
      CALL UCOPY(OLDE,XOLDE,5)
      NEWETA = OLDETA
      NEWPHI = OLDPHI
      IF(ICHOICE.NE.1) THEN  ! Find 0.7 cone
        CALL SET_CAPH('CONE_JET',TEMPLATE,IER)
        LJETS=GZJETS()
        DO WHILE ((LJETS.NE.0).AND.(.NOT.FOUND_MATCH))
          DPHI=ABS(OLDPHI-Q(LJETS+8))
          DETA=ABS(OLDETA-Q(LJETS+9))
          IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
          DR = SQRT(DPHI**2 + DETA**2)
          IF (DR.LT.0.15) THEN
            FOUND_MATCH = .TRUE.
            CALL UCOPY(Q(LJETS+2),XOLDE,5)
            NEWETA = Q(LJETS+9)
            NEWPHI  = Q(LJETS+8)
          ENDIF
          LJETS=LQ(LJETS)
        ENDDO
        CALL RESET_CAPH
      ENDIF
      CORR = FCN_CORR_MC(XOLDE(5))
      IF(EMF.GT.0.9)CORR = 0.
      CALL VSCALE(XOLDE,CORR,DE,5)
      CALL VADD(XOLDE,DE,NEWE,5)
C
  999 RETURN
      END
