      SUBROUTINE ECEMCR_OUTER_RADIUS(E,ZV1,IETA3,X_EM3,Y_EM3,Z_EM3_MOD,
     &  DE, ERR_DE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-SEP-1992   Anthony L. Spadafora
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST /.TRUE./
      INTEGER IETA3,IER

      REAL    X_EM3,Y_EM3,Z_EM3_MOD
      REAL    E,ZV1
      REAL    DE, ERR_DE
      REAL    R0,RMAX,DEDR,ERR_FRAC_IETA14,ERR_FRAC
      REAL    R_EM3,X
C----------------------------------------------------------------------
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('ECEMCR_RCP')
        CALL EZGET(' ECEM_OUT_RAD_R0 ', R0, IER)
        CALL EZGET(' ECEM_OUT_RAD_RMAX ', RMAX, IER)
        CALL EZGET(' ECEM_OUT_RAD_DEDR ', DEDR , IER)
        CALL EZGET(' ECEM_OUT_RAD_ERR_FRAC ', ERR_FRAC , IER)
        CALL EZGET(' ECEM_OUT_RAD_ERR_FRAC_IETA14 ',
     &    ERR_FRAC_IETA14, IER)
        CALL EZRSET
      ENDIF   !first

      DE = 0.
      ERR_DE = 0.
      IER = 0
C
      IF(IETA3.EQ.14) THEN
        ERR_DE = E*ERR_FRAC_IETA14
        IER = 1
      ELSE
        R_EM3 = SQRT(X_EM3**2 + Y_EM3**2)
        IF(R_EM3.LE.R0) THEN
          IER = 0
          RETURN        ! no correction
        ELSEIF(R_EM3.GT.RMAX) THEN
          IER = -2
          RETURN        ! out of range??
        ELSE
          X = DEDR*(R_EM3-R0)
          DE = E*(-X)/(1.+X)
          ERR_DE = ERR_FRAC*DE
        ENDIF
      ENDIF   !IETA 14
  999 RETURN
      END
