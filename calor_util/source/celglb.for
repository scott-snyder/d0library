      SUBROUTINE CELGLB(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : call the global fit package for electrons
C-
C-   Inputs  : LPELC : pointer to electron bank
C-   Outputs : None
C-   Controls: ELFIT.RCP
C-
C-   Created  28-FEB-1995   Meenakshi Narain
C-                          this routine is basically a copy of
C-                          ELBGLB.FOR by Daria Zieminska
C-                          BUT tailored to be called from CAPHEL package.
C-   Updated   6-JUN-1995   Meenakshi Narain  Add error checking 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER LPELC, LHMTE
      INTEGER IERR, LRCP, NDOF, IER
      REAL POINT(3),EPOINT(3),CLIST(100)
      REAL CHISQ, THE, PHI
      LOGICAL FIRST,OK
      SAVE FIRST
      DATA FIRST / .TRUE. /

C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC('ELFIT_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL INRCP('ELFIT_RCP',IER)
          IF (IER.EQ.0) CALL EZPICK('ELFIT_RCP')
          IF (IER.EQ.0) CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CELGLB','CELGLB',
     &        ' ELFIT_RCP not found','F')
          ENDIF
          CALL EZRSET
        ENDIF
      ENDIF
C
C ****  global fitting routine for electrons
C
      IERR=0
      CALL CLUCEN(LPELC,POINT,EPOINT,IERR)  ! get cluster position and errors
      IF(IERR.GT.0) GOTO 800
C
      IF (ABS(POINT(3)).LT.150.) THEN
        CALL ELFITCEN(LPELC,POINT,EPOINT,CLIST,IERR)  ! central region
      ELSE
        CALL ELFITFWD(LPELC,POINT,EPOINT,CLIST,IERR)  ! forward region
      END IF
C ****  save chisq, theta  and phi retuned by global fit in the HMTE bank
C
  800 CONTINUE
      LHMTE = LQ(LPELC-1)
      IF (LHMTE.GT.0) THEN
        IF (IERR.NE.0) THEN
          THE   = 0.
          PHI   = 0.
          NDOF  = -1
          CHISQ = -1.
        ELSE
          THE   = CLIST(65)
          PHI   = CLIST(66)
          NDOF  = CLIST(2)
          IF (CLIST(2).GT.0) THEN
            CHISQ = CLIST(3)/CLIST(2)
          ENDIF
        ENDIF
        Q(LHMTE+21) = CHISQ
        Q(LHMTE+22) = THE
        Q(LHMTE+23) = PHI
        IQ(LHMTE+24) = NDOF
      ENDIF

  999 RETURN
      END
