      FUNCTION DILBOSON()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reconstruct decays X  ->l+l-;
C-   now works only for J/psi -> mumu 
C-
C-   Inputs  : PMUO, PELC  or PTAU banks
C-   Outputs :
C-   Controls:
C-
C-   Created   8-NOV-1991   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ICALL,IER,LEPTONS
      LOGICAL DILBOSON,DIMUON,DIELEC,DITAU,OK
      SAVE ICALL
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('DILBOSON_RCP')
        CALL EZGET('LEPTONS',LEPTONS,IER)
        CALL EZRSET
        ICALL=1
      END IF
      DILBOSON=.FALSE.
      OK=.FALSE.
      IF (LEPTONS.EQ.0) THEN
        OK=DIELEC()
        OK=OK.OR.DIMUON()
C        OK=OK.OR.DITAU()
      ELSE IF (LEPTONS.EQ.12) THEN
        OK=DIELEC()
      ELSE IF (LEPTONS.EQ.14) THEN
        OK=DIMUON()
C      ELSE IF (LEPTONS.EQ.16) THEN
C        OK=DITAU()
      END IF
      DILBOSON=OK
 1000 RETURN
      END
