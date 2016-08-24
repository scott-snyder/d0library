      FUNCTION VEES()
C------------------------------------------------------------------
C 
C  Event processing routine for the package VEES.
C  Find all vee's (K0 or Lambda decays) in an event; 
C  Store them in banks VERT and PVES.
C 
C  Daria Zieminska 6-JUL-1990
C-   Updated  25-APR-1992   Daria Zieminska  add ALL_VEES 
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      LOGICAL VEES,MCDATA 
      INTEGER NTRACK,IZTRK1,IZTRK2,NPRIM,PRUNIT,USUNIT
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER ICALL,IER,ICONT(10)
      REAL ZPRIM(5),DZPRIM(5),Z0,PHI0,DPHI0,THE0,DTHE0
      LOGICAL ALL_VEES
      SAVE ICALL,ALL_VEES
      DATA ICALL/0/
      DATA PHI0,DPHI0,THE0,DTHE0/0.,3.1416,1.5716,1.4/
      IF (ICALL.EQ.0) THEN
        PRUNIT=USUNIT()
        CALL EZPICK('VEES_RCP')
        CALL EZGET('MCDATA',MCDATA,IER)
        CALL EZGET('ALL_VEES',ALL_VEES,IER)
        CALL EZRSET
        ICALL=1
      END IF
      IF (ALL_VEES.EQV..FALSE.) GO TO 1000
      CALL DHDIR('VEES_RCP','HBOOK_DIRECTORY',IER,' ')
      IF (IER.NE.0) THEN
        CALL ERRMSG('VEES','VEES',
     &  ' ERROR SETTING HBOOK DIRECTORY ','W')
      END IF
      CALL ZVERTE(NPRIM,ZPRIM,DZPRIM)
      CALL GTZTRH(ICONT)
      NTRACK=ICONT(2)
      IF (NPRIM.GT.0) THEN
        Z0=ZPRIM(1)
      ELSE 
        GO TO 1000     ! z of primary vertex unknown; skip the event
      END IF
      VEES=.TRUE.
      IF (NTRACK.LT.2) GO TO 1000
      CALL VEES_CONE(PHI0,DPHI0,THE0,DTHE0)
      CALL VEEHIS
 1000 CONTINUE 
      RETURN
      END       
