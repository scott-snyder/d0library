      SUBROUTINE ELROAD(NEL,PHIMIN,PHIMAX,THEMIN,THEMAX,PT) 
C-----------------------------------------------------------------------
C                                                                        
C  Find electron roads for extrapolation to central detector
C  (for now use Isajet leptons)
C
C  Output:
C         NEL           = number of electron candidates 
C
C         limits for three-sigma roads:
C         PHIMIN(1:NEL) 
C         PHIMAX(1:NEL)
C         THEMIN(1:NEL)
C         THEMAX(1:NEL) 
C
C         PT(1:NEL)     = transverse momenta of electron candidates
C
C  Daria Zieminska  Jan., 1989     
C
C------------------------------------------------------------------------
      IMPLICIT NONE          
      INTEGER NMAX
      PARAMETER (NMAX=100)
      INTEGER NLEP,ID(NMAX),ILEP,NEL,IEL 
      REAL PX(NMAX),PY(NMAX),PZ(NMAX),PLEP(NMAX),MLEP(NMAX)
      REAL PHILEP(NMAX),THELEP(NMAX),ETALEP(NMAX),PT(NMAX)
      REAL PHIMIN(NMAX),PHIMAX(NMAX),THEMIN(NMAX),THEMAX(NMAX)
      REAL LENGTH,SIGPHI,SIGTHE
C
C  Find electron tracks (for now: take Isajet electrons)
C
      CALL GTISAL_ALL(NLEP,ID,PX,PY,PZ,PLEP,MLEP,PHILEP,THELEP,ETALEP)
      IF (NLEP.EQ.0) GO TO 1000
      IEL=0
      DO 100 ILEP=1,NLEP
        IF (IABS(ID(ILEP)).EQ.12) THEN       
          IEL=IEL+1
          PT(IEL)=SQRT(PX(ILEP)**2+PY(ILEP)**2) 
          LENGTH=100. ! temporary (depends on phi,theta)
          SIGPHI=0.0141*SQRT(LENGTH)/PT(IEL)
          PHIMIN(IEL)=PHILEP(ILEP)-3*SIGPHI
          PHIMAX(IEL)=PHILEP(ILEP)+3*SIGPHI
          SIGTHE=0.05 
          THEMIN(IEL)=THELEP(ILEP)-3*SIGTHE
          THEMAX(IEL)=THELEP(ILEP)+3*SIGTHE
        END IF
  100 CONTINUE
      NEL=IEL
 1000 RETURN
      END       
