      FUNCTION ONETRK()
C-------------------------------------------------------
C-
C-     READ ONE TRACK PROVIDED BY USER ON TERMINAL
C-
C-------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL ONETRK
      INCLUDE 'D0$INC:PARTCL.INC'
C
      REAL RADEG,E1,TH1,PHI1,M1,P1
      REAL AMASS
      INTEGER ID1
C
      RADEG=3.14159/180.
      PRINT*,' Give E,theta,phi and ident for one particle'
      IF(NPTCL.LT.1)PRINT*,' theta and phi should be in degrees'
      PRINT*,' to end, set E<0 and terminate with /.'
      ONETRK=.TRUE.
    1 READ*,E1,TH1,PHI1,ID1
      IF(E1.LT.0) ONETRK=.FALSE.    ! E1<0 terminates reading
C
      IF(ONETRK) THEN
        M1=AMASS(ID1)
        P1=E1**2-M1**2
        IF(P1.GT.0) THEN     ! check particle mass<particle energy
          P1=SQRT(P1)
          TH1=TH1*RADEG
          PHI1=PHI1*RADEG
          NPTCL=NPTCL+1
          PPTCL(1,NPTCL)=P1*SIN(TH1)*COS(PHI1)
          PPTCL(2,NPTCL)=P1*SIN(TH1)*SIN(PHI1)
          PPTCL(3,NPTCL)=P1*COS(TH1)
          PPTCL(4,NPTCL)=E1
          PPTCL(5,NPTCL)=M1
          IDCAY(NPTCL)=0
          IORIG(NPTCL)=0
          IDENT(NPTCL)=ID1
        ELSE
          PRINT*,' Particle energy is less than its mass. Try again.'
          GOTO 1
        ENDIF
C
      ENDIF
      RETURN
      END
