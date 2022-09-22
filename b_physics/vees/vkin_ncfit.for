      SUBROUTINE VKIN_NCFIT( PARTICLE,IEND,NSTEP,PRB,CHIR )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The  loop  of  sequential  fitting  procedure
C-                         is in here. It begins from label 1.
C-
C-   Inputs  : PARTICLE = 1 Vee
C-                        2 dilepton
C-             NSTEPM - maximal number of steps iteration,
C-             IPRINT = 0 - no any print of iteration values,
C-                      1 - short output,
C-                      2 - full output,
C-                      3 - full output and output from routine BOECK,
C-             IMT    = 1 - fit for decay,
C-                      2 - fit for interaction at target in rest,
C-                      3 - fit for interaction at target in motion.
C-
C-   Outputs : IEND  >  0   fit is successful
C-             IEND  =  2   if VF < 1.e-5 and abs(DCHI) <  0.001
C-             IEND  =  1   if VF < 1.e-5
C-             IEND  <  0   fit is failed
C-             IEND  = -1   if VF > 1.e+5
C-             IEND  = -3   if VF > 1.e+5 and DCHI < 0.
C-             IEND  = -4   if NSTEP > max. number of steps NSTEPM
C-             IEND  = -5   if VF > 1.e+5 at first 3 or 4 steps
C-             IEND  = -6   when overflow
C-             IEND  = -7  if IMT out off order
C-             IEND  = -8  if number of deg. of freedom less then zero
C-             IEND  = -9  if number of tracks great then 10
C-             IEND  = -10 if the covariant matrix doesn't have inverse
C-             NSTEP    -   number of steps iteration,
C-             PRB      -   fit probability,
C-             CHI      -   fit chi-squared.
C-   Controls: 
C-
C-   Created original       V.A.Gapienko
C-   Updated                V.I.Sirotenko
C-   Updated  24-jul-1991   V. Burtovoy
C-   Updated   1-OCT-1993   Oscar Ramírez  If not cov. matrix inv. IEND=-10
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL*8 X(4),ETA(50),Y(50),CY(1500),A(20),B(200),XM(15)
      REAL*8 GMFI(1500),GXI(20),F(4),C(50),DX(50),GI(1500)
      REAL*8 ETA0(50),X0(4),VF,VF0,CHI0,CHI,DZERO
      REAL*8 DCHI,DVMOD
      REAL MP,PRB,CHIR,RRDX,SNGL,CHI5,VF5,PROB,XMIN,XMAX
      LOGICAL FIRST 
      INTEGER IEND,NSTEP,NSTEPM,IPRINT,IER,IMT,NM,NX,NR,NF,K64,J
      INTEGER JRRDX,JBYT,II,I,K,KE,PRUNIT,USUNIT,PARTICLE
      SAVE FIRST,PRUNIT
      DATA FIRST/.TRUE./
      DATA MP / 0.938 /, DZERO / 0.D00 /, NF / 4 /
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VEES_RCP')
        CALL EZGET_i(  'NSTEPM',  NSTEPM,IER)
        CALL EZGET_i(  'IPRINT',  IPRINT,IER)
        CALL EZGET_i(     'IMT',     IMT,IER)
        CALL EZGET(    'XMIN',    XMIN,IER)
        CALL EZGET(    'XMAX',    XMAX,IER)
        CALL EZRSET
        PRUNIT=USUNIT()
      ENDIF
      IF(IMT .EQ. 1)                   XMT = 0.
      IF(IMT .GE. 2 .AND. XMT .EQ. 0.) XMT = MP
      NSTEP = 0
      IEND  = 0
      CALL VKIN_VAL(IMT,IEND,X0,ETA0,CY,NM,NX)
      IF(IEND .LT. 0) GO TO 999 
      VF0  = XMAX
      CHI0 = XMAX
      CALL DVCPY(NM*NM,CY(1),CY(2),GI(1),GI(2))
      IF(NX .GT. 0) CALL DVCPY(NX,X0(1),X0(2),X(1),X(2))
      CALL DVCPY(NM,ETA0(1),ETA0(2),ETA(1),ETA(2))
      IF(IPRINT .GE. 1) CALL MXWRT('GI-ERR  ',GI,NM,NM)
      NR = NF - NX
      CALL DVSET(NM,DZERO,C(1),C(2))
      CALL VKIN_DERIV(IMT,X,ETA,F,A,B,NM,NX,NF)
      IF(IPRINT .GE. 1) CALL MXWRT('CONSTR. ',F,1,4)
    1 NSTEP = NSTEP + 1
      IF(IPRINT .GE. 1) WRITE(PRUNIT,2005) NSTEP
      IF(IPRINT .GE. 2) THEN
        CALL MXWRT('X-DERIV ',A,NF,NX)
        CALL MXWRT('ETA-DRV ',B,NF,NM)
      END IF
      CALL VKIN_BOECK(0,NF,NM,NX,F,B,A,GI,C,DX,GMFI,GXI,CHI,IPRINT)
      IF(CHI.EQ.-1.0) THEN
        IEND = -10    
        GO TO 900
      END IF
      IF(IPRINT .GE. 2) THEN
        CALL MXWRT('DX      ',DX,NX,1)
        CALL MXWRT('DETA    ',C,NM,1)
      END IF
C--
      IF(NX .LE. 0)   GO TO 3
      K64 = 0
      DO 2 J=1,NX
        RRDX  = SNGL(DX(J))
        JRRDX = JBYT(RRDX,8,8)
        IF(JRRDX .GT. 255) K64 = K64+1
    2 CONTINUE
      IF(K64 .EQ. 0)   GO TO 3
      IEND = -6
      GO TO 4
    3 CONTINUE
C--
      IF(NX .GT. 0) CALL DVADD(NX,X(1),X(2),DX(1),DX(2),X(1),X(2))      
      CALL DVADD(NM,ETA0(1),ETA0(2),C(1),C(2),ETA(1),ETA(2))
      CALL VKIN_DERIV(IMT,X,ETA,F,A,B,NM,NX,NF)
      IF(IPRINT .GE. 1) CALL MXWRT('CONSTRNT',F,1,4)
      VF = DVMOD(F,4)
      IF(IPRINT.GE.1) THEN
        CALL MXWRT('X-NEW   ',X,NX,1)
        CALL MXWRT('ETA-NEW ',ETA,NM,1)
        CHI5 = SNGL(CHI)
        VF5  = SNGL(VF)
        WRITE(PRUNIT,2007) CHI5,VF5
 2007   FORMAT(30X,'      CHI2=',E10.4,' MODUL OF CONSTR.=',E10.4)
      END IF
C--
      II = NR + 1
      DCHI = 1.D20
      IF(CHI0 .NE. 0.D0) DCHI = (CHI0-CHI)/CHI0
      IF(NSTEP .LT. 3)           GO TO 8
      GO TO (10,11,12,13,14),II
   10 IF(VF   .GT.  VF0) IEND = -1
      IF(VF   .LT. XMIN) IEND =  1
      GO TO 8
   11 IF(VF   .GT.  VF0) IEND = -1
      IF(VF   .LT. XMIN) IEND =  1
      IF(VF   .LT. XMIN .AND. DABS(DCHI) .LT. 0.001) IEND = 2
      GO TO 8
   12 IF(VF   .GT.  VF0) IEND = -1
      IF(DCHI .LT. DZERO .AND. VF .GT. VF0) IEND = -3
      IF(VF   .LT. XMIN) IEND =  1
      IF(VF   .LT. XMIN .AND. DABS(DCHI) .LT. 0.001) IEND = 2
      GO TO 8
   13 CONTINUE
   14 IF(NSTEP.LT.    5) GO TO   8
      IF(VF   .GT.  VF0) IEND = -3
      IF(VF   .LT. XMIN) IEND =  1
      IF(VF   .LT. XMIN .AND. DABS(DCHI) .LT. 0.001) IEND = 2
    8 IF(NSTEP.GE.NSTEPM)IEND = -4
      IF(VF   .GT. XMAX) IEND = -5
      CHI0 = CHI
      VF0  = VF
      IF(IEND .EQ. 0) GO TO 1
c
      CALL VKIN_BOECK(1,NF,NM,NX,F,B,A,GI,C,DX,GMFI,GXI,CHI,IPRINT)
      IF(CHI.EQ.-1.0) THEN
        IEND= -10    
        GO TO 900
      END IF
      IF(IPRINT .GE. 1) THEN
        CALL MXWRT('ETA-ERR ',GMFI,NM,NM)
        CALL MXWRT('X-ERR   ',GXI,NX,NX)
      END IF
      CHIR = SNGL(CHI)
      PRB  = PROB(CHIR,NF-NX)
      IF(NR .EQ. 0 .AND. CHI .LT. XMIN) PRB = 1.0
      IF(IPRINT .GE. 1) CALL MXWRT('CONSTRNT',F,1,4)
C
C  Don't update STR, ETR if IEND<0
C
      IF (IEND.LT.0) GO TO 900
    4 DO 100 I   = 1, NTR
        DO 100 J = 1, 4
          K    = ITR(J,I)
          IF(K)  30,100,40
   30     STR(J,I) = SNGL(ETA(-K))
          KE       =-(NM*(K+1)+K)
          ETR(J,I) = SNGL(DSQRT(DABS(GMFI(KE))))
          GO TO 100
   40     STR(J,I) = SNGL(X(K))
          KE       = NX*(K-1) + K
          ETR(J,I) = SNGL(DSQRT(DABS(GXI(KE))))
  100 CONTINUE
  900 CONTINUE
      IF (IMT.EQ.1) THEN
        IF (PARTICLE.EQ.1) THEN
          CALL PVESFL(NX,NM,GXI,GMFI,IEND,CHIR,PRB,NR,NSTEP)
        ELSE IF (PARTICLE.EQ.2) THEN
          CALL PDILFL(NX,NM,GXI,GMFI,IEND,CHIR,PRB,NR,NSTEP)
        END IF
      END IF
 2005 FORMAT(/30X,'================(NSTEP=',I3,')===========')
  999 RETURN
      END
