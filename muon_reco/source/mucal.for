      SUBROUTINE MUCAL(PM,X,LMFIT,DECALM_EM,DECALM,CDEDX
     &                             ,PMUN,MUISOL,MOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate various effects of the detectors on
C-                          the Muon, and to calculate energy-loss, isolation,
C-                          new momentum ...
C-
C-   Inputs  :  PM      Momemtum of muon measured in Muon chamber.
C-              X(1:3)  Starting point coordinates.(vertex)
C-              X(4:6)  Direction cosines of the particle.
C-               LMFIT  link to MFIT bank
C-
C-   Outputs :  DECALM_EM(10)   Measured energy loss in EM calorimeter
C               DECALM(10)  Meseaured energy loss in Calorimeter for all cones
C-              CDEDX   Combined Error in (DECALM(3) - DECAL)..
C-              PMUN(3) Estimated mementum of Muon before calorimeter.
C-              MUISOL(10)  Values between 0.0--->99.0, if < 1.0 then muon
C-                          most probaboly isolated.
C-                              MUISOL(1:2)     Calculated on basis of cells
C-                              MUISOL(3:10)    Calculated on basis of cones
C-              MOK     If .false. then problem.
C-   Controls:
C-
C-   Created   11-OCT-1989   Shahriar Abachi
C-   Modified  14-JAN-1992   Shahriar Abachi
C-   Modified  14-FEB-1992   Shahriar Abachi
C-   Modified  03-MAR-1992   Shahriar Abachi  CAL_ECEEL call was made
C-   Modified  08-OCT-1992   Shahriar Abachi  Cone of 0.2 replaced by 2 hit
C-                                            cells, others if vertex >25 cm
C-   Modified  03-DEC-1992   Shuichi Kunori   EM reported separately.
C-                                  new input argument DECALM_EM.
C-   Updated   4-JAN-1995   Daria Zieminska  define PMUN for ASTUBS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$INC:MCACEL.INC'
      REAL PM, X(6)
      INTEGER LMFIT,LMUOT,IFW1
      REAL DECALM(10), DECALR, CDEDX, PMUN(3)
      REAL DECALM_EM(10)
      REAL MUISOL(10)
      LOGICAL  MOK, FIRST
C
      REAL DIRCOS(3),FRAC
      REAL DEDXT,EPS,DEDXM,DEDXT0,ET2
      INTEGER I,J 
      REAL ETA, PHI, DR2, ETOTAL(10), EEM, EEM2, ETT, THET
      INTEGER IERR, ISOCEL, IER, NCN, NC
      REAL DEEF, DE, AMASS, PMU(3), EM, PMN, PNTB(3)
      REAL SIGE, PI, XISOCON(10), SIG, SIGZ(10)
      REAL DEDXT0_EM,DEDXT_EM,DEDXM_EM
      DATA EPS,AMASS, PI /1.0E-5, 0.105, 3.14159/
      DATA FIRST /.TRUE./
      DATA NCN,XISOCON /1,10*0.0/
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        ISOCEL = 1
        CALL EZGETA('ISO_CONE_SIZE',0,0,0,NCN,IERR)
        CALL EZGET('ISO_CONE_SIZE',XISOCON,IERR)
      ENDIF
C     -- clear some variables...   
C          ECONE() is in /MUPHYS/ and DECALM, DECALM_EM are input 
C          arguments and others are local.
      DO I=1,10
         ECONE(I)=0.0
         DECALM(I)=0.0
         ECONE_EM(I)=0.0
         DECALM_EM(I)=0.0
      ENDDO    

      DO I=1,NCN
        ETOTAL(I) = 0.0
      ENDDO
      MOK   = .TRUE.
      DO I=1,10
        MUISOL(I) = 99.0
      ENDDO
      DECALR  = DECAL
C
      DO I=1,3
        PNTB(I) =  X(I)
        DIRCOS(I) = X(I+3)
        PMU(I) = PM * DIRCOS(I)
      ENDDO
C
      PHI = ATAN2(X(5), X(4))
      IF(X(5) .LT. 0.0) PHI = PHI + 2.0 * PI
      THET = ACOS(X(6))
      IF(THET .LT. EPS) THET = THET + EPS
      IF(THET .GT. 3.1) THET = THET - EPS
      ETA = -ALOG(TAN(THET/2.0))
C
      NC = 0
      IER = 0
      CALL CAL_ECELL(PNTB,ETA,PHI,NC, EEM,DEDXT0,ET2,IER)
      IF(DEDXT0 .LT. 0.0) THEN
        DEDXT0 = 0.0
C        WRITE(6,*) 'Negative energy from CAEH. Was set to zero'
      ENDIF
C     -- EM energy...
      IF(EEM.LT.0.0) THEN
          DEDXT0_EM=0.0
      ELSE
          DEDXT0_EM=EEM
      ENDIF
C
      NC = 1
      IER = -1
      CALL CAL_ECELL(PNTB,ETA,PHI,NC, EEM,DEDXT,ET2,IER)
      IF(DEDXT .LT. 0.0) THEN
        DEDXT = 0.0
C        WRITE(6,*) 'Negative energy from CAEH. Was set to zero'
      ENDIF
C     -- EM energy...
      IF(EEM.LT.0.0) THEN
         DEDXT_EM=0.0
      ELSE
         DEDXT_EM=EEM
      ENDIF
C
      IF(IQ(LMFIT + 7) .EQ. 3) THEN
        DEEF = DECAL + 0.5 * DEMUO
      ELSE
        DEEF = DECAL + DEMUO
      ENDIF
      EM = SQRT(PM**2 + AMASS**2) + DEEF
      PMN = SQRT(EM**2 - AMASS**2)
      DO I=1,3
        PMUN(I) = PMN * PMU(I) / PM
      ENDDO
C
C  A stubs
C 
      LMUOT = LQ( LMFIT - 1 )
      IFW1  = IQ( LMUOT + 4)
      IF (IFW1 .EQ. 5) THEN
        FRAC=0.5    ! to be optimized
        PMN=DECAL+FRAC*DEMUO
        DO I=1,3
          PMUN(I) = PMN * DIRCOS(I) 
        ENDDO
      END IF
C
      IF(SIGDEC .LE. EPS) SIGDEC = 0.20 * DECAL
C
      I = 0
      DEDXM = DEDXT0
      DEDXM_EM=DEDXT0_EM
  111 CONTINUE
      I = I + 1
      ECONE(I) = DEDXM
      ECONE_EM(I)=DEDXM_EM
      SIGDEM = 0.50 * SQRT(DEDXM)
      SIGE = SQRT(SIGDEC**2 + SIGDEM**2)
      SIGZ(I) = SIGE
      DE = (DEDXM - DECAL)
      IF(SIGE .GT. 0.0) THEN
        MUISOL(I) = DE / SIGE
      ELSE
        MUISOL(I) = 99.0
      ENDIF
      IF(MUISOL(I) .GT. 99.0) MUISOL(I) = 99.0
      IF(MUISOL(I) .LT. -99.0) MUISOL(I) = -99.0
      IF(I .LT. 2 ) THEN
        DEDXM = DEDXT
        DEDXM_EM=DEDXT_EM
        GOTO 111
      ENDIF
C
      DO I=1,NCN
        J = I + 2
        DR2 = XISOCON(I)
        NC = INT(DR2 * 10)
        IF( I .EQ. 1 .OR. 
     &    (ABS(X(3)) .GT. 20.0 .AND. ABS(ETA) .LT. 1.5) ) THEN
          IER = -1
          CALL CAL_ECELL(PNTB,ETA,PHI,NC, EEM2,ETOTAL(I),ETT,IER)
        ELSE
          CALL CAL_ECONE(ETA, PHI, DR2, EEM2, ETOTAL(I), ETT, IER)
        ENDIF
        IF(IER .EQ. 0) THEN
          IF(ETOTAL(I) .LT. 0.0) ETOTAL(I) = 0.0
          ECONE(J) = ETOTAL(I)
          ECONE_EM(J)=EEM2
          SIG = 0.50 * SQRT(ETOTAL(I))
          SIG = SQRT(SIGDEC**2 + SIG**2)
          SIGZ(J) = SIG
          DE = (ETOTAL(I) - DECAL)
          IF(SIG .GT. 0.0) THEN
            MUISOL(J) = DE / SIG
          ELSE
            MUISOL(J) = 99.0
          ENDIF
          IF(MUISOL(J) .GT. 99.0) MUISOL(J) = 99.0
          IF(MUISOL(J) .LT. -99.0) MUISOL(J) = -99.0
        ENDIF
      ENDDO
C
C - Consider cone size 0.2
C
      DO I=1, 2+NCN
        DECALM(I) = ECONE(I)
        DECALM_EM(I)=ECONE_EM(I)
      ENDDO
      CDEDX  = SIGZ(3)
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
