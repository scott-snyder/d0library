      SUBROUTINE VEEFIT(I1,I2,XYZ,EXYZ,DXYZ,PHI1,PHI2,THE1,THE2)
C------------------------------------------------------------------
C
C  Book and fill banks VERT and PVES for a vee; fit the vee
C
C  9-JUL-1990  Daria Zieminska
C  8-AUG-1991  Tom Trippe - add phi's, theta's, track status, ref.
C                           D0 standards, remove unused EZPICK call.
C 12-oct-1991  Vladimir Burtovoy - updated for kinematical analysis
C-   Updated   7-NOV-1991   Daria Zieminska  use the calorimeter information
C-   Updated  12-DEC-1991   Daria Zieminska modified PVES format;
C-                          moved the call to ZENERGY to VEE
C-   Updated  21-SEP-1993   H. Castilla, B. Gomez & O. Ramirez to include the
C-                          decay lambda --> proton + pion &
C-                                lambda --> pion +  proton
C-
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER NR,LVERH,GZVERH,LVERT,IER,ICALL,ISETVN,LPARH,GZPARH
      INTEGER LPVES,NPVES,NVERT,I1,I2,GZVERT,GZZTRK,LTRAK,PRUNIT,USUNIT
      INTEGER LTRACK(2),ITRK,IVEE,IVCF,LASSOC
      LOGICAL OK,VKINEMA,CAL,CORRECT
      REAL SIGMA_PHI_VEE,SIGMA_THETA_VEE
      REAL XYZ(3),EXYZ(3),DXYZ(3),EDXYZ(3),PXYZ(3),EPXYZ(3)
      REAL DX,DY,DZ,PHI,THETA,THETA_AV,LENGTH,PHI1,PHI2,THE1,THE2
      REAL GAMMA_K0,GAMMA_L1,GAMMA_L2
      REAL MAXLEN,MAXLAM
      REAL PIM,K0,PROTON,LAMBDA,ELM
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        DATA PIM/ 0.1395675/, K0/ 0.497671 /,PROTON/ 0.93827231/
        DATA LAMBDA/ 1.11563/, ELM/ 0.51099906E-3/
        NR=2  ! number of ref. links = number of tracks on vertex
        MAXLEN=25.     ! RCP Driven
        MAXLAM=75.     ! RCP Driven
        CALL EZPICK('VEES_RCP')
        CALL EZGET('MAXLEN',MAXLEN,IER)
        CALL EZGET('MAXLAM',MAXLAM,IER)
        CALL EZRSET
        PRUNIT=USUNIT()
        ICALL=1
      END IF
      DX=DXYZ(1)  ! vector linking vee and primary vertex
      DY=DXYZ(2)
      DZ=DXYZ(3)
C
C  Find vee direction
C
      PHI=ATAN2(DY,DX)  ! phi of vee
      IF (PHI.LT.0.) PHI=PHI+TWOPI
      THETA=ATAN2(SQRT(DX**2+DY**2),DZ)  ! theta of vee
      THETA_AV=(THE1+THE2)/2.   ! average theta of decay particles
      IF (ABS(THETA-THETA_AV).GT.1.) GO TO 1000
      LVERH=GZVERH()
      IF (LVERH.LE.0) GO TO 1000  ! vertex header not booked
      IQ(LVERH+3)=IQ(LVERH+3)+1   ! Increment # of decay vertices
      NVERT=IQ(LVERH+2)+IQ(LVERH+3)
C
C  Book vertex bank VERT.
C
      CALL BKVERT(LVERT,NR)
      IQ(LVERT)=ISETVN(IQ(LVERT),0)     ! set version number
      IQ(LVERT-5)=NVERT                 ! vertex ID
      IQ(LVERT+1)=0                     ! version number
      IQ(LVERT+2)=IBSET(IQ(LVERT+2),29) ! secondary vertex
      Q(LVERT+3)=XYZ(1)
      Q(LVERT+4)=XYZ(2)
      Q(LVERT+5)=XYZ(3)
      Q(LVERT+6)=EXYZ(1)
      Q(LVERT+7)=EXYZ(2)
      Q(LVERT+8)=EXYZ(3)
      Q(LVERT+15)=PHI1
      Q(LVERT+16)=THE1
      Q(LVERT+17)=PHI2
      Q(LVERT+18)=THE2
      LTRACK(1)=GZZTRK(I1)
      LQ(LVERT-2)=LTRACK(1)
      LTRACK(2)=GZZTRK(I2)
      LQ(LVERT-3)=LTRACK(2)
C
C  Set ZTRK status, reference link
C
      DO ITRK = 1,2
        LTRAK = LTRACK(ITRK)
        IVEE = IBITS(IQ(LTRAK+1),4,1)
        IF (IVEE .EQ. 0) THEN
          IQ(LTRAK) = IBSET(IQ(LTRAK),5)
          LQ(LTRAK-2) = LVERT
        ELSE
          LQ(LTRAK-2) = 0   ! ambiguity, track is in more than 1 vee
        ENDIF
C
C  Set status of associated CDC, FDC, and VTX tracks
        DO IVCF = 6,8     ! 6 = VTXT, 7 = DTRK, 8 = FDCT
          LASSOC = LQ(LTRAK - IVCF)
          IF(LASSOC .NE. 0) THEN
            IQ(LASSOC) =IBSET(IQ(LASSOC),9)
          ENDIF
        ENDDO
      ENDDO
C
C  Book particle bank PVES
C
      LPARH=GZPARH()
      IF (LPARH.EQ.0) THEN
        CALL BKPARH(LPARH)
      END IF
      IQ(LPARH+6)=IQ(LPARH+6)+1
      NPVES=IQ(LPARH+6)
      CALL BKPVES(LPVES)
      IQ(LPVES-5)=NPVES
      IQ(LPVES+1)=1      ! Version number
      LVERT=GZVERT(NVERT)
      LQ(LPVES-2)=LVERT
      LTRAK=GZZTRK(I1)
      LQ(LPVES-3)=LTRAK
      LTRAK=GZZTRK(I2)
      LQ(LPVES-4)=LTRAK
      Q(LPVES+23)=PHI
      Q(LPVES+21)=THETA
      CALL SIGMA_VEE(LPVES,SIGMA_PHI_VEE,SIGMA_THETA_VEE)
      Q(LPVES+6)=STR(1,1)
      Q(LPVES+7)=STR(1,2)
      Q(LPVES+24)=SIGMA_PHI_VEE
      Q(LPVES+22)=SIGMA_THETA_VEE
      OK = VKINEMA(NPVES)
      IF (.NOT.OK) GO TO 1000
      LENGTH=SQRT(DX**2+DY**2+DZ**2)
      GAMMA_K0=SQRT(K0**2+Q(LPVES+19)**2)/K0
      GAMMA_L1=SQRT(LAMBDA**2+Q(LPVES+36)**2)/LAMBDA
      GAMMA_L2=SQRT(LAMBDA**2+Q(LPVES+53)**2)/LAMBDA
      Q(LPVES+25)=LENGTH/GAMMA_K0
      Q(LPVES+42)=LENGTH/GAMMA_L1
      Q(LPVES+59)=LENGTH/GAMMA_L2
      IF(Q(LPVES+25).GT.MAXLEN .AND.Q(LPVES+42).GT.MAXLAM .AND.
     &  Q(LPVES+59).GT.MAXLAM ) CALL PVESDR(NPVES)
 1000 RETURN
      END
