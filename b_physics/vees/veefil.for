      SUBROUTINE VEEFIL(I1,I2,XYZ,EXYZ,DXYZ,EDXYZ,RATIO,MOM,
     &                   PHI1,PHI2,THE1,THE2)
C------------------------------------------------------------------
C
C  Book and fill banks VERT and PVES for a vee
C
C  9-JUL-1990  Daria Zieminska
C  8-AUG-1991  Tom Trippe - add phi's, theta's, track status, ref.
C                     D0 standards, remove unused EZPICK call.
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF/LIST'
      INTEGER NR,LVERH,GZVERH,LVERT,IER,ICALL,ISETVN,LPARH,GZPARH
      INTEGER LPVES,NPVES,NVERT,I1,I2,GZVERT,GZZTRK,LTRAK,PRUNIT,USUNIT
      INTEGER LTRACK(2),ITRK,IVEE,IVCF,LASSOC
      REAL XYZ(3),EXYZ(3),DXYZ(3),EDXYZ(3),PXYZ(3),EPXYZ(3),RATIO,MOM(2)
      REAL K0,LENGTH,DX,DY,DZ,PHI,THETA,GAMMA,PHI1,PHI2,THE1,THE2
      DATA ICALL/0/
C------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        NR=2  ! number of ref. links = number of tracks on vertex
        K0=0.497
        PRUNIT=USUNIT()
        ICALL=1
      END IF
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
        IVEE = IBITS(IQ(LTRAK),5,1)
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
            IQ(LASSOC) = IBSET(IQ(LASSOC),9)
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
      DX=DXYZ(1)
      DY=DXYZ(2)
      DZ=DXYZ(3)
      PHI=ATAN2(DY,DX)
      IF (PHI.LT.0.) PHI=PHI+TWOPI
      THETA=ATAN2(SQRT(DX**2+DY**2),DZ)
      LENGTH=SQRT(DX**2+DY**2+DZ**2)
      GAMMA=SQRT(K0**2+MOM(1)**2)/K0
      LENGTH=LENGTH/GAMMA
      Q(LPVES+3)=PHI
      Q(LPVES+4)=THETA
      Q(LPVES+5)=RATIO
      Q(LPVES+6)=MOM(1)
      Q(LPVES+8)=LENGTH
C      CALL PRPVES(PRUNIT,LPVES,0,'ONE',0)
 1000 RETURN
      END
