      SUBROUTINE PVESFL(NX,NM,GXI,GMFI,IEND,CHISQ,PROB,NDF,NSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : update bank PVES adding kinematic fit results
C-
C-   Inputs  :
C              IEND   >    0 - fit is successful
C-             IEND   <    0 - fit is failed
C-             CHISQ         - chi-squared
C-             PROB          - probability
C-             NDF           - number of degrees of freedom
C-             NSTEP         - number of iteration steps
C-
C-   Called  by  VKIN_NCFIT
C-
C-   Created  24-jul-1991   V. Burtovoy
C-   Updated  21-OCT-1991   Daria Zieminska  D0 standards;
C-                          modify PVES format
C-   Updated  21-SEP-1993   H. Castilla, B. Gomez & O. Ramirez to include the
C-                          decay lambda --> proton + pion &                    
C-                                lambda --> pion +  proton
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL*8 GMFI(1500),GXI(20)
      REAL     MASS,MASS1,MASS2,CHISQ,PROB
      INTEGER  NPVES,GZPVES,LPVES,LPARH,GZPARH
      INTEGER  I,J,K,NX,NM,IEND,NSTEP,L,NXT,NDF,M0,MT
      LPARH=GZPARH()
      NPVES=IQ(LPARH+6)
      IF (NPVES.LE.0) GO TO 999
      LPVES=GZPVES(NPVES)
      IQ(LPVES+3) = NDF
      IQ(LPVES+4) = NM
      IQ(LPVES+5) = NX
      M0    = 10
      MT    = 17
      MASS = STR(4,NTR)
      IF (MASS. GT.0.2.AND. MASS .LT. 0.8 )  THEN         ! K0
        IQ(LPVES+M0+1) = IEND
        IQ(LPVES+M0+2) = NSTEP
        IF (IEND.GE.0) IQ(LPVES+2)=IBSET(IQ(LPVES+2),1)
        Q(LPVES+M0+3)=STR(1,1)
        Q(LPVES+M0+4)=STR(2,1)
        Q(LPVES+M0+5)=STR(3,1)
        Q(LPVES+M0+6)=STR(1,2)
        Q(LPVES+M0+7)=STR(2,2)
        Q(LPVES+M0+8)=STR(3,2)
        Q(LPVES+M0+9)=STR(1,3)
        Q(LPVES+M0+10)=ETR(1,3)
        Q(LPVES+M0+11)=STR(2,3)
        Q(LPVES+M0+12)=ETR(2,3)
        Q(LPVES+M0+13)=STR(3,3)
        Q(LPVES+M0+14)=ETR(3,3)
        Q(LPVES+M0+16)=CHISQ
        Q(LPVES+M0+17)=PROB
      ELSE IF( MASS.GT.0.8.AND.MASS .LT. 1.2 )  THEN      ! Lambda
        MASS1 = STR(4,1)
        IF( MASS1 .GT. 0.8 )  THEN    ! Lambda 1 -> proton + pi
          M0=M0+MT
          IQ(LPVES+M0+1) = IEND
          IQ(LPVES+M0+2) = NSTEP
          IF (IEND.GE.0) IQ(LPVES+2)=IBSET(IQ(LPVES+2),2)
          Q(LPVES+M0+3)=STR(1,1)
          Q(LPVES+M0+4)=STR(2,1)
          Q(LPVES+M0+5)=STR(3,1)
          Q(LPVES+M0+6)=STR(1,2)
          Q(LPVES+M0+7)=STR(2,2)
          Q(LPVES+M0+8)=STR(3,2)
          Q(LPVES+M0+9)=STR(1,3)
          Q(LPVES+M0+10)=ETR(1,3)
          Q(LPVES+M0+11)=STR(2,3)
          Q(LPVES+M0+12)=ETR(2,3)
          Q(LPVES+M0+13)=STR(3,3)
          Q(LPVES+M0+14)=ETR(3,3)
          Q(LPVES+M0+16)=CHISQ
          Q(LPVES+M0+17)=PROB
        ELSE                           ! Lambda 2 -> pi + proton
          M0=M0+2*MT
          IQ(LPVES+M0+1) = IEND
          IQ(LPVES+M0+2) = NSTEP
          IF (IEND.GE.0) IQ(LPVES+2)=IBSET(IQ(LPVES+2),3)
          Q(LPVES+M0+3)=STR(1,1)
          Q(LPVES+M0+4)=STR(2,1)
          Q(LPVES+M0+5)=STR(3,1)
          Q(LPVES+M0+6)=STR(1,2)
          Q(LPVES+M0+7)=STR(2,2)
          Q(LPVES+M0+8)=STR(3,2)
          Q(LPVES+M0+9)=STR(1,3)
          Q(LPVES+M0+10)=ETR(1,3)
          Q(LPVES+M0+11)=STR(2,3)
          Q(LPVES+M0+12)=ETR(2,3)
          Q(LPVES+M0+13)=STR(3,3)
          Q(LPVES+M0+14)=ETR(3,3)
          Q(LPVES+M0+16)=CHISQ
          Q(LPVES+M0+17)=PROB
        END IF
      END IF
  999 RETURN
      END
