      SUBROUTINE PDILFL(NX,NM,GXI,GMFI,IEND,CHI,PRB,NDF,NSTEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill PDIL bank (dileptons) 
C-
C-   Inputs  :  
C              IEND   >    0 - fit is successful
C-             IEND   <    0 - fit is failed 
C-             CHI    -    chi-squared 
C-             PRB    -    probability 
C-             NDF    -    number of degrees of freedom 
C-             NSTEP  -    number of iteration steps
C-
C-   Called  by  VKIN_NCFIT
C-
C-   Created 5-DEC-1991   Daria Zieminska  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:VEEKIN.INC/LIST'
      REAL*8 GMFI(1500),GXI(20)
      REAL     MASS,CHI,PRB,COSALFA
      INTEGER NX,NM,ISETVN
      INTEGER  NPDIL,GZPDIL,LPDIL,LPARH,GZPARH
      INTEGER  IEND,NSTEP,NDF 
      IF (IEND.LE.0) GO TO 999
C
C  Book bank PDIL.
C
      LPARH=GZPARH()
      IF (LPARH.LE.0) GO TO 999 ! particle header not booked
      COSALFA=0 ! to be calculated using STR
      IQ(LPARH+8)=IQ(LPARH+8)+1 
      NPDIL=IQ(LPARH+8) 
      CALL BKPDIL(LPDIL)
      IQ(LPDIL)=ISETVN(IQ(LPDIL),0)     ! set version number
      IQ(LPDIL-5)=NPDIL                 ! dilepton ID
      IQ(LPDIL+1)=0                     ! version number
      IQ(LPDIL+4)=NDF
      IQ(LPDIL+5)=IEND 
      IQ(LPDIL+6)=NSTEP 
      Q(LPDIL+16)=STR(1,1)
      Q(LPDIL+17)=STR(2,1)
      Q(LPDIL+18)=STR(3,1)
      Q(LPDIL+19)=STR(1,2)
      Q(LPDIL+20)=STR(2,2)
      Q(LPDIL+21)=STR(3,2)
      Q(LPDIL+22)=STR(1,3)
      Q(LPDIL+23)=STR(2,3)
      Q(LPDIL+24)=STR(3,3)
      Q(LPDIL+25)=COSALFA 
      Q(LPDIL+26)=CHI 
      Q(LPDIL+27)=PRB 
  999 RETURN
      END
