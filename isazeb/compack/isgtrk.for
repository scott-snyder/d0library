      SUBROUTINE ISGTRK(ID,XYZ,PXYZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Fill ISAJET common blocks with information from one track
C-
C-   Inputs  : 
C-   ID     = particle ID
C-   XYZ(3) = x,y,z of track
C-   PXYZ(3)=px,py,pz of track
C-   
C-
C-   Created   6-MAR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PARTCL.INC'
      INTEGER ID
      REAL    XYZ(3),PXYZ(3)
      REAL    M1,AMASS
C----------------------------------------------------------------------
      M1=AMASS(ID)
      NPTCL=NPTCL+1
      PPTCL(1,NPTCL)=PXYZ(1)
      PPTCL(2,NPTCL)=PXYZ(2)
      PPTCL(3,NPTCL)=PXYZ(3)
      PPTCL(4,NPTCL)=SQRT(PXYZ(1)**2+PXYZ(2)**2+PXYZ(3)**2+M1**2)
      PPTCL(5,NPTCL)=M1
      IDCAY(NPTCL)=0
      IORIG(NPTCL)=0
      IDENT(NPTCL)=ID
      CALL SETVTX(XYZ)
  999 RETURN
      END
