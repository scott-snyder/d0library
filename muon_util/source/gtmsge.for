      SUBROUTINE GTMSGE(NMOD,NCEL,NPMT,IORN,XYZ,DXYZ,JERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Scintillator Geometry
C-
C-   Inputs  :  NMOD    Module ID
C-              NCEL    Cell Number
C-   Outputs :  NPMT    Number of Phototubes 
C-              IORN    Orientation of scintillator
C-              XYZ(3)  Center of Scintillator  (Global Geometry)
C-              DXYZ(3) Half widths of Scintillator size (Local Geometry)
C-              JERR    Error flag (0 if OK)
C-   Controls:
C-
C-   Created   4-NOV-1993   B.S.Acharya
C-   Modified  5/95 MF  Remove fatal error messages, add orientation
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- Arguments
      INTEGER NMOD,NCEL,NPMT,IORN,JERR
      REAL    XYZ(3),DXYZ(3)
C- Includes
      INCLUDE 'D0$INC:ZEBSTP.INC'
C- Local Variables
      INTEGER I,ISCINT,IADD,IPMT,NSCI
      INTEGER LMSGE,GZMSGE
      INTEGER LMSGM,GZMSGM
C-
C----------------------------------------------------------------------
C
      NPMT=0
      IORN=0
      DO I=1,3
        XYZ(I)=0.0
        DXYZ(I)=0.0
      ENDDO
C
C          Point to MSGM bank
C
      JERR = 1
      LMSGM = GZMSGM(NMOD)
      IF (LMSGM.EQ.0) GOTO 999
      ISCINT = IC(LMSGM+10)
      IPMT = IC(LC(LMSGM-1)+10)  ! Assume IPMT the same for entire module
C
C          Point to MSGE bank
C
      JERR = 2
      NSCI = (NCEL/4)/IPMT + 1
      IF (NSCI.GT.ISCINT) GOTO 999
      LMSGE = GZMSGE(NMOD,NSCI)
      JERR = 3
      IADD = NMOD*256 + NCEL
      IF(IADD.NE.IC(LMSGE+9)) GOTO 999
C
C          Extract geometry constants
C
      JERR = 0
      NPMT = IC(LMSGE+10)
      IORN = IC(LMSGE+11)
      DO I =1,3
        XYZ(I)=C(LMSGE+11+I)
        DXYZ(I)=C(LMSGE+15+I)/2.0
      ENDDO
C
  999 RETURN
      END
