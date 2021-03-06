      SUBROUTINE DETFDC
C------------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes the FDC for hits and digitization
C
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   23-JUL-1986  K. Ng
C-   Updated   dd-APR-1987  Daria Zieminska 
C-   Updated   7-MAR-1988   Ghita Rahal-Callot  : correct FACT for the pulse
C-                                                height
C-   Updated  15-OCT-1988   Jeffrey Bantly  : for new GEOFDC format 
C-   Updated   7-AUG-1989   Jeffrey Bantly  : reorder FDC geometry 
C-   Updated   9-APR-1992   Qizhong Li-Demarteau  change all NBITSH to 32 to
C-                                              solve the overflow in GSAHIT
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER NHIT
      PARAMETER (NHIT=10)
      CHARACTER*4 NAME01(6),NAME02(6),NAME03(6),NAME04(6),NAMESH(NHIT)
      CHARACTER*4 NAME05(6),NAME06(6),NAME07(6),NAME08(6)
      CHARACTER*4 NAME09(6),NAME10(6),NAME11(6),NAME12(6)
      CHARACTER*4 NAMEP0(6),NAMEP1(6),NAMEP2(6),NAMEP3(6)
      CHARACTER*4 NAMEP4(6),NAMEP5(6)
      INTEGER NBITS(6),NBITSH(NHIT),NBITP(6)
      INTEGER IDET,ISET
      REAL ORIG(NHIT),FACT(NHIT)
      DATA NAME01/'FDC ','FTH ','FWTB','FWB0','FDX0','FXZ0'/
      DATA NAME02/'FDC ','FTH ','FWTA','FWA0','FDY0','FYZ0'/
      DATA NAME03/'FDC ','FTH ','FWTB','FWB1','FDX1','FXZ1'/
      DATA NAME04/'FDC ','FTH ','FWTA','FWA1','FDY1','FYZ1'/
      DATA NAME05/'FDC ','FTH ','FWTB','FWB2','FDX2','FXZ2'/
      DATA NAME06/'FDC ','FTH ','FWTA','FWA2','FDY2','FYZ2'/
      DATA NAME07/'FDC ','FTH ','FWTB','FWB3','FDX3','FXZ3'/
      DATA NAME08/'FDC ','FTH ','FWTA','FWA3','FDY3','FYZ3'/
      DATA NAME09/'FDC ','FTH ','FWTB','FWB4','FDX4','FXZ4'/
      DATA NAME10/'FDC ','FTH ','FWTA','FWA4','FDY4','FYZ4'/
      DATA NAME11/'FDC ','FTH ','FWTB','FWB5','FDX5','FXZ5'/
      DATA NAME12/'FDC ','FTH ','FWTA','FWA5','FDY5','FYZ5'/
      DATA NAMEP0/'FDC ','FPH ','FDPH','FPC0','FPP0','FPZ0'/
      DATA NAMEP1/'FDC ','FPH ','FDPH','FPC1','FPP1','FPZ1'/
      DATA NAMEP2/'FDC ','FPH ','FDPH','FPC2','FPP2','FPZ2'/
      DATA NAMEP3/'FDC ','FPH ','FDPH','FPC3','FPP3','FPZ3'/
      DATA NAMEP4/'FDC ','FPH ','FDPH','FPC4','FPP4','FPZ4'/
      DATA NAMEP5/'FDC ','FPH ','FDPH','FPC5','FPP5','FPZ5'/
      DATA NBITS/6*8/
      DATA NBITP/6*8/
      DATA NAMESH/'XGLO','YGLO','ZGLO','XLOC','PLHT',
     &            'Z+  ','Z-  ','TLEN','ISAT','DXDZ'/
      DATA NBITSH/10*32/
      DATA ORIG/3*1000.,10.,6*0./
      DATA FACT/4*100.,10.,3*100.,1.,100./
C
C  Define set and detector volume parameters.
C       for THETA module
      CALL GSDET('FDC ','FXZ0',6,NAME01,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ0',6,NAME02,NBITS,92,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FXZ1',6,NAME03,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ1',6,NAME04,NBITS,92,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FXZ2',6,NAME05,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ2',6,NAME06,NBITS,92,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FXZ3',6,NAME07,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ3',6,NAME08,NBITS,92,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FXZ4',6,NAME09,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ4',6,NAME10,NBITS,92,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FXZ5',6,NAME11,NBITS,91,3300,1100,ISET,IDET)
      CALL GSDET('FDC ','FYZ5',6,NAME12,NBITS,92,3300,1100,ISET,IDET)
C       for PHI module
      CALL GSDET('FDC ','FPZ0',6,NAMEP0,NBITP,93,3300,1100,
     &                           ISET,IDET)
      CALL GSDET('FDC ','FPZ1',6,NAMEP1,NBITP,93,3300,1100,
     &                           ISET,IDET)
      CALL GSDET('FDC ','FPZ2',6,NAMEP2,NBITP,93,3300,1100,
     &                           ISET,IDET)
      CALL GSDET('FDC ','FPZ3',6,NAMEP3,NBITP,93,3300,1100,
     &                           ISET,IDET)
      CALL GSDET('FDC ','FPZ4',6,NAMEP4,NBITP,93,3300,1100,
     &                           ISET,IDET)
      CALL GSDET('FDC ','FPZ5',6,NAMEP5,NBITP,93,3300,1100,
     &                           ISET,IDET)
C
C  Define set and detector hit parameters.
C       for THETA module
      CALL GSDETH('FDC ','FXZ0',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ0',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FXZ1',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ1',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FXZ2',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ2',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FXZ3',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ3',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FXZ4',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ4',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FXZ5',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FYZ5',NHIT,NAMESH,NBITSH,ORIG,FACT)
C       for PHI module
      CALL GSDETH('FDC ','FPZ0',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FPZ1',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FPZ2',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FPZ3',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FPZ4',NHIT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('FDC ','FPZ5',NHIT,NAMESH,NBITSH,ORIG,FACT)
      RETURN
      END
