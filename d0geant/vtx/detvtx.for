C DETVTX
      SUBROUTINE DETVTX
C**********************************************************************
C
C  DETVTX initializes the vertex detector for hits and digitization
C  T. Trippe, Nov. 4, 1985
C-   Updated  21-OCT-1988   Ghita Rahal-Callot  : suppress one level of volumes
C-                                                the definition of the sets 
C-   Updated  25-JUN-1989   Thomas G. Trippe  : New geometry, see D0 note 808 
C-   Updated   9-APR-1992   Qizhong Li-Demarteau  change all NBITSH to 32 to
C-                                              solve the overflow in GSAHIT
C- 
C
C**********************************************************************
C
      INTEGER NBITS(2)
      character*4 NAMES0(2),NAMES1(2),NAMES2(2)
      INTEGER ISET,IDET,NDAT
      PARAMETER (NDAT=11)
      INTEGER NBITSH(NDAT)
      character*4 NAMESH(NDAT)
      REAL ORIG(NDAT),FACT(NDAT)
C
      DATA NAMES0/'VSC0','VCL0'/
      DATA NAMES1/'VSC1','VCL1'/
      DATA NAMES2/'VSC2','VCL2'/
      DATA NBITS/16, 16/
      DATA NAMESH/'XGLO','YGLO','ZGLO','XLOC','PLHT','Z+  ','Z-  ',
     + 'TKLN','TKID','XLEN','TOF '/
      DATA NBITSH/11*32/
      DATA ORIG/3* 64.,      10.,  0., 3*  0., 0.,    0.,  0./
      DATA FACT/3*512.,10000000., 10., 3*512., 1., 8192., 16./
C
C ****    Define set and detector volume parameters.
C
      CALL GSDET('VTX ','VCL0',2,NAMES0,NBITS,61,1500,500,ISET,IDET)
      CALL GSDET('VTX ','VCL1',2,NAMES1,NBITS,62,1500,500,ISET,IDET)
      CALL GSDET('VTX ','VCL2',2,NAMES2,NBITS,63,1500,500,ISET,IDET)
C
C  Define set and detector hit parameters.
C
      CALL GSDETH('VTX ','VCL0',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('VTX ','VCL1',NDAT,NAMESH,NBITSH,ORIG,FACT)
      CALL GSDETH('VTX ','VCL2',NDAT,NAMESH,NBITSH,ORIG,FACT)
C
      RETURN
      END

