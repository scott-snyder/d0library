      SUBROUTINE DETMUO_V1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-    This initializes Muon Drift Chambers for hits and
C-  digitization.
C-
C-   Inputs  : None
C-   Outputs : Geant banks
C-   Controls: None
C-   External:
C-    S/R MUMODU    to get parameters for PDT module, IM.
C-    S/R MUMODC    to get cell name for PDT module, IM.
C-    S/R GSDET     to define Geant detector for PDT module.
C-    S/R GSDETH    to define HIT bank in Geant.
C              S/R DETMUO  (V2.0)
C-
C-   Created   5-MAR-1987   S. Kunori
C-   Updated   5-APR-1988   S. Kunori
C-                          fixed fifth argument in call GSDET.
C---------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER     IM,ISET,IDET
      CHARACTER*4 VMNAME,VPNAME,VCNAME
      character*4     NAMESV(2),NAMEH(6), HHNAME
      INTEGER  NBITSH(6),NBITSV(2)
      REAL ORIG(6),FACT(6)
C
      DATA NBITSV/2*16/
      DATA NBITSH/3*16,3*32/
      DATA ORIG/6*0./
      DATA FACT/3*0.,3*1000./
      DATA NAMEH/'MODU','PLNE','WIRE','DTIM','XLN1','XLN2'/
C---------------------------------------------------------------
C
C  Store Hollerith names.
C  ======================
C
C      CALL UCTOH('MODU',NAMEH(1),4,4)
C      CALL UCTOH('PLNE',NAMEH(2),4,4)
C      CALL UCTOH('WIRE',NAMEH(3),4,4)
C      CALL UCTOH('DTIM',NAMEH(4),4,4)
C      CALL UCTOH('XLN1',NAMEH(5),4,4)
C      CALL UCTOH('XLN2',NAMEH(6),4,4)
C
C  Loop over PDT modules.
C  ======================
C
      DO 100 IM=1,307
C        -- get Geant volume names for module, plane, cell...
C        -- names will be ACxx, BCxx and CCxx for cell in A-,
C        -- B- and C-layer respectively.   xx is last two
C        -- digits of module number.
         CALL MUMODC(IM,VMNAME,VPNAME,VCNAME)
C
C        -- check if module exist.  if NPAR is zero, not exists...
         IF(VMNAME.NE.' ') THEN
C
C           -- define detector...
C
C            CALL UCTOH(VPNAME,NAMESV(1),4,4)
C            CALL UCTOH(VCNAME,NAMESV(2),4,4)
C            CALL UCTOH(VCNAME,HHNAME,   4,4)
C
            namesv(1)=vpname
            namesv(2)=vcname
            CALL GSDET('MPDT',VCNAME,2,NAMESV,NBITSV,21
     +                ,100,100,ISET,IDET)
            CALL GSDETH('MPDT',VCNAME,6,NAMEH,NBITSH,ORIG,FACT)
         ENDIF
100   CONTINUE
C
  999 RETURN
      END
