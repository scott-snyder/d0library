      SUBROUTINE DETMUO
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
C-   Created   5-MAR-1997   S. Kunori
C-   Updated   5-APR-1988   S. Kunori
C-                          fixed fifth argument in call GSDET.
C-   Updated   9-APR-1991   Susumu Igarashi 
C-                          changed argument in call GSDETH 6 -> 9
C-   Updated  27-FEB-1992   Susumu Igarashi  hollerith to character 
C---------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER     IM,ISET,IDET
      CHARACTER*4 VMNAME,VPNAME,VCNAME
      CHARACTER*4 NAMESV(2),NAMEH(9)
      INTEGER     HHNAME,NBITSH(9),NBITSV(2)
      REAL ORIG(9),FACT(9)
C
      DATA NBITSV/2*16/
      DATA NBITSH/3*16,6*32/
      DATA ORIG/3*0.,100.,2*0.,200.,2*0./
      DATA FACT/3*1.,6*1000./
      DATA NAMEH/'MODU','PLNE','WIRE','XDIS','ZLN1','ZLN2',
     &           'THET','PHI ','TOF '/
C---------------------------------------------------------------
********************************************
*     Big Branch to old version,  V1.      *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         CALL DETMUO_V1
         RETURN
      ENDIF
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
            NAMESV(1)=VPNAME
            NAMESV(2)=VCNAME
C
            CALL GSDET('MPDT',VCNAME,2,NAMESV,NBITSV,21
     +                ,100,100,ISET,IDET)
            CALL GSDETH('MPDT',VCNAME,9,NAMEH,NBITSH,ORIG,FACT)
         ENDIF
100   CONTINUE
C
  999 RETURN
      END
