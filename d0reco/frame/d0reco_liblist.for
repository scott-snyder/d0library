      SUBROUTINE D0RECO_LIBLIST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Print versions of libraries used by D0RECO
C-
C-   Created  22-JUN-1990   Serban D. Protopopescu
C-   Updated  21-FEB-1992   Qizhong Li-Demarteau  use split libraries
C-                                                for Central Detectors 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*72 V,VD0RECO,VCALOR_UTIL,VGENERAL
      CHARACTER*72 VMUON_UTIL
      CHARACTER*72 VVTX_UTIL, VTRD_UTIL, VCDC_UTIL, VFDC_UTIL, VCD_UTIL
      INTEGER SUM_UNIT,SSUNIT
C----------------------------------------------------------------------
      SUM_UNIT=SSUNIT()
      WRITE(SUM_UNIT,1001) 
      V=VD0RECO()
      WRITE(SUM_UNIT,1002) V
      V=VCALOR_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VGENERAL()
      WRITE(SUM_UNIT,1002) V
      V=VMUON_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VVTX_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VTRD_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VCDC_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VFDC_UTIL()
      WRITE(SUM_UNIT,1002) V
      V=VCD_UTIL()
      WRITE(SUM_UNIT,1002) V
      WRITE(SUM_UNIT,1003)
 1001 FORMAT(///,2X,18('*'),'  Version numbers of libraries used',
     &  18('*')/)
 1002 FORMAT(1X,A)
 1003 FORMAT(/,2X,72('*'),/)
  999 RETURN
      END
