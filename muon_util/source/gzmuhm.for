      INTEGER FUNCTION GZMUHM(MODID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :     Find pointer to MUHM bank associated
C-                             with module #MODID
C-
C-   Created  13-JAN-1994   M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,LMUHT,GZMUHT,NMOD,MODID
      DATA NMOD/460/
C
      GZMUHM=0
      LMUHT=GZMUHT(0)
      IF(LMUHT.NE.0.AND.MODID.GE.10.AND.MODID.LE.NMOD) THEN
          GZMUHM=LQ(LMUHT-MODID)
      ENDIF
C
      RETURN
      END
