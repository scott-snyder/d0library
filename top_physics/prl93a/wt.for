      REAL FUNCTION WT(XDUMMY)
      REAL
     +MET1    ,MET2    ,RUN     ,PHNU    ,ETEL    ,ETAE    ,
     +PHEL    ,MWT     ,ETW     ,CHSQ    ,EISO    ,MIPS    ,
     +DPHI    ,STRK    ,ZVTX    ,ETJ1    ,ETJ2    ,ETJ3    ,
     +ETJ4    ,ETJ5    ,ETJ6    ,ETMU    ,ETAM    ,PHMU    ,
     +ETA1    ,ETA2    ,ETA3    ,ETA4    ,ETA5    ,ETA6    ,
     +PHJ1    ,PHJ2    ,ETE2    ,TAE2    ,PHE2    ,MWH     ,
     +MWJ     ,M3J     ,ETMC    ,YMMC    ,MISO    ,EMOB    ,
     +METC    ,EJ1     ,EJ2     ,EJ3     ,EJ4     ,EJ5     ,
     +EJ6     ,SPHE    ,APLA    ,SPH1    ,APL1    ,MET3    ,
     +RID      
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),
     +MET1    ,MET2    ,RUN     ,PHNU    ,ETEL    ,ETAE    ,
     +PHEL    ,MWT     ,ETW     ,CHSQ    ,EISO    ,MIPS    ,
     +DPHI    ,STRK    ,ZVTX    ,ETJ1    ,ETJ2    ,ETJ3    ,
     +ETJ4    ,ETJ5    ,ETJ6    ,ETMU    ,ETAM    ,PHMU    ,
     +ETA1    ,ETA2    ,ETA3    ,ETA4    ,ETA5    ,ETA6    ,
     +PHJ1    ,PHJ2    ,ETE2    ,TAE2    ,PHE2    ,MWH     ,
     +MWJ     ,M3J     ,ETMC    ,YMMC    ,MISO    ,EMOB    ,
     +METC    ,EJ1     ,EJ2     ,EJ3     ,EJ4     ,EJ5     ,
     +EJ6     ,SPHE    ,APLA    ,SPH1    ,APL1    ,MET3    ,
     +RID      
C
      IF (ETEL.GT.20..AND.METC.GT.30..AND.ETJ4.GT.15.
     &  .AND.STRK.LT.5.0.AND.EISO.LT.0.1 ) THEN
        WT=1.0
      ELSE
        WT=0.
      ENDIF
      END
