      REAL FUNCTION CORPRE(XDUMMY)
      REAL ALPHA
      REAL
     +DAY     ,D_TIME  ,GAIN    ,DGAI    ,ATTC    ,DATT    ,
     +ENRJ    ,SNRJ    ,PPAT    ,PRAT    ,TRM     ,PXEN    ,
     +AO2     ,TPLT    ,PCAN    ,HV_001  ,HV_002  ,HV_003  ,
     +HV_004  ,HV_005  ,HV_006  ,HV_007  ,HV_008  ,HV_009  ,
     +HV_010  ,HV_011  ,HV_012  ,HV_013  ,HV_014  ,HV_015  ,
     +HV_016  ,HV_017  ,HV_018  ,HV_019  ,HV_020  ,HV_021  ,
     +HV_022  ,HV_023  ,HV_024  ,HV_025  ,HV_026  ,HV_027  ,
     +HV_028  ,HV_029  ,HV_030  ,HV_031  ,HV_032  ,HV_033  ,
     +HV_034  ,HV_035  ,HV_036  ,HV_037  ,HV_038  ,HV_039  ,
     +HV_040  ,HV_041  ,HV_042  ,HV_043  ,HV_044  ,HV_045  ,
     +HV_046  ,HV_047  ,HV_048  ,WIND    ,POT     ,GRID     
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),
     +DAY     ,D_TIME  ,GAIN    ,DGAI    ,ATTC    ,DATT    ,
     +ENRJ    ,SNRJ    ,PPAT    ,PRAT    ,TRM     ,PXEN    ,
     +AO2     ,TPLT    ,PCAN    ,HV_001  ,HV_002  ,HV_003  ,
     +HV_004  ,HV_005  ,HV_006  ,HV_007  ,HV_008  ,HV_009  ,
     +HV_010  ,HV_011  ,HV_012  ,HV_013  ,HV_014  ,HV_015  ,
     +HV_016  ,HV_017  ,HV_018  ,HV_019  ,HV_020  ,HV_021  ,
     +HV_022  ,HV_023  ,HV_024  ,HV_025  ,HV_026  ,HV_027  ,
     +HV_028  ,HV_029  ,HV_030  ,HV_031  ,HV_032  ,HV_033  ,
     +HV_034  ,HV_035  ,HV_036  ,HV_037  ,HV_038  ,HV_039  ,
     +HV_040  ,HV_041  ,HV_042  ,HV_043  ,HV_044  ,HV_045  ,
     +HV_046  ,HV_047  ,HV_048  ,WIND    ,POT     ,GRID     
      DIMENSION XDUMMY( 66)
      ALPHA=6.2
      CORPRE=(PRAT+PCAN)/1013.
      IF(CORPRE.GT.0.) THEN
        CORPRE=CORPRE**ALPHA
      ELSE
        CORPRE=1.
      ENDIF
      END
