      REAL*8 MQ(6),ML(6),MW,MZ,MBOT
      REAL*8 MSQ(12),MSL(12),MGF(3),MH(4),MH0
      REAL*8 PI,SW2,ALPHA(3),ALPHAW,VEV
      REAL*8 M0,MHALF,MMU,A0,B0,MX,ALPHAX,GTOPX,GBOTX,GTAUX
      REAL*8 TBETA,MUZ,BZ,ATOPZ,ABOTZ,ATAUZ,HALPHA, GTOP_limit
      Logical gtop_too_big
      real*8 gtop_max
      real elapsed_time
      COMMON/SMMASSES/MQ,ML,MW,MZ,MBOT
      COMMON/SUSYMASSES/MSQ,MSL,MGF,MH,MH0
      COMMON/CONSTS/PI,SW2,ALPHA,ALPHAW,VEV
      COMMON/GUTPARAMS/M0,MHALF,MMU,A0,B0,MX,ALPHAX,GTOPX,GBOTX,GTAUX,
     +  gtop_limit
      COMMON/LOWEPARAMS/TBETA,MUZ,BZ,ATOPZ,ABOTZ,ATAUZ,HALPHA
      COMMON/FLAGS/ gtop_too_big
      COMMON/TIMING/ elapsed_time
      save gtop_max

