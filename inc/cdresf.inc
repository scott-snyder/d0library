      INTEGER NHTDLY            
      PARAMETER( NHTDLY= 20)
      COMMON /CDRESF/ SUMRES(0:MXSENS,0:31,0:3),
     &                SUMRE2(0:MXSENS,0:31,0:3),
     &                NBRES (0:MXSENS,0:31,0:3),
     &                NZRES( NBDELY,0:31,0:3),
     &                SZRES( NBDELY,0:31,0:3),
     &                SZRE2( NBDELY,0:31,0:3),
     &                NDLEV (NBDELY,0:31,0:3),
     &                SDLRES(NBDELY,0:31,0:3),
     &                SDLRE2(NBDELY,0:31,0:3),
     &                DLRES(NHTDLY,NBDELY,0:31,0:3),
     &                NEVPED(0:MXFADC,0:31,0:3),
     &                SUMPED(0:MXFADC,0:31,0:3),
     &                SM2PED(0:MXFADC,0:31,0:3),
     &                NEVGAI(0:MXFADC,0:31,0:3),
     &                SUMGAI(0:MXFADC,0:31,0:3),
     &                SU2GAI(0:MXFADC,0:31,0:3)
      REAL    SUMRES, SUMRE2, DLRES, SZRES, SZRE2, SDLRES, SDLRE2
      INTEGER NBRES, NZRES, NDLEV, NEVPED, NEVGAI
      REAL    SUMPED, SM2PED, SUMGAI, SU2GAI
