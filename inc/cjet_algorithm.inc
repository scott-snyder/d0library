      INTEGER MAX_PARAMS,N_ALG
      PARAMETER (MAX_PARAMS=20)
      INTEGER ALG_PARAMS(MAX_PARAMS),TYPE_PARAMS(MAX_PARAMS),NPARAMS
      CHARACTER ALGORITHM*20
      COMMON/CJET_ALGORITHM/ALG_PARAMS,TYPE_PARAMS,NPARAMS,ALGORITHM