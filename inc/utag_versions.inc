C----------------------------------------------------------------------
C-
C-   Created  17-MAY-1994   Ian Adam
C-   Updated  26-OCT-1995   Ian Adam  Version 5 
C-
C----------------------------------------------------------------------
        INTEGER N_VERSION,N_TAGS_MAX
        PARAMETER( N_VERSION  = 5 )
        PARAMETER( N_TAGS_MAX = 200 )
        INTEGER       IDMAXV(N_VERSION)
        INTEGER       NDIMGV(N_VERSION,MAX_GRP)
        CHARACTER*4   XGRPV(N_VERSION,MAX_GRP)
        CHARACTER*8   XTAGSV(N_VERSION,MAX_GRP,N_TAGS_MAX)

        COMMON /UTAG_VERSIONS/ IDMAXV,NDIMGV,XGRPV,XTAGSV
