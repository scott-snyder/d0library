C----------------------------------------------------------------------
C-
C-   UTAG_DATA.INC
C-   Updated  13-NOV-1995   Ulrich Heintz increase MAX_GRP to 25
C--------------------------------------------------------------------
      INTEGER MAX_WORDS,MAX_GRP
      PARAMETER (MAX_WORDS=10000,MAX_GRP=25)
      INTEGER NTAGS_GRP(MAX_GRP),MAP(MAX_WORDS,MAX_GRP),NWORD(MAX_GRP)
      INTEGER NDIMG1(MAX_GRP),IDMAX,IELE(MAX_WORDS,MAX_GRP)
      INTEGER NOBJ(MAX_GRP)
      CHARACTER*1 IOR(MAX_WORDS,MAX_GRP)
      CHARACTER*4 XGRP1(MAX_GRP),BANK_NAME(MAX_WORDS,MAX_GRP)
      CHARACTER*4 LINK_BANK(MAX_WORDS,MAX_GRP)
      CHARACTER*13 TAGS_MAP(2,MAX_WORDS,MAX_GRP)
      REAL UDST_DATA(MAX_WORDS,MAX_GRP)
      COMMON/UTAG_DATA/XGRP1,NTAGS_GRP,MAP,NWORD,NDIMG1,NOBJ,IDMAX,
     &  TAGS_MAP,BANK_NAME,LINK_BANK,IOR,IELE,UDST_DATA
C----------------------------------------------------------------------
