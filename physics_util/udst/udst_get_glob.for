      SUBROUTINE UDST_GET_GLOB(N_PELC,N_PPHO,N_PMUO,N_PTAU,N_JET,N_VERT)
C----------------------------------------------------------------------
C-      PURPOSE: fetch info from GLOB bank for micro DSTs
C-
C-      INPUTS: N_PELC,N_PPHO,N_PMUO,N_PTAU,N_JET,N_VERT
C-
C-   Created 19-DEC-1993   Ulrich Heintz
C-   Updated   5-JAN-1994   Ian Adam  Split bitmasks MUSTP words 1 to 4
C-   Updated   6-JAN-1994   Ian Adam  Fix VZERO call
C-   Updated  10-JAN-1994   Ian Adam  Check for FULL_D0RECO in HSTR (code
C-                                                    taken from CLEANEM)
C-   Updated  25-JAN-1994   Ulrich Heintz  call RECO_VERSION
C-   Updated  11-OCT-1994   Ulrich Heintz  add L1 andor bits
C-   Updated  10-OCT-1995   Ian Adam  Add 12 words (#CDC,VERH info,PLV0)
C-   Updated  23-OCT-1995   Ulrich Heintz  save IQ(LFILT+19)
C-   Updated  11-DEC-1996   Ulrich Heintz  save low bits of IQ(LDTRH)  
C---------------------------------------------------------------------- 
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INTEGER N_PELC,N_PPHO,N_PMUO,N_PTAU,N_JET(4),N_VERT,MI_TOOL
      INTEGER MULTIPLE_INTERACTION_TOOL,POLARITY,STATION_HITS(1:6)
      INTEGER LGLOB,GZGLOB,LVCOR,KGLOB,LPLV0,GZPLV0,LMTRH,GZMTRH
      INTEGER LTTRH,GZTTRH,LVERH,GZVERH,TEMP,LFILT,LDTRH,GZDTRH
      INTEGER ID_GLOB,VRECO,PASS,I
      REAL    OUT(2)
      EXTERNAL MULTIPLE_INTERACTION_TOOL
C
      PARAMETER( KGLOB = 106 )
      CHARACTER*8 GLOB_TAGS(KGLOB)
      DATA GLOB_TAGS/
     &   'NPELC'   ,'NPPHO'   ,'NPMUO'   ,'NPTAU'   ,'NJET7'
     &  ,'NJET5'   ,'NJET3'   ,'NJETN'   ,'NVERTEX' ,'L1MET'   
     &  ,'L1METPHI','L1SUMET' ,'L2MET'   ,'L2METPHI','L2SUMET' 
     &  ,'MIFLAG'  ,'RECO'    ,'QUALITY_','QUALITY' ,'NCEMFH'  
     &  ,'EC_SUMET','CC_SUMET','CAL_SUME','ECS_SUME','CUP_SUME'
     &  ,'CDN_SUME','ECN_SUME','MR_SUME' ,'MR_SUMET','HOT_SUME'
     &  ,'HOT_ET'  ,'EVENT29' ,'MR_BITS' ,'VCORPX'  ,'VCORPY'  
     &  ,'MAGPOL'  ,'L0STAT_' ,'L0STAT'  ,'FASTZ'   ,'SLOWZ'   
     &  ,'SLOWZQ'  ,'SLOWERZ' ,'SLOWERZQ','MUSTP1_' ,'MUSTP1'  
     &  ,'MUSTP2_' ,'MUSTP2'  ,'MUSTP3_' ,'MUSTP3'  ,'MUSTP4_' 
     &  ,'MUSTP4'  ,'NHTTRD'  ,'SAMUSAN' ,'SAMUSBN' ,'SAMUSCN' 
     &  ,'SAMUSAS' ,'SAMUSBS' ,'SAMUSCS' ,'VERSION1','VERSION2'
     &  ,'VERSION3','ANDOR1_' ,'ANDOR1'  ,'ANDOR2_' ,'ANDOR2'  
     &  ,'ANDOR3_' ,'ANDOR3'  ,'ANDOR4_' ,'ANDOR4'  ,'ANDOR5_' 
     &  ,'ANDOR5'  ,'ANDOR6_' ,'ANDOR6'  ,'ANDOR7_' ,'ANDOR7' 
     &  ,'ANDOR8_' ,'ANDOR8'  ,'ENEG'    ,'ETNEG'   ,'L15M1_'
     &  ,'L15M1'   ,'L15M2_'  ,'L15M2'   ,'L15M3_'  ,'L15M3' 
     &  ,'VCORPX1' ,'VCORPY1' ,'CHNEGX'  ,'CHNEGY'  ,'PXOPT'
     &  ,'PYOPT'   ,'NCDCTK'  ,'VTSTAT'  ,'VHTRKW'  ,'XBM'     
     &  ,'YBM'     ,'ZBM'     ,'DXDZBM'  ,'DYDZBM'  ,'XBMERR'  
     &  ,'YBMERR'  ,'OLDZ'    ,'FZADCTN' ,'FZADCTS' ,'L2STAT'  
     &  ,'DTSTAT'
     &  /
      REAL  
     &   NPELC   ,NPPHO   ,NPMUO   ,NPTAU   ,NJET7
     &  ,NJET5   ,NJET3   ,NJETN   ,NVERTEX ,L1MET   
     &  ,L1METPHI,L1SUMET ,L2MET   ,L2METPHI,L2SUMET 
     &  ,MIFLAG  ,RECO    ,QUALITY_,QUALITY ,NCEMFH  
     &  ,EC_SUMET,CC_SUMET,CAL_SUME,ECS_SUME,CUP_SUME
     &  ,CDN_SUME,ECN_SUME,MR_SUME ,MR_SUMET,HOT_SUME
     &  ,HOT_ET  ,EVENT29 ,MR_BITS ,VCORPX  ,VCORPY  
     &  ,MAGPOL  ,L0STAT_ ,L0STAT  ,FASTZ   ,SLOWZ   
     &  ,SLOWZQ  ,SLOWERZ ,SLOWERZQ,MUSTP1_ ,MUSTP1  
     &  ,MUSTP2_ ,MUSTP2  ,MUSTP3_ ,MUSTP3  ,MUSTP4_ 
     &  ,MUSTP4  ,NHTTRD  ,SAMUSAN ,SAMUSBN ,SAMUSCN 
     &  ,SAMUSAS ,SAMUSBS ,SAMUSCS ,VERSION(3),ANDOR(2,8)
     &  ,ENEG    ,ETNEG   ,L15M1_  ,L15M1   ,L15M2_  
     &  ,L15M2   ,L15M3_  ,L15M3   ,VCORPX1 ,VCORPY1 
     &  ,CHNEGX  ,CHNEGY  ,PXOPT   ,PYOPT   ,NCDCTK  
     &  ,VTSTAT  ,VHTRKW  ,XBM     ,YBM     ,ZBM     
     &  ,DXDZBM  ,DYDZBM  ,XBMERR  ,YBMERR  ,OLDZ    
     &  ,FZADCTN ,FZADCTS ,L2STAT  ,DTSTAT
      COMMON/GLOB_OBJECT/
     &   NPELC   ,NPPHO   ,NPMUO   ,NPTAU   ,NJET7
     &  ,NJET5   ,NJET3   ,NJETN   ,NVERTEX ,L1MET   
     &  ,L1METPHI,L1SUMET ,L2MET   ,L2METPHI,L2SUMET 
     &  ,MIFLAG  ,RECO    ,QUALITY_,QUALITY ,NCEMFH  
     &  ,EC_SUMET,CC_SUMET,CAL_SUME,ECS_SUME,CUP_SUME
     &  ,CDN_SUME,ECN_SUME,MR_SUME ,MR_SUMET,HOT_SUME
     &  ,HOT_ET  ,EVENT29 ,MR_BITS ,VCORPX  ,VCORPY  
     &  ,MAGPOL  ,L0STAT_ ,L0STAT  ,FASTZ   ,SLOWZ   
     &  ,SLOWZQ  ,SLOWERZ ,SLOWERZQ,MUSTP1_ ,MUSTP1  
     &  ,MUSTP2_ ,MUSTP2  ,MUSTP3_ ,MUSTP3  ,MUSTP4_ 
     &  ,MUSTP4  ,NHTTRD  ,SAMUSAN ,SAMUSBN ,SAMUSCN 
     &  ,SAMUSAS ,SAMUSBS ,SAMUSCS ,VERSION ,ANDOR
     &  ,ENEG    ,ETNEG   ,L15M1_  ,L15M1   ,L15M2_  
     &  ,L15M2   ,L15M3_  ,L15M3   ,VCORPX1 ,VCORPY1 
     &  ,CHNEGX  ,CHNEGY  ,PXOPT   ,PYOPT   ,NCDCTK  
     &  ,VTSTAT  ,VHTRKW  ,XBM     ,YBM     ,ZBM     
     &  ,DXDZBM  ,DYDZBM  ,XBMERR  ,YBMERR  ,OLDZ    
     &  ,FZADCTN ,FZADCTS ,L2STAT  ,DTSTAT
      REAL    XDATA(KGLOB)
      EQUIVALENCE(XDATA,NPELC)
C----------------------------------------------------------------------
      CALL VZERO(XDATA,KGLOB)
      NPELC = FLOAT(N_PELC)            ! number of PELC banks
      NPPHO = FLOAT(N_PPHO)            ! number of PPHO banks
      NPMUO = FLOAT(N_PMUO)            ! number of PMUO banks
      NPTAU = FLOAT(N_PTAU)            ! number of PTAU banks
      NJET7 = FLOAT(N_JET(1))          ! number of cone 0.7 jets
      NJET5 = FLOAT(N_JET(2))          ! number of cone 0.5 jets
      NJET3 = FLOAT(N_JET(3))          ! number of cone 0.3 jets
      NJETN = FLOAT(N_JET(4))          ! number of NN-jets
      NVERTEX = FLOAT(N_VERT)          ! number of vertices
      CALL UDST_GET_L12_MET(L1MET,L1METPHI,L1SUMET,L2MET,L2METPHI,
     &  L2SUMET)
      MI_TOOL = MULTIPLE_INTERACTION_TOOL()
      MIFLAG = FLOAT(MI_TOOL)
C
      CALL RECO_VERSION(VRECO,PASS)
      RECO = FLOAT(VRECO*100+PASS)/100.   ! RECO version
C
      CALL GLOBFL(1)
      LGLOB=GZGLOB()
      IF (LGLOB.GT.0) THEN
        CALL SPLIT_BITMASK(IQ(LGLOB+2),OUT)
        QUALITY_ = OUT(1)
        QUALITY  = OUT(2)
        NCDCTK   = FLOAT(IQ(LGLOB+3))
        NCEMFH   = FLOAT(IQ(LGLOB+4))
        EC_SUMET = Q(LGLOB+6)
        CC_SUMET = Q(LGLOB+7)
        CAL_SUME = Q(LGLOB+8)
        ECS_SUME = Q(LGLOB+9)
        CUP_SUME = Q(LGLOB+10)
        CDN_SUME = Q(LGLOB+11)
        ECN_SUME = Q(LGLOB+12)
        MR_SUME  = Q(LGLOB+13)
        MR_SUMET = Q(LGLOB+14)
        HOT_SUME = Q(LGLOB+15)
        HOT_ET   = Q(LGLOB+16)
        EVENT29  = Q(LGLOB+17)
        MR_BITS  = FLOAT(IQ(LGLOB+18))
        DO I=1,8
          CALL SPLIT_BITMASK(IQ(LGLOB+20+I),OUT)
          ANDOR(1,I) = OUT(1)
          ANDOR(2,I) = OUT(2)
        ENDDO
        ENEG     = Q(LGLOB+29)
        ETNEG    = Q(LGLOB+30)
        CHNEGX   = Q(LGLOB+31)
        CHNEGY   = Q(LGLOB+32)
        CALL PTOPT(PXOPT,PYOPT)
C
        LVCOR=LQ(LGLOB-2)
        IF (LVCOR.GT.0) THEN
          VCORPX = Q(LVCOR+3)
          VCORPY = Q(LVCOR+4)
        ELSE
          VCORPX = 0.
          VCORPY = 0.
        ENDIF
        LVCOR=LQ(LGLOB-3)
        IF (LVCOR.GT.0) THEN
          VCORPX1 = Q(LVCOR+3)
          VCORPY1 = Q(LVCOR+4)
        ELSE
          VCORPX1 = 0.
          VCORPY1 = 0.
        ENDIF
      ENDIF
C
      CALL UDST_GET_MAGPOL(POLARITY)
      MAGPOL = FLOAT(POLARITY)
C
      LPLV0=GZPLV0()
      IF (LPLV0.GT.0) THEN
        CALL SPLIT_BITMASK(IQ(LPLV0+1),OUT)
        L0STAT_ = OUT(1)
        L0STAT  = OUT(2)
        FASTZ   = Q(LPLV0+2)
        SLOWZ   = Q(LPLV0+3)
        SLOWZQ  = Q(LPLV0+4)
        SLOWERZ = Q(LPLV0+5)
        SLOWERZQ= Q(LPLV0+6)
        FZADCTN = Q(LPLV0+16)
        FZADCTS = Q(LPLV0+17)
      ENDIF
C
      LMTRH=GZMTRH()
      IF (LMTRH.GT.0) THEN
        CALL SPLIT_BITMASK(Q(LMTRH+2),OUT)
        MUSTP1_ = OUT(1)
        MUSTP1  = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+3),OUT)
        MUSTP2_ = OUT(1)
        MUSTP2  = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+4),OUT)
        MUSTP3_ = OUT(1)
        MUSTP3  = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+5),OUT)
        MUSTP4_ = OUT(1)
        MUSTP4  = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+8),OUT)
        L15M1_  = OUT(1)
        L15M1   = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+9),OUT)
        L15M2_  = OUT(1)
        L15M2   = OUT(2)
        CALL SPLIT_BITMASK(Q(LMTRH+10),OUT)
        L15M3_  = OUT(1)
        L15M3   = OUT(2)
      ENDIF
C
      LTTRH=GZTTRH()
      IF (LTTRH.GT.0) THEN
        NHTTRD  = Q(LTTRH+2)
      ENDIF
C
      CALL UDST_GET_SAHH(STATION_HITS)
      SAMUSAN = FLOAT(STATION_HITS(1))
      SAMUSBN = FLOAT(STATION_HITS(2))
      SAMUSCN = FLOAT(STATION_HITS(3))
      SAMUSAS = FLOAT(STATION_HITS(4))
      SAMUSBS = FLOAT(STATION_HITS(5))
      SAMUSCS = FLOAT(STATION_HITS(6))

      LDTRH = GZDTRH()
      if (LDTRH.GT.0) THEN
        TEMP = IQ(LDTRH)
        CALL SPLIT_BITMASK(TEMP,OUT)
        DTSTAT =  OUT(2)
      ENDIF

      LVERH = GZVERH()
      IF (LVERH.GT.0) THEN
        TEMP = IQ(LVERH)
        CALL SPLIT_BITMASK(TEMP,OUT)
        VTSTAT =  OUT(2)        
        XBM    =  Q(LVERH + 4)
        YBM    =  Q(LVERH + 5)
        ZBM    =  Q(LVERH + 6)
        DXDZBM =  Q(LVERH + 7)
        DYDZBM =  Q(LVERH + 8)
        XBMERR =  Q(LVERH + 9)
        YBMERR =  Q(LVERH + 10)
        IF (IQ(LVERH+1).GE.3) THEN
          CALL SPLIT_BITMASK(IQ(LVERH+12),OUT)
          VHTRKW =  OUT(2)
          OLDZ   =  Q(LVERH + 11)
        ELSE
          OLDZ   =  0.0
        ENDIF          
      ELSE
        VTSTAT =  0.0
        XBM    =  0.0
        YBM    =  0.0
        ZBM    =  0.0
        DXDZBM =  0.0
        DYDZBM =  0.0
        XBMERR =  0.0
        YBMERR =  0.0
        OLDZ   =  0.0
        VHTRKW =  0.0
      ENDIF
C... get out-of-space/time bits from FILT bank
      LFILT = LQ(LHEAD-IZFILT)
      IF (LFILT.GT.0) THEN
        IF(IQ(LFILT+19).NE.0)THEN
          L2STAT = IQ(LFILT+19)+4*IQ(LFILT+20)
        ELSE
          L2STAT = 0.
        ENDIF
      ENDIF
C
      CALL UDST_GET_VERSION(VERSION)
C
      CALL UDST_FILL_GROUP(ID_GLOB,XDATA,KGLOB,1)
  999 RETURN
C
      ENTRY UDST_GLOB_TAGS
C----------------------------------------------------------------------
      ID_GLOB=1
      CALL UDST_BOOK_GROUP(ID_GLOB,'GLOB',GLOB_TAGS,KGLOB)
C----------------------------------------------------------------------
      RETURN
      END
