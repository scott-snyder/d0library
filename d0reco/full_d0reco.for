      SUBROUTINE PBDINI                                                         
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-    This routine must be called by the FrameWork to initialize                
C-    the run time switches associated with the packages                        
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL WRNGOK                                                            
      INTEGER LOGUNT,MAXLOG,MAXWRN                                              
      CHARACTER*32 STRGLG                                                       
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
C-                                                                              
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      LOGUNT = 0                                                                
      WRNGOK = .TRUE.                                                           
      CALL ERRINI(LOGUNT,WRNGOK)                                                
      MAXLOG = 1                                                                
      MAXWRN = 1                                                                
      STRGLG = ' '                                                              
      CALL ERRMAX(STRGLG,MAXLOG,MAXWRN)                                         
C-                                                                              
      PBD_FLAG_MAX = 16                                                         
C-                                                                              
      PBD_FLAG_VALUE( 1) = .TRUE.                                               
      PBD_FLAG_NAME( 1) = 'LEVEL0'                                              
      PBD_FLAG_VALUE( 2) = .TRUE.                                               
      PBD_FLAG_NAME( 2) = 'ZTRAKS'                                              
      PBD_FLAG_VALUE( 3) = .TRUE.                                               
      PBD_FLAG_NAME( 3) = 'T0D'                                                 
      PBD_FLAG_VALUE( 4) = .TRUE.                                               
      PBD_FLAG_NAME( 4) = 'CALOR'                                               
      PBD_FLAG_VALUE( 5) = .TRUE.                                               
      PBD_FLAG_NAME( 5) = 'CAHITS'                                              
      PBD_FLAG_VALUE( 6) = .TRUE.                                               
      PBD_FLAG_NAME( 6) = 'MURECO'                                              
      PBD_FLAG_VALUE( 7) = .TRUE.                                               
      PBD_FLAG_NAME( 7) = 'C3PMET'                                              
      PBD_FLAG_VALUE( 8) = .TRUE.                                               
      PBD_FLAG_NAME( 8) = 'CAJETS'                                              
      PBD_FLAG_VALUE( 9) = .TRUE.                                               
      PBD_FLAG_NAME( 9) = 'CAPHEL'                                              
      PBD_FLAG_VALUE(10) = .TRUE.                                               
      PBD_FLAG_NAME(10) = 'HMATRIX'                                             
      PBD_FLAG_VALUE(11) = .TRUE.                                               
      PBD_FLAG_NAME(11) = 'L2C2EM'                                              
      PBD_FLAG_VALUE(12) = .TRUE.                                               
      PBD_FLAG_NAME(12) = 'CTAUS'                                               
      PBD_FLAG_VALUE(13) = .TRUE.                                               
      PBD_FLAG_NAME(13) = 'CAJNEP'                                              
      PBD_FLAG_VALUE(14) = .TRUE.                                               
      PBD_FLAG_NAME(14) = 'CAFIX'                                               
      PBD_FLAG_VALUE(15) = .TRUE.                                               
      PBD_FLAG_NAME(15) = 'CATD'                                                
      PBD_FLAG_VALUE(16) = .TRUE.                                               
      PBD_FLAG_NAME(16) = 'RECO_ESUM'                                           
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      SUBROUTINE INZCOM(I)                                                      
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods :                                                      
C-       Initialize ZEBCOM (event data Zebra common)                            
C-                                                                              
C-   Inputs  : I = 1 data in division 1, otherwise in division 2                
C-   Outputs : NONE                                                             
C-                                                                              
C-   Created  28-OCT-1988   Serban D. Protopopescu                              
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
C-                                                                              
C     INCLUDE 'D0$INC:ZEBCOM.INC'                                               
C-                                                                              
C-                                                                              
C                                                                               
C  ZEBCOM is the main zebra common block for event data storage                 
C                                                                               
      INTEGER NNQ,NREF                                                          
C     PARAMETER (NNQ=200000)                                                    
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NNQ=3000000 )                                                  
C-                                                                              
      PARAMETER (NREF=9)                                                        
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,                 
     &  ZSTOR,ENDZS                                                             
      INTEGER IXCOM    ! store number                                           
     &       ,IXMAIN   ! event division number                                  
     &       ,IXDVR    ! run division number                                    
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS                              
      INTEGER LHEAD     ! pointer to event HEAD bank                            
      INTEGER LHEADR    ! pointer to begin run HEAD bank                        
      REAL Q(NNQ)                                                               
      INTEGER IQ(NNQ),LQ(NNQ)                                                   
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))                              
C                                                                               
C-                                                                              
C-                                                                              
      INTEGER I                                                                 
      LOGICAL FIRST                                                             
      SAVE FIRST                                                                
      DATA FIRST/.TRUE./                                                        
C----------------------------------------------------------------------         
C                                                                               
      IF(FIRST) THEN                                                            
C                                                                               
C **** Initialize store in /ZEBCOM/ common (store 0)                            
C                                                                               
        IXCOM=0                                                                 
        CALL MZSTOR (IXCOM,'/ZEBCOM/','Q',FENCE,LHEAD,LREF(1),ZSTOR(1),         
     &   ZSTOR(40000),ENDZS)                                                    
C                                                                               
C **** Use division IXMAIN for event data                                       
C                                                                               
        IXMAIN=IXCOM+2                                                          
        IF(I.EQ.1) IXMAIN=IXCOM+1                                               
C                                                                               
C **** Create a division for run header (3rd division)                          
C                                                                               
        CALL MZDIV(IXCOM,IXDVR,'RUN DIV',100,40000,'L')                         
        FIRST=.FALSE.                                                           
      ENDIF                                                                     
  999 RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      SUBROUTINE INZSTP                                                         
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods :                                                      
C-       Initialize ZEBSTP (Zebra common for static parameters)                 
C-                                                                              
C-   Created  28-OCT-1988   Serban D. Protopopescu                              
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
C-                                                                              
C     INCLUDE 'D0$INC:ZEBSTP.INC'                                               
C-                                                                              
C-                                                                              
C ZEBSTP.INC                                                                    
      INTEGER NNC                                                               
C     PARAMETER (NNC=200000)                                                    
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NNC=4200000 )                                                  
C-                                                                              
      COMMON/ZEBSTP/IXSTP,IDVSTP,IDVSUM,FENSTP,LSTPH,LZSUM,                     
     1 LSLV0,LLPDH,LLGNH,LLTMH,LLGEH,LLV,               ! level 0 links         
     2 LSMUO,LMPDH,LMGNH,LMTMH,LMGEH,LMU,               ! muon links            
     3 LSVTX,LVPDH,LVGNH,LVTMH,LVGEH,LBMXY,LVT,         ! vertex chamber links  
     4 LSCDC,LDPDH,LDGNH,LDTMH,LDGEH,LDALH,LDC,         ! cdc links             
     5 LSTRD,LTPDH,LTGAI,LTGEN,LTGEO,LTCAN,LTLIK,LTR,   ! trd links             
     6 LSFDC,LFPDH,LFGNH,LFTMH,LFGEH,LFD,               ! fdc links             
     7 LSCAL,LCPDH,LCGNH,LCGEH,LCA,                     ! calorimeter links     
     8 ZCONS,ENDZC                                                              
      INTEGER IXSTP,IDVSTP,IDVSUM,FENSTP(10),LZSUM,ZCONS(NNC),ENDZC             
      REAL C(NNC)                                                               
      INTEGER IC(NNC),LC(NNC)                                                   
      EQUIVALENCE (LSTPH,LC(1)),(LC(9),IC(1),C(1))                              
      INTEGER LSTPH ! static parameter header                                   
     $,  LSLV0      ! level 0 constants header                                  
     $,  LLPDH      ! level 0 pedestal header                                   
     $,  LLGNH      ! level 0 gain header                                       
     $,  LLTMH      ! level 0 time constants header                             
     $,  LLGEH      ! level 0 geometry constants header                         
     $,  LLV(10)    ! available                                                 
C                                                                               
      INTEGER LSMUO ! Muon constants header                                     
     $,  LMPDH      ! Muon pedestal header                                      
     $,  LMGNH      ! Muon gain header                                          
     $,  LMTMH      ! Muon time constants header                                
     $,  LMGEH      ! Muon geometry constants header                            
     $,  LMU(10)    ! available                                                 
C                                                                               
C LBMXY added NGraf 8/16/94                                                     
      INTEGER LSVTX ! Vertex chamber constants header                           
     $,  LVPDH      ! Vertex chamber pedestal header                            
     $,  LVGNH      ! Vertex chamber gain header                                
     $,  LVTMH      ! Vertex chamber time constants header                      
     $,  LVGEH      ! Vertex chamber geometry constants header                  
     $,  LBMXY      ! Beam position bank (does NOT hang from SVTX)              
     $,  LVT(9)     ! available                                                 
C                                                                               
      INTEGER LSCDC ! CDC constants header                                      
     $,  LDPDH      ! CDC pedestal header                                       
     $,  LDGNH      ! CDC gain header                                           
     $,  LDTMH      ! CDC time constants header                                 
     $,  LDGEH      ! CDC geometry constants header                             
     $,  LDALH      ! CDC wire alignment constants header                       
     $,  LDC(9)     ! available                                                 
C                                                                               
C LTCAN and LTLIK add Azlyber 8/1/94                                            
      INTEGER LSTRD ! TRD constants header                                      
     $,  LTPDH      ! TRD pedestal header                                       
     $,  LTGAI      ! TRD gains header                                          
     $,  LTGEN      ! TRD general header                                        
     $,  LTGEO      ! TRD geometry constants header                             
     $,  LTCAN      ! TRD canary header                                         
     $,  LTLIK      ! TRD header for JFL's likelihood tables                    
     $,  LTR(8)     ! available                                                 
C                                                                               
      INTEGER LSFDC ! FDC constants header                                      
     $,  LFPDH      ! FDC pedestal header                                       
     $,  LFGNH      ! FDC gain header                                           
     $,  LFTMH      ! FDC time constants header                                 
     $,  LFGEH      ! FDC geometry constants header                             
     $,  LFD(10)    ! available                                                 
C                                                                               
      INTEGER LSCAL ! Calorimeter constants header                              
     $,  LCPDH      ! Calorimeter pedestal header                               
     $,  LCGNH      ! Calorimeter gain header                                   
     $,  LCGEH      ! Calorimeter geometry constants header                     
     $,  LCA(11)    ! available                                                 
C                                                                               
C-                                                                              
C-                                                                              
      LOGICAL FIRST                                                             
      SAVE FIRST                                                                
      DATA FIRST/.TRUE./                                                        
C----------------------------------------------------------------------         
C                                                                               
      IF(FIRST) THEN                                                            
C                                                                               
C          Note that 10 words are set aside for reference links                 
C          but are not specifically named in the common block                   
C                                                                               
        CALL MZSTOR (IXSTP,'/ZEBSTP/','C',FENSTP,LSTPH,ZCONS,ZCONS(10),         
     &   ZCONS(10000),ENDZC)                                                    
C                                                                               
C **** IDVSTP is the 2nd division in the ZEBSTP store.                          
C                                                                               
        IDVSTP=IXSTP+2                                                          
        CALL CONSTP                 ! construct STP headers                     
        FIRST=.FALSE.                                                           
      ENDIF                                                                     
  999 RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      SUBROUTINE INZWRK                                                         
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods :                                                      
C-      Initialize /ZEBWRK/, working area not written out                       
C-                                                                              
C-   Created  28-OCT-1988   Serban D. Protopopescu                              
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
C-                                                                              
C     INCLUDE 'D0$INC:ZEBWRK.INC'                                               
C-                                                                              
C-                                                                              
C                                                                               
C--     Zebra working space; Banks WRKH; STEP; ADCS;                            
C                                                                               
      INTEGER NNW,NWR                                                           
C     PARAMETER (NNW=30000)                                                     
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NNW=80000 )                                                    
C-                                                                              
      PARAMETER (NWR=9)                                                         
      COMMON/ZEBWRK/IXWRK,IDVWRK,FENWRK,LWRKH,LRWRK,LSTEP,                      
     2 ZWORK,ENDZW                                                              
C                                                                               
      INTEGER IXWRK,IDVWRK,LSTEP,FENWRK(10),ZWORK(NNW),ENDZW                    
      REAL W(NNW)                                                               
      INTEGER IW(NNW),LW(NNW)                                                   
      INTEGER*2 KW(2*NNW)                                                       
      EQUIVALENCE (LWRKH,LW(1)),(LW(9),IW(1),W(1),KW(1))                        
      INTEGER LWRKH               ! working space header                        
     $,  LRWRK(NWR)                                                             
C                                                                               
C-                                                                              
C-                                                                              
      LOGICAL FIRST                                                             
      SAVE FIRST                                                                
      DATA FIRST/.TRUE./                                                        
C----------------------------------------------------------------------         
C                                                                               
      IF(FIRST) THEN                                                            
        CALL MZSTOR (IXWRK,'/ZEBWRK/','Q',FENWRK,LWRKH,LRWRK,ZWORK(1),          
     &   ZWORK(10000),ENDZW)                                                    
        IDVWRK=IXWRK+2                                                          
        FIRST=.FALSE.                                                           
      ENDIF                                                                     
  999 RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      SUBROUTINE INPAWC                                                         
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods :                                                      
C-       Initialize PAWC common for HBOOK4                                      
C-                                                                              
C-   Created  27-MAR-1989   Serban D. Protopopescu                              
C-   Updated  17-APR-1991   Rajendran Raja  Updated to have Global sections     
C-   Updated   3-MAR-1992   K. Wyatt Merritt  Put global sections in machine    
C-                           block for unix compatibility                       
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
C-                                                                              
C     INCLUDE 'D0$INC:PAWC.INC'                                                 
C-                                                                              
C-                                                                              
C----------------------------------------------------------------------         
C-                                                                              
C-   Created  16-MAY-1989   Serban D. Protopopescu                              
C-   Updated   1-JUN-1991   Rajendran Raja                                      
C-                                                                              
C----------------------------------------------------------------------         
      INTEGER NPAWC                                                             
C     PARAMETER (NPAWC=100000)                                                  
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NPAWC=100000 )                                                 
C-                                                                              
      INTEGER PAGELEN                                                           
      PARAMETER( PAGELEN = 128 )        ! LENGTH OF PAGE                        
      INTEGER MAXPAGES                                                          
      PARAMETER( MAXPAGES = NPAWC/PAGELEN )                                     
      CHARACTER*32 GNAME                                                        
C                                                                               
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LPMAIN,                
     &  HCV(9989)                                                               
C                                                                               
      REAL HMEMOR(PAGELEN*MAXPAGES)                                             
      INTEGER NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC,LPMAIN,HCV                    
      EQUIVALENCE(NWPAW,HMEMOR(1))                                              
      INTEGER IP(2),LP(8000)                                                    
      REAL    P(2)                                                              
      EQUIVALENCE (LP(1),LPMAIN),(IP(1),LP(9)),(P(1),IP(1))                     
C-                                                                              
C-                                                                              
      INTEGER NPAGES,HCREATEG                                                   
      INTEGER TRNLNM,STATUS,LENG                                                
      INTEGER TRULEN                                                            
C----------------------------------------------------------------------         
C                                                                               
      STATUS = TRNLNM('GLOBAL$PAWC',GNAME,LENG)                                 
      IF ( GNAME(1:LENG).EQ.'GLOBAL$PAWC' ) THEN          ! NO GLOBAL SECTION   
        CALL HLIMIT(-PAGELEN*MAXPAGES)                                          
      ELSE                                                                      
C&IF VAXVMS                                                                     
        NPAGES = HCREATEG(GNAME(1:LENG),HMEMOR,PAGELEN*MAXPAGES)                
        IF ( NPAGES.GT.0 ) THEN                                                 
          CALL ERRMSG(' INPAWC ','INPAWC',                                      
     &    ' GLOBAL SECTION: '//GNAME(1:LENG)//' CREATED ','S')                  
          CALL HLIMIT(-PAGELEN*MAXPAGES)                                        
        ELSE                                                                    
          CALL ERRMSG(' INPAWC ','INPAWC ',                                     
     &    ' GLOBAL SECTION ERROR ','S')                                         
        ENDIF                                                                   
C&ELSE                                                                          
C&        CALL HLIMIT(-PAGELEN*MAXPAGES)                                        
C&        CALL ERRMSG(' INPAWC ','INPAWC ',                                     
C&     &    'Global section not created on non-VMS system','S')                 
C&ENDIF                                                                         
      ENDIF                                                                     
C                                                                               
  999 RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      SUBROUTINE INZGCB                                                         
C----------------------------------------------------------------------         
C-                                                                              
C-   Purpose and Methods : Init GCBANK (Geant Data Store)                       
C-                                                                              
C-   Inputs  :                                                                  
C-   Outputs :                                                                  
C-   Controls:                                                                  
C-                                                                              
C-   Created  24-NOV-1989   Alan M. Jonckheere                                  
C-                                                                              
C----------------------------------------------------------------------         
      IMPLICIT NONE                                                             
C-                                                                              
C     INCLUDE 'D0$INC:GCBANK.INC'                                               
C-                                                                              
C-                                                                              
C----------------------------------------------------------------------         
C-                                                                              
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14                     
C-                                                                              
C----------------------------------------------------------------------         
      INTEGER NGCBNK                                                            
C     PARAMETER( NGCBNK = 69000 )                                               
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER( NGCBNK = 250000 )                                              
C-                                                                              
      INTEGER KWBANK,KWWORK                                                     
      PARAMETER (KWBANK=NGCBNK,KWWORK=5200)                                     
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)          
     +             ,LMAIN,LR1,WS(KWBANK)                                        
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))               
c                                                                               
      INTEGER IQ(2),LQ(8000),IWS(2)                                             
      INTEGER NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1                              
      REAL GVERSN,ZVERSN,FENDQ,WS,Q(1)                                          
C                                                                               
C-                                                                              
C-                                                                              
      LOGICAL FIRST                                                             
      DATA    FIRST/.TRUE./                                                     
C----------------------------------------------------------------------         
      IF ( FIRST ) THEN                                                         
        CALL GZEBRA(NGCBNK)                                                     
      ENDIF                                                                     
  999 RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION STSWCH()                                                 
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-    This routine allows the Program Builder user to set                       
C-    and reset(ON/OFF) the run time switches associated                        
C-    with the packages.                                                        
C-    Created by the PROGRAM BUILDER                                            
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
C-    A dummy routine becuase no SWITCH
C-    input qualifier specified
C-                                                                              
      STSWCH = .TRUE.                                                           
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION PBD_SET_FLAG(FLAG_NAME,VALUE)                            
C-                                                                              
      IMPLICIT NONE                                                             
C-                                                                              
      CHARACTER*(*) FLAG_NAME                                                   
      CHARACTER*80 TMP_FLAG_NAME                                                
      INTEGER I                                                                 
      LOGICAL DONE,VALUE                                                        
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
C-                                                                              
      I = 1                                                                     
      DONE = .FALSE.                                                            
      CALL PBD_UPCASE(FLAG_NAME,TMP_FLAG_NAME)                                  
      DO WHILE ((I.LE.PBD_FLAG_MAX).AND.(.NOT.DONE))                            
        IF (TMP_FLAG_NAME(1:LEN(FLAG_NAME)).EQ.                                 
     &      PBD_FLAG_NAME(I)(1:LEN(FLAG_NAME))) THEN                            
          PBD_FLAG_VALUE(i) = VALUE                                             
          DONE = .TRUE.                                                         
        END IF                                                                  
        I = I + 1                                                               
      END DO                                                                    
      PBD_SET_FLAG = DONE                                                       
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION PBD_GET_FLAG(FLAG_NAME,VALUE)                            
C-                                                                              
      IMPLICIT NONE                                                             
C-                                                                              
      CHARACTER*(*) FLAG_NAME                                                   
      CHARACTER*80 TMP_FLAG_NAME                                                
      INTEGER I                                                                 
      LOGICAL DONE,VALUE                                                        
C-                                                                              
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      I = 1                                                                     
      DONE = .FALSE.                                                            
      CALL PBD_UPCASE(FLAG_NAME,TMP_FLAG_NAME)                                  
      DO WHILE ((I.LE.PBD_FLAG_MAX).AND.(.NOT.DONE))                            
        IF (TMP_FLAG_NAME(1:LEN(FLAG_NAME)).EQ.                                 
     &      PBD_FLAG_NAME(I)(1:LEN(FLAG_NAME))) THEN                            
          VALUE = PBD_FLAG_VALUE(I)                                             
          DONE = .TRUE.                                                         
        END IF                                                                  
        I = I + 1                                                               
      END DO                                                                    
      PBD_GET_FLAG = DONE                                                       
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      SUBROUTINE PBD_DUMP_FLAGS                                                 
C-                                                                              
      IMPLICIT NONE                                                             
C-                                                                              
      INTEGER I                                                                 
C-                                                                              
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
      DO I = 1, PBD_FLAG_MAX                                                    
        WRITE (6,'('' '',A,'' : '',L1)') PBD_FLAG_NAME(I)(1:32),                
     &PBD_FLAG_VALUE(I)                                                         
      END DO                                                                    
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      SUBROUTINE PBD_UPCASE(IN_STRING,OUT_STRING)                               
      IMPLICIT NONE                                                             
      CHARACTER*(*) IN_STRING,OUT_STRING                                        
      INTEGER I,OFFSET                                                          
C-                                                                              
C-                                                                              
C-                                                                              
      OFFSET = ICHAR('A') - ICHAR('a')                                          
      DO I = 1, LEN(IN_STRING)                                                  
        IF ((IN_STRING(I:I).GE.'a').AND.(IN_STRING(I:I).LE.'z')) THEN           
          OUT_STRING(I:I) = CHAR(ICHAR(IN_STRING(I:I)) + OFFSET)                
        ELSE                                                                    
          OUT_STRING(I:I) = IN_STRING(I:I)                                      
        END IF                                                                  
      END DO                                                                    
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      SUBROUTINE PBD_GET_FLAG_NAME(FLAG_ID,FLAG_NAME,FLAG_VALUE)                
C-                                                                              
      IMPLICIT NONE                                                             
C-                                                                              
      INTEGER FLAG_ID                                                           
      CHARACTER*(*) FLAG_NAME                                                   
      LOGICAL FLAG_VALUE                                                        
C-                                                                              
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      IF ( FLAG_ID .LE. PBD_FLAG_MAX ) THEN                                     
        FLAG_NAME = PBD_FLAG_NAME(FLAG_ID)                                      
        FLAG_VALUE = PBD_FLAG_VALUE(FLAG_ID)                                    
      ELSE                                                                      
        FLAG_NAME = ' '                                                         
        FLAG_ID = 0                                                             
        FLAG_VALUE = .FALSE.                                                    
      END IF                                                                    
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION EVENT_READ_OK()                                          
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      EVENT_READ_OK = .TRUE.                                                    
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: ZTRAKS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MURECO                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION EVENT_RECO_PBD()                                         
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL LV0EVT                                                            
      EXTERNAL LV0EVT                                                           
      LOGICAL ZTREVT                                                            
      EXTERNAL ZTREVT                                                           
      LOGICAL T0DEVT                                                            
      EXTERNAL T0DEVT                                                           
      LOGICAL CAHITS                                                            
      EXTERNAL CAHITS                                                           
      LOGICAL MURECO_EVT                                                        
      EXTERNAL MURECO_EVT                                                       
      LOGICAL C3PMET                                                            
      EXTERNAL C3PMET                                                           
      LOGICAL CAJETS                                                            
      EXTERNAL CAJETS                                                           
      LOGICAL CAPHEL                                                            
      EXTERNAL CAPHEL                                                           
      LOGICAL L2C2EM                                                            
      EXTERNAL L2C2EM                                                           
      LOGICAL CTAUS                                                             
      EXTERNAL CTAUS                                                            
      LOGICAL CAJNEP_EVT                                                        
      EXTERNAL CAJNEP_EVT                                                       
      LOGICAL CAFIX                                                             
      EXTERNAL CAFIX                                                            
      LOGICAL CATD_EVT                                                          
      EXTERNAL CATD_EVT                                                         
      LOGICAL RECO_HSUM_FILL                                                    
      EXTERNAL RECO_HSUM_FILL                                                   
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      EVENT_RECO_PBD = .TRUE.                                                   
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 1))                                                 
     &    ) THEN                                                                
         IF (.NOT. LV0EVT()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!LV0EVT is false.'                                        
            CALLER = 'LV0EVT'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTREVT()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!ZTREVT is false.'                                        
            CALLER = 'ZTREVT'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 3))                                                 
     &    ) THEN                                                                
         IF (.NOT. T0DEVT()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!T0DEVT is false.'                                        
            CALLER = 'T0DEVT'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 5))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAHITS()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CAHITS is false.'                                        
            CALLER = 'CAHITS'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MURECO_EVT()) THEN                                           
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!MURECO_EVT is false.'                                    
            CALLER = 'MURECO_EVT'                                               
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 7))                                                 
     &    ) THEN                                                                
         IF (.NOT. C3PMET()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!C3PMET is false.'                                        
            CALLER = 'C3PMET'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 8))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAJETS()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CAJETS is false.'                                        
            CALLER = 'CAJETS'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 9))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAPHEL()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CAPHEL is false.'                                        
            CALLER = 'CAPHEL'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(11))                                                 
     &    ) THEN                                                                
         IF (.NOT. L2C2EM()) THEN                                               
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!L2C2EM is false.'                                        
            CALLER = 'L2C2EM'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(12))                                                 
     &    ) THEN                                                                
         IF (.NOT. CTAUS()) THEN                                                
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CTAUS is false.'                                         
            CALLER = 'CTAUS'                                                    
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(13))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAJNEP_EVT()) THEN                                           
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CAJNEP_EVT is false.'                                    
            CALLER = 'CAJNEP_EVT'                                               
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(14))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAFIX()) THEN                                                
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CAFIX is false.'                                         
            CALLER = 'CAFIX'                                                    
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(15))                                                 
     &    ) THEN                                                                
         IF (.NOT. CATD_EVT()) THEN                                             
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!CATD_EVT is false.'                                      
            CALLER = 'CATD_EVT'                                                 
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(16))                                                 
     &    ) THEN                                                                
         IF (.NOT. RECO_HSUM_FILL()) THEN                                       
            EVENT_RECO_PBD = .FALSE.                                            
            MESSID = '!RECO_HSUM_FILL is false.'                                
            CALLER = 'RECO_HSUM_FILL'                                           
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION INITIALIZE_RECO_PBD()                                    
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL LV0INI                                                            
      EXTERNAL LV0INI                                                           
      LOGICAL ZTRINI                                                            
      EXTERNAL ZTRINI                                                           
      LOGICAL T0DINI                                                            
      EXTERNAL T0DINI                                                           
      LOGICAL CALOR_INI                                                         
      EXTERNAL CALOR_INI                                                        
      LOGICAL CHTINI                                                            
      EXTERNAL CHTINI                                                           
      LOGICAL MURECO_INI                                                        
      EXTERNAL MURECO_INI                                                       
      LOGICAL C3PMET_INI                                                        
      EXTERNAL C3PMET_INI                                                       
      LOGICAL CJTINI                                                            
      EXTERNAL CJTINI                                                           
      LOGICAL CPHINI                                                            
      EXTERNAL CPHINI                                                           
      LOGICAL HMATRIX_INI                                                       
      EXTERNAL HMATRIX_INI                                                      
      LOGICAL CTAUS_INI                                                         
      EXTERNAL CTAUS_INI                                                        
      LOGICAL CAJNEP_INI                                                        
      EXTERNAL CAJNEP_INI                                                       
      LOGICAL CAFIX_BEGIN                                                       
      EXTERNAL CAFIX_BEGIN                                                      
      LOGICAL CATD_INI                                                          
      EXTERNAL CATD_INI                                                         
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      INITIALIZE_RECO_PBD = .TRUE.                                              
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 1))                                                 
     &    ) THEN                                                                
         IF (.NOT. LV0INI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!LV0INI is false.'                                        
            CALLER = 'LV0INI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTRINI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!ZTRINI is false.'                                        
            CALLER = 'ZTRINI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 3))                                                 
     &    ) THEN                                                                
         IF (.NOT. T0DINI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!T0DINI is false.'                                        
            CALLER = 'T0DINI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 4))                                                 
     &    ) THEN                                                                
         IF (.NOT. CALOR_INI()) THEN                                            
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CALOR_INI is false.'                                     
            CALLER = 'CALOR_INI'                                                
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 5))                                                 
     &    ) THEN                                                                
         IF (.NOT. CHTINI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CHTINI is false.'                                        
            CALLER = 'CHTINI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MURECO_INI()) THEN                                           
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!MURECO_INI is false.'                                    
            CALLER = 'MURECO_INI'                                               
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 7))                                                 
     &    ) THEN                                                                
         IF (.NOT. C3PMET_INI()) THEN                                           
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!C3PMET_INI is false.'                                    
            CALLER = 'C3PMET_INI'                                               
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 8))                                                 
     &    ) THEN                                                                
         IF (.NOT. CJTINI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CJTINI is false.'                                        
            CALLER = 'CJTINI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 9))                                                 
     &    ) THEN                                                                
         IF (.NOT. CPHINI()) THEN                                               
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CPHINI is false.'                                        
            CALLER = 'CPHINI'                                                   
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(10))                                                 
     &    ) THEN                                                                
         IF (.NOT. HMATRIX_INI()) THEN                                          
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!HMATRIX_INI is false.'                                   
            CALLER = 'HMATRIX_INI'                                              
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(12))                                                 
     &    ) THEN                                                                
         IF (.NOT. CTAUS_INI()) THEN                                            
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CTAUS_INI is false.'                                     
            CALLER = 'CTAUS_INI'                                                
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(13))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAJNEP_INI()) THEN                                           
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CAJNEP_INI is false.'                                    
            CALLER = 'CAJNEP_INI'                                               
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(14))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAFIX_BEGIN()) THEN                                          
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CAFIX_BEGIN is false.'                                   
            CALLER = 'CAFIX_BEGIN'                                              
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(15))                                                 
     &    ) THEN                                                                
         IF (.NOT. CATD_INI()) THEN                                             
            INITIALIZE_RECO_PBD = .FALSE.                                       
            MESSID = '!CATD_INI is false.'                                      
            CALLER = 'CATD_INI'                                                 
            MESSAG = 'Program execution is aborted'                             
            CALL ERRMSG(MESSID,CALLER,MESSAG,'F')                               
C-                                                                              
C-          ERRMSG will trigger error messages printing                         
C-          and will abort the execution.                                       
C-          The following RETURN statement is only there                        
C-          to satisfy the FORTRAN compiler.                                    
C-                                                                              
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION NEW_RECO_RUN_PBD()                                       
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZTRPAR                                                            
      EXTERNAL ZTRPAR                                                           
      LOGICAL T0DPAR                                                            
      EXTERNAL T0DPAR                                                           
      LOGICAL CHTRUN                                                            
      EXTERNAL CHTRUN                                                           
      LOGICAL MUOPAR                                                            
      EXTERNAL MUOPAR                                                           
      LOGICAL RECO_HSUM_RUN                                                     
      EXTERNAL RECO_HSUM_RUN                                                    
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      NEW_RECO_RUN_PBD = .TRUE.                                                 
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTRPAR()) THEN                                               
            NEW_RECO_RUN_PBD = .FALSE.                                          
            MESSID = '!ZTRPAR is false.'                                        
            CALLER = 'ZTRPAR'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 3))                                                 
     &    ) THEN                                                                
         IF (.NOT. T0DPAR()) THEN                                               
            NEW_RECO_RUN_PBD = .FALSE.                                          
            MESSID = '!T0DPAR is false.'                                        
            CALLER = 'T0DPAR'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 5))                                                 
     &    ) THEN                                                                
         IF (.NOT. CHTRUN()) THEN                                               
            NEW_RECO_RUN_PBD = .FALSE.                                          
            MESSID = '!CHTRUN is false.'                                        
            CALLER = 'CHTRUN'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MUOPAR()) THEN                                               
            NEW_RECO_RUN_PBD = .FALSE.                                          
            MESSID = '!MUOPAR is false.'                                        
            CALLER = 'MUOPAR'                                                   
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(16))                                                 
     &    ) THEN                                                                
         IF (.NOT. RECO_HSUM_RUN()) THEN                                        
            NEW_RECO_RUN_PBD = .FALSE.                                          
            MESSID = '!RECO_HSUM_RUN is false.'                                 
            CALLER = 'RECO_HSUM_RUN'                                            
            MESSAG = 'Further processing is skipped'                            
            CALL ERRMSG(MESSID,CALLER,MESSAG,'W')                               
            RETURN                                                              
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION RECO_RUN_SUMMARY_PBD()                                   
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZTENDR                                                            
      EXTERNAL ZTENDR                                                           
      LOGICAL MUENDR                                                            
      EXTERNAL MUENDR                                                           
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      RECO_RUN_SUMMARY_PBD = .TRUE.                                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTENDR()) THEN                                               
            RECO_RUN_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'ZTENDR'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MUENDR()) THEN                                               
            RECO_RUN_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'MUENDR'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION RESET_RECO_POINTERS_PBD()                                
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZCLEAN                                                            
      EXTERNAL ZCLEAN                                                           
      LOGICAL CALEVZ                                                            
      EXTERNAL CALEVZ                                                           
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      RESET_RECO_POINTERS_PBD = .TRUE.                                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZCLEAN()) THEN                                               
            RESET_RECO_POINTERS_PBD = .FALSE.                                   
            CALLER = 'ZCLEAN'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 4))                                                 
     &    ) THEN                                                                
         IF (.NOT. CALEVZ()) THEN                                               
            RESET_RECO_POINTERS_PBD = .FALSE.                                   
            CALLER = 'CALEVZ'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MURECO                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION RECO_JOB_SUMMARY_PBD()                                   
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZTR_SUMMARY                                                       
      EXTERNAL ZTR_SUMMARY                                                      
      LOGICAL CHTFIN                                                            
      EXTERNAL CHTFIN                                                           
      LOGICAL C3PMET_FIN                                                        
      EXTERNAL C3PMET_FIN                                                       
      LOGICAL CJTFIN                                                            
      EXTERNAL CJTFIN                                                           
      LOGICAL CPHFIN                                                            
      EXTERNAL CPHFIN                                                           
      LOGICAL HMATRIX_FIN                                                       
      EXTERNAL HMATRIX_FIN                                                      
      LOGICAL CAJNEP_FIN                                                        
      EXTERNAL CAJNEP_FIN                                                       
      LOGICAL CAFIX_END                                                         
      EXTERNAL CAFIX_END                                                        
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      RECO_JOB_SUMMARY_PBD = .TRUE.                                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTR_SUMMARY()) THEN                                          
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'ZTR_SUMMARY'                                              
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 5))                                                 
     &    ) THEN                                                                
         IF (.NOT. CHTFIN()) THEN                                               
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'CHTFIN'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: MURECO                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 7))                                                 
     &    ) THEN                                                                
         IF (.NOT. C3PMET_FIN()) THEN                                           
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'C3PMET_FIN'                                               
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 8))                                                 
     &    ) THEN                                                                
         IF (.NOT. CJTFIN()) THEN                                               
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'CJTFIN'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 9))                                                 
     &    ) THEN                                                                
         IF (.NOT. CPHFIN()) THEN                                               
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'CPHFIN'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(10))                                                 
     &    ) THEN                                                                
         IF (.NOT. HMATRIX_FIN()) THEN                                          
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'HMATRIX_FIN'                                              
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(13))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAJNEP_FIN()) THEN                                           
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'CAJNEP_FIN'                                               
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(14))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAFIX_END()) THEN                                            
            RECO_JOB_SUMMARY_PBD = .FALSE.                                      
            CALLER = 'CAFIX_END'                                                
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION DMPUSR()                                                 
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZTRDMP                                                            
      EXTERNAL ZTRDMP                                                           
      LOGICAL CALDMP                                                            
      EXTERNAL CALDMP                                                           
      LOGICAL MURECO_DUMP                                                       
      EXTERNAL MURECO_DUMP                                                      
      LOGICAL RECO_HSUM_DUMP                                                    
      EXTERNAL RECO_HSUM_DUMP                                                   
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      DMPUSR = .TRUE.                                                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZTRDMP()) THEN                                               
            DMPUSR = .FALSE.                                                    
            CALLER = 'ZTRDMP'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 4))                                                 
     &    ) THEN                                                                
         IF (.NOT. CALDMP()) THEN                                               
            DMPUSR = .FALSE.                                                    
            CALLER = 'CALDMP'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MURECO_DUMP()) THEN                                          
            DMPUSR = .FALSE.                                                    
            CALLER = 'MURECO_DUMP'                                              
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(16))                                                 
     &    ) THEN                                                                
         IF (.NOT. RECO_HSUM_DUMP()) THEN                                       
            DMPUSR = .FALSE.                                                    
            CALLER = 'RECO_HSUM_DUMP'                                           
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION EVT_DSTDROP()                                            
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL ZDROP_TRACKS                                                      
      EXTERNAL ZDROP_TRACKS                                                     
      LOGICAL MUDROP_DST                                                        
      EXTERNAL MUDROP_DST                                                       
      LOGICAL CAJETS_DROP_DST                                                   
      EXTERNAL CAJETS_DROP_DST                                                  
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      EVT_DSTDROP = .TRUE.                                                      
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. ZDROP_TRACKS()) THEN                                         
            EVT_DSTDROP = .FALSE.                                               
            CALLER = 'ZDROP_TRACKS'                                             
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. MUDROP_DST()) THEN                                           
            EVT_DSTDROP = .FALSE.                                               
            CALLER = 'MUDROP_DST'                                               
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 8))                                                 
     &    ) THEN                                                                
         IF (.NOT. CAJETS_DROP_DST()) THEN                                      
            EVT_DSTDROP = .FALSE.                                               
            CALLER = 'CAJETS_DROP_DST'                                          
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION POST_WRITE()                                             
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      POST_WRITE = .TRUE.                                                       
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: ZTRAKS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MURECO                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION UQUIT()                                                  
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-     4-AUG-95 14:03:47                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PBD_FLAG_VALUE(16)                                                
      CHARACTER*32 PBD_FLAG_NAME(16)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      UQUIT = .TRUE.                                                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LEVEL0                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: ZTRAKS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: T0D                              
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALOR                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAHITS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MURECO                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: C3PMET                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJETS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAPHEL                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: HMATRIX                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: L2C2EM                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CTAUS                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAJNEP                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CAFIX                            
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CATD                             
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: RECO_ESUM                        
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
