      SUBROUTINE PBDINI                                                         
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-    This routine must be called by the FrameWork to initialize                
C-    the run time switches associated with the packages                        
C-    Created by the PROGRAM BUILDER                                            
C-    28-SEP-92 16:33:02                                                        
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL WRNGOK                                                            
      INTEGER LOGUNT,MAXLOG,MAXWRN                                              
      CHARACTER*32 STRGLG                                                       
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
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
      PBD_FLAG_MAX = 15                                                         
C-                                                                              
      PBD_FLAG_VALUE( 1) = .TRUE.                                               
      PBD_FLAG_NAME( 1) = 'COMBDIS'                                             
      PBD_FLAG_VALUE( 2) = .TRUE.                                               
      PBD_FLAG_NAME( 2) = 'COM3DIS'                                             
      PBD_FLAG_VALUE( 3) = .TRUE.                                               
      PBD_FLAG_NAME( 3) = 'ZTRAKSDIS'                                           
      PBD_FLAG_VALUE( 4) = .TRUE.                                               
      PBD_FLAG_NAME( 4) = 'VTXDIS'                                              
      PBD_FLAG_VALUE( 5) = .TRUE.                                               
      PBD_FLAG_NAME( 5) = 'TRDDIS'                                              
      PBD_FLAG_VALUE( 6) = .TRUE.                                               
      PBD_FLAG_NAME( 6) = 'CDCDIS'                                              
      PBD_FLAG_VALUE( 7) = .TRUE.                                               
      PBD_FLAG_NAME( 7) = 'FDCDIS'                                              
      PBD_FLAG_VALUE( 8) = .TRUE.                                               
      PBD_FLAG_NAME( 8) = 'CALDIS'                                              
      PBD_FLAG_VALUE( 9) = .TRUE.                                               
      PBD_FLAG_NAME( 9) = 'MUTRAKSDIS'                                          
      PBD_FLAG_VALUE(10) = .TRUE.                                               
      PBD_FLAG_NAME(10) = 'MUODIS'                                              
      PBD_FLAG_VALUE(11) = .TRUE.                                               
      PBD_FLAG_NAME(11) = 'SAMDIS'                                              
      PBD_FLAG_VALUE(12) = .TRUE.                                               
      PBD_FLAG_NAME(12) = 'PHYDIS'                                              
      PBD_FLAG_VALUE(13) = .TRUE.                                               
      PBD_FLAG_NAME(13) = 'LV0DIS'                                              
      PBD_FLAG_VALUE(14) = .TRUE.                                               
      PBD_FLAG_NAME(14) = 'SCANDIS'                                             
      PBD_FLAG_VALUE(15) = .TRUE.                                               
      PBD_FLAG_NAME(15) = 'PIXIE_EVENT'                                         
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-    The following routine has been generated by the Program Builder           
C-    because the size of the common blocks has been modified                   
C-    by the user.                                                              
C-                                                                              
C-    Created by the PROGRAM BUILDER                                            
C-    28-SEP-92 16:33:02                                                        
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
C DEC/CMS REPLACEMENT HISTORY, Element ZEBCOM.INC                               
C *12   19-DEC-1988 11:45:39 D0LIBRARY "added IXCOM, removed IXDIV"             
C *11   29-NOV-1988 16:25:17 D0LIBRARY "added comments"                         
C *10   29-NOV-1988 16:21:03 D0LIBRARY "added IXDIV and IXDVR"                  
C *9    22-SEP-1986 11:26:53 JONCKHEERE "Increase ZEBCOM size for GEANT"        
C *8     2-SEP-1986 13:45:57 SERBAN "links are now in .LINKS files"             
C *7    15-JUL-1986 16:00:14 JONCKHEERE "PARAMETER FOR MUHT,CAHT,CDHT ADDED (MUH
C *6    27-MAY-1986 13:10:11 SERBAN "parameter NNQ changed from 50000 to 100000"
C *5    15-MAY-1986 16:51:11 SERBAN "added more pointers for ISAJET"            
C *4    12-MAY-1986 18:41:24 HEDIN "Put in Muon Zebra Links"                    
C *3    12-MAY-1986 16:46:40 SERBAN "added parameters for link pointers"        
C *2     7-MAY-1986 21:41:19 KUNORI "convert to standard 77 (drop *4)"          
C *1    14-JAN-1986 17:22:30 PROTOPOPESCU "Zebra common block for event data"   
C DEC/CMS REPLACEMENT HISTORY, Element ZEBCOM.INC                               
C                                                                               
C  ZEBCOM is the main zebra common block for event data storage                 
C                                                                               
      INTEGER NNQ,NREF                                                          
C     PARAMETER (NNQ=200000)                                                    
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NNQ=1500000 )                                                  
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
C-    28-SEP-92 16:33:02                                                        
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
      PARAMETER (NNC=1000000 )                                                  
C-                                                                              
      COMMON/ZEBSTP/IXSTP,IDVSTP,IDVSUM,FENSTP,LSTPH,LZSUM,                     
     1 LSLV0,LLPDH,LLGNH,LLTMH,LLGEH,LLV,   ! level 0 links                     
     2 LSMUO,LMPDH,LMGNH,LMTMH,LMGEH,LMU,   ! muon links                        
     3 LSVTX,LVPDH,LVGNH,LVTMH,LVGEH,LVT,   ! vertex chamber links              
     4 LSCDC,LDPDH,LDGNH,LDTMH,LDGEH,LDALH,LDC,   ! cdc links                   
     5 LSTRD,LTPDH,LTGAI,LTGEN,LTGEO,LTR,   ! trd links                         
     6 LSFDC,LFPDH,LFGNH,LFTMH,LFGEH,LFD,   ! fdc links                         
     7 LSCAL,LCPDH,LCGNH,LCGEH,LCA,         ! calorimeter links                 
     8 ZCONS,ENDZC                                                              
      INTEGER IXSTP,IDVSTP,IDVSUM,FENSTP(10),LZSUM,ZCONS(NNC),ENDZC             
      REAL C(NNC)                                                               
      INTEGER IC(NNC),LC(NNC)                                                   
      EQUIVALENCE (LSTPH,LC(1)),(LC(9),IC(1),C(1))                              
      INTEGER LSTPH     ! static parameter header                               
     $,  LSLV0      ! level 0 constants header                                  
     $,  LLPDH      ! level 0 pedestal header                                   
     $,  LLGNH      ! level 0 gain header                                       
     $,  LLTMH      ! level 0 time constants header                             
     $,  LLGEH      ! level 0 geometry constants header                         
     $,  LLV(10)    ! available                                                 
C                                                                               
      INTEGER LSMUO      ! Muon constants header                                
     $,  LMPDH      ! Muon pedestal header                                      
     $,  LMGNH      ! Muon gain header                                          
     $,  LMTMH      ! Muon time constants header                                
     $,  LMGEH      ! Muon geometry constants header                            
     $,  LMU(10)    ! available                                                 
C                                                                               
      INTEGER LSVTX      ! Vertex chamber constants header                      
     $,  LVPDH      ! Vertex chamber pedestal header                            
     $,  LVGNH      ! Vertex chamber gain header                                
     $,  LVTMH      ! Vertex chamber time constants header                      
     $,  LVGEH      ! Vertex chamber geometry constants header                  
     $,  LVT(10)    ! available                                                 
C                                                                               
      INTEGER LSCDC      ! CDC constants header                                 
     $,  LDPDH      ! CDC pedestal header                                       
     $,  LDGNH      ! CDC gain header                                           
     $,  LDTMH      ! CDC time constants header                                 
     $,  LDGEH      ! CDC geometry constants header                             
     $,  LDALH      ! CDC wire alignment constants header                       
     $,  LDC(9)     ! available                                                 
C                                                                               
      INTEGER LSTRD      ! TRD constants header                                 
     $,  LTPDH      ! TRD pedestal header                                       
     $,  LTGAI      ! TRD gains header                                          
     $,  LTGEN      ! TRD general header                                        
     $,  LTGEO      ! TRD geometry constants header                             
     $,  LTR(10)    ! available                                                 
C                                                                               
      INTEGER LSFDC      ! FDC constants header                                 
     $,  LFPDH      ! FDC pedestal header                                       
     $,  LFGNH      ! FDC gain header                                           
     $,  LFTMH      ! FDC time constants header                                 
     $,  LFGEH      ! FDC geometry constants header                             
     $,  LFD(10)    ! available                                                 
C                                                                               
      INTEGER LSCAL      ! Calorimeter constants header                         
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
C-    28-SEP-92 16:33:02                                                        
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
C     PARAMETER (NNW=20000)                                                     
C-                                                                              
C-    The following is the new parameter value                                  
C-                                                                              
      PARAMETER (NNW=100 )                                                      
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
C-    28-SEP-92 16:33:02                                                        
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
      PARAMETER (NPAWC=250000 )                                                 
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
      LOGICAL FUNCTION STSWCH()                                                 
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-    This routine allows the Program Builder user to set                       
C-    and reset(ON/OFF) the run time switches associated                        
C-    with the packages.                                                        
C-    Created by the PROGRAM BUILDER                                            
C-    28-SEP-92 16:33:02                                                        
C-                                                                              
C-                                                                              
      LOGICAL PRODUC                                                            
      LOGICAL LCOMBDIS                                                          
      LOGICAL LCOM3DIS                                                          
      LOGICAL LZTRAKSDIS                                                        
      LOGICAL LVTXDIS                                                           
      LOGICAL LTRDDIS                                                           
      LOGICAL LCDCDIS                                                           
      LOGICAL LFDCDIS                                                           
      LOGICAL LCALDIS                                                           
      LOGICAL LMUTRAKSDIS                                                       
      LOGICAL LMUODIS                                                           
      LOGICAL LSAMDIS                                                           
      LOGICAL LPHYDIS                                                           
      LOGICAL LLV0DIS                                                           
      LOGICAL LSCANDIS                                                          
      LOGICAL LPIXIE_EVENT                                                      
      INTEGER NUMPAR                                                            
      CHARACTER*30 LABELS(15)                                                   
      CHARACTER TYPARR(15)                                                      
      INTEGER LIMITS(2,15)                                                      
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
      INTEGER PBD_FLAG_MAX                                                      
      EXTERNAL PRODUC                                                           
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      DATA NUMPAR /15/                                                          
      DATA LABELS /                                                             
     &          'COMBDIS Selected'                                              
     &         ,'COM3DIS Selected'                                              
     &         ,'ZTRAKSDIS Selected'                                            
     &         ,'VTXDIS Selected'                                               
     &         ,'TRDDIS Selected'                                               
     &         ,'CDCDIS Selected'                                               
     &         ,'FDCDIS Selected'                                               
     &         ,'CALDIS Selected'                                               
     &         ,'MUTRAKSDIS Selected'                                           
     &         ,'MUODIS Selected'                                               
     &         ,'SAMDIS Selected'                                               
     &         ,'PHYDIS Selected'                                               
     &         ,'LV0DIS Selected'                                               
     &         ,'SCANDIS Selected'                                              
     &         ,'PIXIE_EVENT Selected'                                          
     &       /                                                                  
      DATA TYPARR /                                                             
     &           'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &          ,'L'                                                            
     &       /                                                                  
C-                                                                              
      IF (.NOT. PRODUC()) THEN                                                  
         LCOMBDIS = PBD_FLAG_VALUE( 1)                                          
         LCOM3DIS = PBD_FLAG_VALUE( 2)                                          
         LZTRAKSDIS = PBD_FLAG_VALUE( 3)                                        
         LVTXDIS = PBD_FLAG_VALUE( 4)                                           
         LTRDDIS = PBD_FLAG_VALUE( 5)                                           
         LCDCDIS = PBD_FLAG_VALUE( 6)                                           
         LFDCDIS = PBD_FLAG_VALUE( 7)                                           
         LCALDIS = PBD_FLAG_VALUE( 8)                                           
         LMUTRAKSDIS = PBD_FLAG_VALUE( 9)                                       
         LMUODIS = PBD_FLAG_VALUE(10)                                           
         LSAMDIS = PBD_FLAG_VALUE(11)                                           
         LPHYDIS = PBD_FLAG_VALUE(12)                                           
         LLV0DIS = PBD_FLAG_VALUE(13)                                           
         LSCANDIS = PBD_FLAG_VALUE(14)                                          
         LPIXIE_EVENT = PBD_FLAG_VALUE(15)                                      
         CALL GETDIS(NUMPAR,LABELS,TYPARR,LIMITS                                
     &              ,LCOMBDIS                                                   
     &              ,LCOM3DIS                                                   
     &              ,LZTRAKSDIS                                                 
     &              ,LVTXDIS                                                    
     &              ,LTRDDIS                                                    
     &              ,LCDCDIS                                                    
     &              ,LFDCDIS                                                    
     &              ,LCALDIS                                                    
     &              ,LMUTRAKSDIS                                                
     &              ,LMUODIS                                                    
     &              ,LSAMDIS                                                    
     &              ,LPHYDIS                                                    
     &              ,LLV0DIS                                                    
     &              ,LSCANDIS                                                   
     &              ,LPIXIE_EVENT                                               
     &                                          )                               
C-                                                                              
         PBD_FLAG_VALUE( 1) = LCOMBDIS                                          
         PBD_FLAG_VALUE( 2) = LCOM3DIS                                          
         PBD_FLAG_VALUE( 3) = LZTRAKSDIS                                        
         PBD_FLAG_VALUE( 4) = LVTXDIS                                           
         PBD_FLAG_VALUE( 5) = LTRDDIS                                           
         PBD_FLAG_VALUE( 6) = LCDCDIS                                           
         PBD_FLAG_VALUE( 7) = LFDCDIS                                           
         PBD_FLAG_VALUE( 8) = LCALDIS                                           
         PBD_FLAG_VALUE( 9) = LMUTRAKSDIS                                       
         PBD_FLAG_VALUE(10) = LMUODIS                                           
         PBD_FLAG_VALUE(11) = LSAMDIS                                           
         PBD_FLAG_VALUE(12) = LPHYDIS                                           
         PBD_FLAG_VALUE(13) = LLV0DIS                                           
         PBD_FLAG_VALUE(14) = LSCANDIS                                          
         PBD_FLAG_VALUE(15) = LPIXIE_EVENT                                      
      END IF                                                                    
C-                                                                              
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
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
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
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
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
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
      DO I = 1, PBD_FLAG_MAX                                                    
        WRITE (6,'('' '',A,'' : '',L)') PBD_FLAG_NAME(I)(1:32),                 
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
      LOGICAL FUNCTION PXINIT()                                                 
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-    28-SEP-92 16:33:02                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PCOMB_INIT                                                        
      EXTERNAL PCOMB_INIT                                                       
      LOGICAL PCOM3DIS_INIT                                                     
      EXTERNAL PCOM3DIS_INIT                                                    
      LOGICAL PZTRAKS_INIT                                                      
      EXTERNAL PZTRAKS_INIT                                                     
      LOGICAL PVINIT                                                            
      EXTERNAL PVINIT                                                           
      LOGICAL PTINIT                                                            
      EXTERNAL PTINIT                                                           
      LOGICAL PDINIT                                                            
      EXTERNAL PDINIT                                                           
      LOGICAL PFINIT                                                            
      EXTERNAL PFINIT                                                           
      LOGICAL PCINIT                                                            
      EXTERNAL PCINIT                                                           
      LOGICAL PMUTRAKS_INIT                                                     
      EXTERNAL PMUTRAKS_INIT                                                    
      LOGICAL PMINIT                                                            
      EXTERNAL PMINIT                                                           
      LOGICAL PSINIT                                                            
      EXTERNAL PSINIT                                                           
      LOGICAL PPINIT                                                            
      EXTERNAL PPINIT                                                           
      LOGICAL P0INIT                                                            
      EXTERNAL P0INIT                                                           
      LOGICAL SCAN_INI                                                          
      EXTERNAL SCAN_INI                                                         
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      PXINIT = .TRUE.                                                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 1))                                                 
     &    ) THEN                                                                
         IF (.NOT. PCOMB_INIT()) THEN                                           
            PXINIT = .FALSE.                                                    
            MESSID = '!PCOMB_INIT is false.'                                    
            CALLER = 'PCOMB_INIT'                                               
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
         IF (.NOT. PCOM3DIS_INIT()) THEN                                        
            PXINIT = .FALSE.                                                    
            MESSID = '!PCOM3DIS_INIT is false.'                                 
            CALLER = 'PCOM3DIS_INIT'                                            
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
         IF (.NOT. PZTRAKS_INIT()) THEN                                         
            PXINIT = .FALSE.                                                    
            MESSID = '!PZTRAKS_INIT is false.'                                  
            CALLER = 'PZTRAKS_INIT'                                             
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
         IF (.NOT. PVINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PVINIT is false.'                                        
            CALLER = 'PVINIT'                                                   
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
         IF (.NOT. PTINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PTINIT is false.'                                        
            CALLER = 'PTINIT'                                                   
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
         IF (.NOT. PDINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PDINIT is false.'                                        
            CALLER = 'PDINIT'                                                   
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
         IF (.NOT. PFINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PFINIT is false.'                                        
            CALLER = 'PFINIT'                                                   
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
         IF (.NOT. PCINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PCINIT is false.'                                        
            CALLER = 'PCINIT'                                                   
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
         IF (.NOT. PMUTRAKS_INIT()) THEN                                        
            PXINIT = .FALSE.                                                    
            MESSID = '!PMUTRAKS_INIT is false.'                                 
            CALLER = 'PMUTRAKS_INIT'                                            
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
         IF (.NOT. PMINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PMINIT is false.'                                        
            CALLER = 'PMINIT'                                                   
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
      IF ( (PBD_FLAG_VALUE(11))                                                 
     &    ) THEN                                                                
         IF (.NOT. PSINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PSINIT is false.'                                        
            CALLER = 'PSINIT'                                                   
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
      IF ( (PBD_FLAG_VALUE(12))                                                 
     &    ) THEN                                                                
         IF (.NOT. PPINIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!PPINIT is false.'                                        
            CALLER = 'PPINIT'                                                   
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
         IF (.NOT. P0INIT()) THEN                                               
            PXINIT = .FALSE.                                                    
            MESSID = '!P0INIT is false.'                                        
            CALLER = 'P0INIT'                                                   
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
         IF (.NOT. SCAN_INI()) THEN                                             
            PXINIT = .FALSE.                                                    
            MESSID = '!SCAN_INI is false.'                                      
            CALLER = 'SCAN_INI'                                                 
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
C-    No routine was provided for the package: PIXIE_EVENT                      
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
C-    28-SEP-92 16:33:02                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PXEVENT_READ_OK                                                   
      EXTERNAL PXEVENT_READ_OK                                                  
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      EVENT_READ_OK = .TRUE.                                                    
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: COMBDIS                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: COM3DIS                          
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: ZTRAKSDIS                        
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: VTXDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: TRDDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CDCDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: FDCDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: CALDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MUTRAKSDIS                       
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: MUODIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: SAMDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: PHYDIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: LV0DIS                           
C-                                                                              
C-                                                                              
C-    No routine was provided for the package: SCANDIS                          
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE(15))                                                 
     &    ) THEN                                                                
         IF (.NOT. PXEVENT_READ_OK()) THEN                                      
            EVENT_READ_OK = .FALSE.                                             
            CALLER = 'PXEVENT_READ_OK'                                          
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
      LOGICAL FUNCTION PXEXEC()                                                 
C-                                                                              
C-                                                                              
C-    Purposes and Methods:                                                     
C-    --------------------                                                      
C-                                                                              
C-    Created by the PROGRAM BUILDER Release                                    
C-    28-SEP-92 16:33:02                                                        
C-                                                                              
C-                                                                              
      CHARACTER*32 MESSID,CALLER                                                
      CHARACTER*80 MESSAG                                                       
      LOGICAL PCOMB_EXEC                                                        
      EXTERNAL PCOMB_EXEC                                                       
      LOGICAL PCOM3DIS_EXEC                                                     
      EXTERNAL PCOM3DIS_EXEC                                                    
      LOGICAL PZTRAKS_EXEC                                                      
      EXTERNAL PZTRAKS_EXEC                                                     
      LOGICAL PVEXEC                                                            
      EXTERNAL PVEXEC                                                           
      LOGICAL PTEXEC                                                            
      EXTERNAL PTEXEC                                                           
      LOGICAL PDEXEC                                                            
      EXTERNAL PDEXEC                                                           
      LOGICAL PFEXEC                                                            
      EXTERNAL PFEXEC                                                           
      LOGICAL PCEXEC                                                            
      EXTERNAL PCEXEC                                                           
      LOGICAL PMUTRAKS_EXEC                                                     
      EXTERNAL PMUTRAKS_EXEC                                                    
      LOGICAL PMEXEC                                                            
      EXTERNAL PMEXEC                                                           
      LOGICAL PSEXEC                                                            
      EXTERNAL PSEXEC                                                           
      LOGICAL PPEXEC                                                            
      EXTERNAL PPEXEC                                                           
      LOGICAL P0EXEC                                                            
      EXTERNAL P0EXEC                                                           
      LOGICAL PSCAN_EXEC                                                        
      EXTERNAL PSCAN_EXEC                                                       
      LOGICAL PBD_FLAG_VALUE(15)                                                
      CHARACTER*32 PBD_FLAG_NAME(15)                                            
      INTEGER PBD_FLAG_MAX                                                      
      COMMON /PBD_COMMON/ PBD_FLAG_VALUE,PBD_FLAG_NAME,PBD_FLAG_MAX             
C-                                                                              
      PXEXEC = .TRUE.                                                           
C-                                                                              
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 1))                                                 
     &    ) THEN                                                                
         IF (.NOT. PCOMB_EXEC()) THEN                                           
            PXEXEC = .FALSE.                                                    
            CALLER = 'PCOMB_EXEC'                                               
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 2))                                                 
     &    ) THEN                                                                
         IF (.NOT. PCOM3DIS_EXEC()) THEN                                        
            PXEXEC = .FALSE.                                                    
            CALLER = 'PCOM3DIS_EXEC'                                            
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 3))                                                 
     &    ) THEN                                                                
         IF (.NOT. PZTRAKS_EXEC()) THEN                                         
            PXEXEC = .FALSE.                                                    
            CALLER = 'PZTRAKS_EXEC'                                             
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 4))                                                 
     &    ) THEN                                                                
         IF (.NOT. PVEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PVEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 5))                                                 
     &    ) THEN                                                                
         IF (.NOT. PTEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PTEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 6))                                                 
     &    ) THEN                                                                
         IF (.NOT. PDEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PDEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 7))                                                 
     &    ) THEN                                                                
         IF (.NOT. PFEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PFEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 8))                                                 
     &    ) THEN                                                                
         IF (.NOT. PCEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PCEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE( 9))                                                 
     &    ) THEN                                                                
         IF (.NOT. PMUTRAKS_EXEC()) THEN                                        
            PXEXEC = .FALSE.                                                    
            CALLER = 'PMUTRAKS_EXEC'                                            
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(10))                                                 
     &    ) THEN                                                                
         IF (.NOT. PMEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PMEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(11))                                                 
     &    ) THEN                                                                
         IF (.NOT. PSEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PSEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(12))                                                 
     &    ) THEN                                                                
         IF (.NOT. PPEXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'PPEXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(13))                                                 
     &    ) THEN                                                                
         IF (.NOT. P0EXEC()) THEN                                               
            PXEXEC = .FALSE.                                                    
            CALLER = 'P0EXEC'                                                   
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
      IF ( (PBD_FLAG_VALUE(14))                                                 
     &    ) THEN                                                                
         IF (.NOT. PSCAN_EXEC()) THEN                                           
            PXEXEC = .FALSE.                                                    
            CALLER = 'PSCAN_EXEC'                                               
            MESSAG = 'This error is ignored'                                    
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')                               
         END IF                                                                 
      END IF                                                                    
C-                                                                              
C-    No routine was provided for the package: PIXIE_EVENT                      
C-                                                                              
      RETURN                                                                    
      END                                                                       
C-                                                                              
C-                                                                              
C-                                                                              
