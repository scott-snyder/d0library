      SUBROUTINE QCD_FAKE_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Includes init routines with right common blocks
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL INZBRA                       ! INITIALIZE ZEBRA
      CALL INZCOM(2)                    ! Initialize /ZEBCOM/
      CALL INZLNK                       ! Initialize ZLINKA
      CALL INZSTP                       ! Initialize /ZEBSTP/
      CALL INPAWC                       ! Initialize HBOOK4
  999 RETURN
      END
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
      PARAMETER (NPAWC=2500000 )
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
      PARAMETER (NNQ=4000000 )
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
      PARAMETER (NNC=2000000 )
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
      PARAMETER (NNW=10000 )
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
