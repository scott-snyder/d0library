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
C
C ZEBSTP.INC
      INTEGER NNC
      PARAMETER (NNC=3000000)
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
