c
c this is the include file for "MAP" (or FZBROWSE if you prefer)
c
c this file "drives" the program - and as such you can load it by
c hand from disk in the "FILE" menu
c
c FILES:  you can specify a "data file" (FZ or RZ), an stp file, or
c         a regular old output file (from bank dumps, dzsurv, etc.)
c         what's needed is a name (global) and logicals specifying
c         if it's been specified and opened
c
c --- filenames and their lengths
      character*80 fzrz_file,stp_file,out_file
      integer fzrz_len,stp_len,out_len
c
c --- is it specified?
      logical fzrz_spec,stp_spec,out_spec
c
c --- is it already opened?
      logical fzrz_opened,stp_opened,out_opened
c
c --- logical unit number
      integer fzrz_lun,stp_lun,out_lun
c
c --- file type:  0=fzrz, 1=stp, 2=output
      integer file_type
c
c --- fzrz mode (0=exchange, 1=native)
      integer fzrz_mode
c
c --- few handy dandy things to use for events
      integer nrun,nevo,nevi1,nevi2,grun
c
c --- halt = .true. then HALT (set asynchronously)
      logical halt
c
c --- which store/division/head/name?
      integer dbstore(0:3),dbdiv(0:3),dbhead(0:3)
      character*8 dbname(0:3)
      integer pstore
c
c --- which linear bank to get
      integer plinear
      logical acclin
c
c --- d0$zeblst:  zstate = 0: off, 1: on
      character*80 d0zeblst
      integer d0zeblst_length,zstate
c
c --- tree widget? tstate=0: off, 1: on
      integer tstate
c
c --- data window? dstate=0: off, 1: on
      integer dstate
c
c --- format for output (0=auto, 1=int, 2=float, 3=hex)
      integer iformt
c
c --- offset for navigating
      integer ifoff
      logical accoff
c
c --- for output conversion native-->exchange (1), otherwise (2)
c --- iouttype=0 file closed
c ---          1 output all events
c ---          3 output from list
c
      integer iouttype,ntag,irtag(1000),ietag(1000)
c
c --- jet cone for physics analysis
c
      real jcone
c
c --- pnut bank type
c
      integer pnuttype
c
c --- bank lists?
c
      integer dropkeep
      integer nbdrop
      character*4 cbdrop(100)
c
      common /d0map/fzrz_spec,fzrz_opened,fzrz_file,fzrz_len,
     &  fzrz_lun,fzrz_mode,file_type,
     &  stp_spec,stp_opened,stp_file,stp_len,stp_lun,
     &  out_spec,out_opened,out_file,out_len,out_lun,
     &  nrun,nevo,nevi1,nevi2,grun,halt,dbstore,dbdiv,dbhead,
     &  dbname,pstore,plinear,d0zeblst,d0zeblst_length,iformt,
     &  ifoff,accoff,acclin,zstate,tstate,dstate,iouttype,jcone,
     &  pnuttype,nbdrop,cbdrop,ntag,irtag,ietag,dropkeep
c
c     make ZEBCOM with alot of space
c
      INTEGER NNQ,NREF
      PARAMETER (NNQ=3000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    
     &       ,IXMAIN   
     &       ,IXDVR    
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     
      INTEGER LHEADR    
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
c
c     this is how CERNLIB defines the pesky FOR003
c
      INTEGER IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
      COMMON /ZUNIT/ IQREAD,IQPRNT,IQPR2,IQLOG,IQPNCH,IQTTIN,IQTYPE
c
C
C ZEBSTP.INC
      INTEGER NNC
      PARAMETER (NNC=1000000)
      COMMON/ZEBSTP/IXSTP,IDVSTP,IDVSUM,FENSTP,LSTPH,LZSUM,
     1 LSLV0,LLPDH,LLGNH,LLTMH,LLGEH,LLV,   
     2 LSMUO,LMPDH,LMGNH,LMTMH,LMGEH,LMU,   
     3 LSVTX,LVPDH,LVGNH,LVTMH,LVGEH,LVT,   
     4 LSCDC,LDPDH,LDGNH,LDTMH,LDGEH,LDALH,LDC,   
     5 LSTRD,LTPDH,LTGAI,LTGEN,LTGEO,LTR,   
     6 LSFDC,LFPDH,LFGNH,LFTMH,LFGEH,LFD,   
     7 LSCAL,LCPDH,LCGNH,LCGEH,LCA,         
     8 ZCONS,ENDZC
      INTEGER IXSTP,IDVSTP,IDVSUM,FENSTP(10),LZSUM,ZCONS(NNC),ENDZC
      REAL C(NNC)
      INTEGER IC(NNC),LC(NNC)
      EQUIVALENCE (LSTPH,LC(1)),(LC(9),IC(1),C(1))
      INTEGER LSTPH     
     $,  LSLV0      
     $,  LLPDH      
     $,  LLGNH      
     $,  LLTMH      
     $,  LLGEH      
     $,  LLV(10)    
C
      INTEGER LSMUO      
     $,  LMPDH      
     $,  LMGNH      
     $,  LMTMH      
     $,  LMGEH      
     $,  LMU(10)    
C
      INTEGER LSVTX      
     $,  LVPDH      
     $,  LVGNH      
     $,  LVTMH      
     $,  LVGEH      
     $,  LVT(10)    
C
      INTEGER LSCDC      
     $,  LDPDH      
     $,  LDGNH      
     $,  LDTMH      
     $,  LDGEH      
     $,  LDALH      
     $,  LDC(9)     
C
      INTEGER LSTRD      
     $,  LTPDH      
     $,  LTGAI      
     $,  LTGEN      
     $,  LTGEO      
     $,  LTR(10)    
C
      INTEGER LSFDC      
     $,  LFPDH      
     $,  LFGNH      
     $,  LFTMH      
     $,  LFGEH      
     $,  LFD(10)    
C
      INTEGER LSCAL      
     $,  LCPDH      
     $,  LCGNH      
     $,  LCGEH      
     $,  LCA(11)    
