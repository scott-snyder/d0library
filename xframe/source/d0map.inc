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
c
      integer d0xuserunit
      parameter (d0xuserunit = 269)
c
c --- filenames and their lengths
      character*160 fzrz_file
      character*80 stp_file,out_file
      integer fzrz_len,stp_len,out_len
      logical wildc
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
c --- number of events output
      integer nevout
c
c --- offset for navigating
      integer ifoff
      logical accoff
c
c --- for output conversion native-->exchange (1), otherwise (2)
c --- iouttype=0 file closed, reset output mode
c ---          1 output all events
c ---          3 output from list
c ---          5 output if satisfy d0xquery requirement
c ---          6 output if using d0xuser as a filter
c ---          7 output in sets - use "nper" to tell how many
c
      integer iouttype,ntag,irtag(1000),ietag(1000),nper
c
c --- implement requirements (-1=yes, 0=no)
      integer doreq
c
c --- here come the requirements
      integer nreqs,nreq2
      logical doreqtf,doandor,reqstop
      character*4 rbanks(100)
      integer roffsets(100),rdtypes(100),rops1(100),rpointer(100)
      integer rops2(100),rvals2(100)
      integer rivals1(100)
      real rrvals1(100)
      logical rbvals1(100),rsatis(100)
      character*4 rcvals1(100)
      equivalence (rivals1,rbvals1,rrvals1,rcvals1)
      character*12 rspecs(100)
c
c --- parameters here
c     operations
      integer islt,isle,iseq,isge,isgt,istrue,isfalse,isne
      parameter (islt=0,isle=1,iseq=2,isge=3)
      parameter (isgt=4,istrue=5,isfalse=6,isne=7)
c     datatypes
      integer isreal,isint,ishex,istf,ischar
      parameter (isreal=0,isint=1,ishex=2,istf=3,ischar=4)
c     positive/negative/and/or
      integer dopos,doneg,doand,door,dostop,dogo
      parameter (dopos=-1,doneg=0,doand=-1,door=0,dostop=-1,dogo=0)
c
c --- make histos 
c
      integer nhsts,hhids(100),hoffsets(100),htypes(100)
      character*4 hbanks(100)
      character*12 hspecs(100)
c
c --- make tuples
c
      integer ntups,hnids(100),hnoffsets(100),hntypes(100)
      character*4 hnbanks(100)
      character*12 hnspecs(100)
c
c --- d0xuser
c
      integer nd0xtrue,nd0xfalse
      logical dod0xuser
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
c --- yes/no on various packages
c
      logical udsttodst,docafix,cafixinit,docleanem,cleaneminit
      logical docleanmu,cleanmuinit,docahits,cahitsinit
c
c --- d0dad stuff
c
      logical d0dadrdc
      character*256 d0dadcat
c
c --- disk or tape
c
      logical d0xdisk
c
      common /d0map/fzrz_spec,fzrz_opened,fzrz_file,fzrz_len,
     &  fzrz_lun,fzrz_mode,file_type,
     &  stp_spec,stp_opened,stp_file,stp_len,stp_lun,
     &  out_spec,out_opened,out_file,out_len,out_lun,
     &  nrun,nevo,nevi1,nevi2,grun,halt,dbstore,dbdiv,dbhead,
     &  dbname,pstore,plinear,d0zeblst,d0zeblst_length,iformt,
     &  ifoff,accoff,acclin,zstate,tstate,dstate,iouttype,jcone,
     &  pnuttype,nbdrop,cbdrop,ntag,irtag,ietag,dropkeep,doreq,
     &  nreqs,rbanks,roffsets,rdtypes,rops1,rops2,rvals2,rivals1,
     &  rspecs,doreqtf,rsatis,doandor,reqstop,nhsts,hbanks,hhids,
     &  hoffsets,htypes,hspecs,ntups,hnbanks,hnids,
     &  hnoffsets,hntypes,hnspecs,rpointer,nreq2,wildc,
     &  nd0xtrue,nd0xfalse,dod0xuser,udsttodst,docafix,cafixinit,
     &  docleanem,cleaneminit,docleanmu,cleanmuinit,d0dadrdc,
     &  d0dadcat,nevout,d0xdisk,docahits,cahitsinit,nper
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
C
C       hbook common here
C
      INTEGER TLIMIT
      PARAMETER (TLIMIT = 1000000)
      COMMON /PAWC/HMEMOR(TLIMIT)
      REAL HMEMOR
