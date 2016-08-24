C----------------------------------------------------------------------
C-
C-  DI3GL.INC  - include file for SGI DI3000 emulator
C-
C-  Created XX-XX-XX Mike Shupe
C-  Updated 12-01-92 Lupe Howell Defined all the variable
C-
C----------------------------------------------------------------------
      INTEGER*4 IQD
      INTEGER*4 DI3INI
      INTEGER*4 DI3DIN
      INTEGER*4 IDEVIC
      INTEGER IRSPRJ
      INTEGER JUNIT
      COMMON /DI3M/IQD(47),DI3INI,IDEVIC,DI3DIN,IRSPRJ
      COMMON /DI3PR/JUNIT

C------------------RJ3 ARRAY (J3RGET)-----------------
      COMMON/J_DI3RJ3/
     &   XSIZE, YSIZE,   GAP,
     &   XBASEN,YBASEN,ZBASEN,
     &   XPLANN,YPLANN,ZPLANN,
     &   XSIZED,YSIZED,  GAPD,
     &   XBASND,YBASND,ZBASND,
     &   XPLNND,YPLNND,ZPLNND,
     &   XVW,   YVW,   ZVW,
     &   VNORMX,VNORMY,VNORMZ,
     &   VUPX,  VUPY,  VUPZ,
     &   VDIST, HDIST, YDIST,
     &   XPROJ, YPROJ, ZPROJ,
     &   VVECX, VVECY, VVECZ,
     &   UVECX, UVECY, UVECZ
      SAVE /J_DI3RJ3/
      REAL XSIZE, YSIZE, GAP, XBASEN, YBASEN, ZBASEN
      REAL XPLANN, YPLANN, ZPLANN, XSIZED, YSIZED, GAPD
      REAL XBASND, YBASND, ZBASND, XPLNND, YPLNND, ZPLNND
      REAL XVW, YVW, ZVW, VNORMX, VNORMY, VNORMZ, VUPX
      REAL VUPY, VUPZ, VDIST, HDIST, YDIST, XPROJ, YPROJ
      REAL ZPROJ, VVECX, VVECY, VVECZ, UVECX, UVECY, UVECZ
      REAL RJ3(3,13),UPVEC(3),VVEC(3),UVEC(3),NVEC(3)
      REAL VVIEW(3),VPROJ(3)
      EQUIVALENCE (UPVEC,VUPX),(VVEC,VVECX),(UVEC,UVECX)
      EQUIVALENCE (NVEC,VNORMX),(XSIZE,RJ3(1,1)),(VPROJ,XPROJ)
      EQUIVALENCE (VVIEW,XVW)
C--------------------RJ4 ARRAY (J4RGET)-----------------
C-                  WINDOW SPECIFICATIONS              -
      REAL UMIN, UMAX, VMIN, VMAX, VXMIN, VXMAX, VYMIN
      REAL VYMAX, VCXMN, VCXMX, VCYMN, VCYMX, XSCALE, YSCALE
      COMMON/J_DI3RJ4/
     &   UMIN, UMAX, VMIN, VMAX,
     &   VXMIN,VXMAX,VYMIN,VYMAX,
     &   VCXMN,VCXMX,VCYMN,VCYMX,
     &   XSCALE,YSCALE
      REAL RJ4
      DIMENSION RJ4(4,3)
      SAVE /J_DI3RJ4/
      EQUIVALENCE (UMIN,RJ4(1,1))
C------------------IJ1 ARRAY (J1IGET)-----------------
      INTEGER ISEGNM,INSIDE,IERROR,IDEBUG,ICOLOR
      INTEGER IINTEN,ILSTYL,ILWID ,IPEN  ,IPEDGE
      INTEGER IPSTYL,IPCOLO,IPINT ,IMARK ,IPICID
      INTEGER ICLRDF,IINTDF,ILSTDF,ILWDDF,IPENDF
      INTEGER IPEDDF,IPINDF,IPCLDF,IPNTDF,IMRKDF
      INTEGER IPIKDF,IPATH ,IFONT ,IHJUST,IVJUST
      INTEGER IPTHDF,IFONDF,IHJUDF,IVJUDF,IVISDF
      INTEGER IHIDF ,IDETDF,IPRIDF,ITYPDF,IWCLIP
      INTEGER IHCLIP,IYCLIP,IRIGHT,IPROJ ,IMODEL
      INTEGER IBATCH
C
      COMMON/J_DI3IJ1/
     &  ISEGNM,INSIDE,IERROR,IDEBUG,ICOLOR,
     &  IINTEN,ILSTYL,ILWID ,IPEN  ,IPEDGE,
     &  IPSTYL,IPCOLO,IPINT ,IMARK ,IPICID,
     &  ICLRDF,IINTDF,ILSTDF,ILWDDF,IPENDF,
     &  IPEDDF,IPINDF,IPCLDF,IPNTDF,IMRKDF,
     &  IPIKDF,IPATH ,IFONT ,IHJUST,IVJUST,
     &  IPTHDF,IFONDF,IHJUDF,IVJUDF,IVISDF,
     &  IHIDF ,IDETDF,IPRIDF,ITYPDF,IWCLIP,
     &  IHCLIP,IYCLIP,IRIGHT,IPROJ ,IMODEL,
     &  IBATCH
      INTEGER IJ1
      DIMENSION IJ1(46)
      SAVE /J_DI3RJ1/
      EQUIVALENCE (ISEGNM,IJ1(1))
C------------------RJ16 ARRAY (J16GET)----------------
      COMMON/J_VIEW/MVIEW(4,4),MVINV(4,4),MMODEL(4,4)
      REAL*4 MVIEW,MVINV,MMODEL
      DIMENSION RJ16(4,4,3)
      REAL RJ16
      SAVE /J_VIEW/
      EQUIVALENCE (RJ16,MVIEW)
C------------------SCRATCH----------------------------
      COMMON/J_DI3SCR/XBASE,YBASE,ZBASE,
     &            XPLANE,YPLANE,ZPLANE
      SAVE /J_DI3SCR/
      REAL XBASE,YBASE,ZBASE,XPLANE,YPLANE,ZPLANE
C------------------COLOR MAP--------------------------
      COMMON/J_COLRS/NCOLRS,RVEC(50),BVEC(50),GVEC(50)
      INTEGER NCOLRS,RVEC,BVEC,GVEC
C------------------POSITIONS--------------------------
C-            CURRENT POSITIONS
      COMMON/J_POSNOW/VPOSN(3),
     &  CRTX,CRTY,ARL,
     &  PARRAY(3,256)
      REAL VPOSN,CRTX,CRTY,ARL,PARRAY
      REAL P2ARRY(2,256)
      EQUIVALENCE (PARRAY,P2ARRY)
C-----------------WINDOW PARAMETERS-------------------
C-  IFDFLT=0 MEANS DEFAULTS ARE IN EFFECT (ONLY JWINDO CALLED)
C-  THE REST ARE FLAGS INDICATING JVPORT, JVSPAC, AND JWINDO CALLS.

      COMMON/J_WFLAGS/IFDFLT,IFSPAC,IFPORT,IFWIND,ITRANS,IMDEV
      INTEGER IFDFLT,IFSPAC,IFPORT,IFWIND,ITRANS,IMDEV
C-
C-  ITRANS = IFDFLT OR IPROJ OR IMODEL OR IIMAGE OR IMDEV
C-  IMDEV=IFSPAC OR IFPORT
C-    (FLAG TO INDICATE THAT A TRANSFORMATION IS NEEDED EITHER BECAUSE
C-     WINDOWING HAS BEEN DONE OR A TRANSFORMATION HAS BEEN CALLED FOR)
C-
      COMMON/J_IDS/WDID,LWDID,WDRZ
      INTEGER*4 WDID,LWDID,WDRZ
      COMMON/SCRDIM/SCRX,SCRY,WLEFT,WRIGHT,WBOTM,WTOP,WNEAR,WFAR,
     &  XMARG,YMARG,WIDTH,HEIGHT,RASP,IX1W,IX2W,IY1W,IY2W
C
      REAL SCRX,SCRY,WLEFT,WRIGHT,WBOTM,WTOP,WNEAR,WFAR
      REAL XMARG,YMARG,WIDTH,HEIGHT,RASP
      INTEGER*4 IX1W,IX2W,IY1W,IY2W

C-------SEGMENT INFO FOR ROTATION, AND SEGMENT STORAGE FOR HARDCOPY------
C-
C-  CODE FOR EACH SUBROUTINE (FOR BRANCHING FROM DISPLAY LIST)
C-
      COMMON/J_SUBIDS/PUTS,PUTSUP,HCPY,
     &  I3DRAW,I3MARK,I3MOVE,I3POLY,I3PLGN,
     &  I2DRAW,I2MARK,I2MOVE,I2POLY,I2PLGN,
     &  IJCOLR,IJINTN,IJSTYL, IJWID, IJPEN,
     &  IJPEDG,IJPSTY,IJPCOL,IJPINT,IJMARK,
     &  IJPICK,IJPATH,IJFONT,IJJUST,IJRECT,
     &  IR3DRA,IR3MRK,IR3MOV,IR3PLY,IR3PGN,
     &  IRDRAW,IRMARK,IRMOVE,IRPOLY,IRPLGN,
     &  IJSIZE,IJBASE,IPLANE,I1STRG,I3STRG,
     &  IJCIRC
C
      LOGICAL PUTS,PUTSUP,HCPY
C-               PUTS    IF TRUE, PUT STUFF INTO SEGMENT STORAGE
C-               PUTSUP  IF TRUE, SUPPRESS PUTTING FOR THIS ROUTINE
C-               HCPY    IF TRUE, HARDOPY IN PROGRESS
      INTEGER I3DRAW,I3MARK,I3MOVE,I3POLY,I3PLGN
      INTEGER I2DRAW,I2MARK,I2MOVE,I2POLY,I2PLGN
      INTEGER IJCOLR,IJINTN,IJSTYL, IJWID, IJPEN
      INTEGER IJPEDG,IJPSTY,IJPCOL,IJPINT,IJMARK
      INTEGER IJPICK,IJPATH,IJFONT,IJJUST,IJRECT
      INTEGER IR3DRA,IR3MRK,IR3MOV,IR3PLY,IR3PGN
      INTEGER IRDRAW,IRMARK,IRMOVE,IRPOLY,IRPLGN
      INTEGER IJSIZE,IJBASE,IPLANE,I1STRG,I3STRG
      INTEGER IJCIRC
C-
C-  IMAGE TRANSFORMATIONS ON SEGMENTS
C-
      INTEGER NSMX
      PARAMETER (NSMX=400)
      COMMON/J_SEGTR/IASSO(NSMX),PXSEG(NSMX),PYSEG(NSMX),SXSEG(NSMX),
     &  SYSEG(NSMX),ROTSEG(NSMX),TXSEG(NSMX),TYSEG(NSMX)
      INTEGER IASSO
      REAL PXSEG,PYSEG,SXSEG,SYSEG,ROTSEG
      REAL TXSEG,TYSEG
C-
C-  CURRENT SEGMENT TRANSFORMATIONS
C-
      COMMON/J_CURIMG/ITYCUR,PXC,PYC,SXC,SYC,TXC,TYC,CSC,SNC
      INTEGER ITYCUR
      REAL PXC,PYC,SXC,SYC,TXC,TYC,CSC,SNC
C-
C-  SEGMENT BOOKKEEPING
C-
      COMMON/J_SEGS/NSMXX,NSEGS,NUMSEG(NSMX),IVIS(NSMX),
     &  IATT3D(NSMX),INDS1(NSMX),NCALLS(NSMX),ITTYP(NSMX),
     &  IDETEC(NSMX),IFMODE,RJ4SAV(4,3,NSMX),ISPRIO(NSMX),
     &  OBJID(NSMX),IDNOW
C
      INTEGER NSMXX,NSEGS,NUMSEG,IVIS
      INTEGER IATT3D,INDS1,NCALLS,ITTYP
      INTEGER IDETEC,IFMODE,RJ4SAV,ISPRIO
      INTEGER*4 OBJID,IDNOW
C-              NSMX    MAX. # OF SEGMENTS ALLOWED
C-              NSEGS   CURRENT # OF SEGMENTS
C-              NUMSEG  ID NUMBER ("NAME") OF SEGMENT
C-              IVIS    VISIBILITY FLAG (0=NO, 1=YES)
C-              IATT3D  SEGMENT MAY BE 3D MANIPULATED (0=NO,1=YES)
C-              INDS1   BEGINNING OF SEGMENT IN BLOC STORAGE
C-              NCALLS  NUMBER OF SUBROUTINE CALLS IN THIS SEGMENT
C-              ITTYP   IMAGE TRANSFORMATION TYPE
C-              IDETEC  DETECTABILITY OF THIS SEGMENT (0=N0,1=YES) (PICKS)
C-              IFMODE  =1 WHEN PLAYING BACK IN JFRAME (BLOCK PUTS)
C-              RJ4SAV  INITIAL WINDOWS FOR EACH SEGMENT
C-              ISPRIO  SEGMENT PRIORITY
C-              OBJID   SEGMENT ID NUMBER (GL)
C-              HCPY    HARDCOPY IN PROGRESS
C-
C- SEGMENT STRING STORAGE CONTROL
C-
      COMMON/J_FRAME/LFRAME,LENST
      LOGICAL LFRAME
      INTEGER LENST
C-
C- PICK COMMONS
C-
      COMMON/J_PKREGS/NPICKS,IPNOW,ISNOW,IPKSEG(100),IDPICK(100),
     &  IPRIOR(100),PKXMN(100),PKYMN(100),PKXMX(100),PKYMX(100),
     &  PDXAPR,PDYAPR
C
      INTEGER NPICKS,IPNOW,ISNOW,IPKSEG,IDPICK,IPRIOR
      REAL PKXMN,PKYMN,PKXMX,PKYMX,PDXAPR,PDYAPR
C
      COMMON/J_PICKY/NPSEGS,NMPSEG(100),IPECHO,IPBUTN,
     &  XECHO(6),YECHO(6),IASSOC(6)
      INTEGER NPSEGS,NMPSEG,IPECHO,IPBUTN,IASSOC
      REAL XECHO,YECHO
C-
C--------------------------DEV_ ATTRIBUTES, ETC.----------------------
C-
      COMMON/J_GRFCTL/FANCY,IDFONT,IDTFNT,
     &  IDCOLR,IDLSTY,IDLWID,IDFILL,IDFPAT
      LOGICAL FANCY
      INTEGER IDFONT,IDTFNT,IDCOLR,IDLSTY,IDLWID,IDFILL,IDFPAT
C-
C-    FANCY  - TRUE IF FANCY GRAPHICS (FONTS, ETC) ARE LINKED
C-    IDFONT - CURRENT FONT TYPE.
C-             -1 = HARDWARE CHARACTERS
C-             0 =  STROKE FONT
C-             1 =  ITALICIZED STROKE FONT
C-             2 =  BOLD STROKE FONT
C-             3 =  ITALICIZED BOLD
C-    IDCOLR - CURRENT COLOR INDEX
C-    IDLSTY - CURRENT LINE STYLE INDEX
C-    IDLWID - CURRENT LINE WIDTH
C-    IDFILL  - 0=DON'T FILL POLY, 1=FILL POLY
C-    IDFPAT - CURRENT FILL PATTERN INDEX
C-    IDTFNT  - FULL FONT TYPE (CDF,ROMAN,ETC)
C-
C-------------------------DEVICES AND TRANSFORMATIONS--------------------
      COMMON/J_GRFCT2/IDV,ICLR,IFILL,IDRUNI,IGSYNC,IDUBL,
     &    ICSUPP,NONROT,
     &    XSIZEH,YSIZEH,
     &    WNX,WNY,WNDX,WNDY,
     &    UMINH,UMAXH,VMINH,VMAXH,
     &    XSCAL,YSCAL,DXOFF,DYOFF,
     &    DVUMIN,DVUMAX,DVVMIN,DVVMAX,
     &    TMODEL(4,4),TTRAN(4,4) ,TUVN(4,4),  TVIEW(4,4),
     &    TSCREE(4,4),TSCINV(4,4),TDEVIC(4,4),TTOTAL(4,4),
     &    TTOTAH(4,4),CURMAT(4,4),UVNMAT(4,4),
     &    D2LINE,IPP,
     &    IHCDEV,HFILNAME,IHNAME,HCFILNAM
      INTEGER IDV,ICLR,IFILL,IDRUNI,IGSYNC,IDUBL
      INTEGER ICSUPP,NONROT
      REAL XSIZEH,YSIZEH
      REAL WNX,WNY,WNDX,WNDY
      REAL UMINH,UMAXH,VMINH,VMAXH
      REAL XSCAL,YSCAL,DXOFF,DYOFF
      REAL DVUMIN,DVUMAX,DVVMIN,DVVMAX
      REAL TMODEL,TTRAN,TUVN,  TVIEW
      REAL TSCREE,TSCINV,TDEVIC,TTOTAL
      REAL TTOTAH,CURMAT,UVNMAT
      INTEGER IPP,IHCDEV,IHNAME
      CHARACTER*100 D2LINE
      CHARACTER*80 HFILNAME
      CHARACTER*12 HCFILNAM
C-
C-    IDV    - DRIVER TYPE (1=PS,2=TK,3=TKTERM)
C-    ICLR   - COLOR DRIVER ENABLED
C-    IFILL  - CURRENT POLYGON IS BEING FILLED
C-    IDRUNI - UNIT NUMBER FOR HARDCOPY OUTPUT FILE
C-    IGSYNC - SYNC CHECK ON INITIAL CALLING SEQUENCES
C-    IDUBL  - IF 1, DOUBLE BUFFERING IN EFFECT
C-    ICSUPP - IF 1, SUPPRESS JCLEAR SEGMENT CLEARING
C-    NONROT - IF 1, CALL NON_ROTATABLE_SEGMENTS
C-    WNX... - WINDOW PLACEMENT AND SIZE ON SCREEN
C-    UMIN...- WINDOW CORNERS IN WORLD COORDINATES
C-    TMODEL - MODELLING TRANSFORMATION MATRIX
C-    TTRAN  - TRANSLATION TO VIEWPOINT
C-    TUVN   - ROTATION FROM WORLD TO UVN SYSTEM
C-    TVIEW  - TUVN X TTRAN
C-    TSCREE - TRANSFORMATION FROM VIRTUAL WINDOW TO WORKSTATION SCREEN
C-    TSCINV - INVERSE OF TSCREE
C-    TTOTAL - TOTAL TRANSFORMATION FOR WORKSTATION (TVIEW X TSCREE)
C-    TDEVIC - TRANSFORMATION FROM VIRTUAL WINDOW TO HARDCOPY DEVICE
C-    TTOTAH - TOTAL TRANSFORMATION FOR HARDCOPY DEVICE
C-    CURMAT - MATRIX GOTTEN FROM TOP OF SGI STACK
C-    UVNMAT - UVN MATRIX EXTRACTED FROM CURMAT
C-    D2LINE - LINE BUFFER FOR HARDCOPY OUTPUT
C-    IPP    - LINE BUFFER POINTER FOR HARDCOPY OUTPUT
C-    IHCDEV - HARDCOPY DEVICE NUMBER
C-    HFILNAME - FILE NAME FOR HARDCOPY FILE (PROVIDED BY USER)
C-    IHNAME - 0 = USE DEFAULT NAMES, 1 = USE HFILNAME

C-----------------------------3D MENU COMMON-------------------------------
      COMMON /J_MEN3D/ENAB3D,NMENID,MPID(9),MPIX(9),MPIY(9),
     &     MENSTR(9),IMENOB,IMACOB(9),MEVIEW
C
      INTEGER NMENID,MPID,MPIX,MPIY
      INTEGER IMENOB,IMACOB,MEVIEW
      CHARACTER*4 MENSTR
      LOGICAL ENAB3D
C------------------------------WRITTEN DEBUG COMMON-------------------------
      COMMON/J_DEBWR/IDEBWR
      INTEGER IDEBWR

