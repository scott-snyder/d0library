C **** COMMON BLOCK GRFPAR
C
C    This common block is used for device status and initialization.
C
C      DINIT    logical        designates that JDINIT has been called.
C      EMDISP   character*5    Emulator display name
C      UWIND(1) real           the lower X window coordinate
C      UWIND(2) real           the upper X window coordinate
C      UWIND(3) real           the lower Y window coordinate
C      UWIND(4) real           the upper Y window coordinate
C      UWIND(5) real           the lower Z window coordinate
C      UWIND(6) real           the upper Z window coordinate
C      UVIEW(1) real           the lower X viewport coordinate
C      UVIEW(2) real           the upper X viewport coordinate
C      UVIEW(3) real           the lower Y viewport coordinate
C      UVIEW(4) real           the upper Y viewport coordinate
C      UVIEW(5) real           the lower intensity value
C      UVIEW(6) real           the upper intensity value
C      WCLIP    logical        Window clipping status
C      HCLIP    logical        Hither clipping status flag
C      YCLIP    logical        Yon clipping status flag
C      SCV(1)   real           Scale factor along the X-axis
C      SCV(2)   real           Scale factor along the Y-axis
C      SCV(3)   real           Scale factor along the Z-axis
C      TRV(1)   real           Translate factor along the X-axis
C      TRV(2)   real           Translate factor along the Y-axis
C      TRV(3)   real           Translate factor along the Z-axis
C      VUPNT(1) real           The X world viewpoint
C      VUPNT(2) real           The Y world viewpoint
C      VUPNT(3) real           The Z world viewpoint
C      NORML(1) real           The X world view normal vector
C      NORML(2) real           The Y world view normal vector
C      NORML(3) real           The Z world view normal vector
C      UPVEC(1) real           The X world view up vector
C      UPVEC(2) real           The Y world view up vector
C      UPVEC(3) real           The Z world view up vector
C      PAROBX   real           The X world displacement from VWRP
C      PAROBY   real           The Y world displacement from VWRP
C      PAROBZ   real           The Z world displacement from VWRP
C      PERSP    real           The N world displacement from VWRP
C      PEROBX   real           The X world displacement from VWRP
C      PEROBY   real           The Y world displacement from VWRP
C      PEROBZ   real           The Z world displacement from VWRP
C      PRJTYP   integer        The Type of projection (1..4)
C      PTPARA   integer        Constant value for PARALLEL ORTHO Proj
C      PTPARO   integer        Constant value for PARALLEL OBLIQ Proj
C      PTPERS   integer        Constant value for PERSPECT ORTHO Proj
C      PTPERO   integer        Constant value for PERSPECT OBLIQ Proj
C      RIGHT    real           Factor used to convert from left- to
C                              right-handed coordinate axis.
C      KPTRN    real           Current pattern number.
C      MODEL    logical        Status of the modeling transformation
C      MODMAT(4,4) real        Current modeling matrix.
C      MODACT(8,MTRN,MMAT) real Modeling action storage area where
C                               MODACT(1..8,NTRN,NMAT) = eight passed
C                               parameters to JTRANS and JBUILD.
C      NTRN(MMAT) integer      Array storing the number of 
C                              transformations stored for a particular
C                              modeling matrix.
C
      INTEGER MTRN, MMAT, PTPARA, PTPARO, PTPERS, PTPERO
      PARAMETER (PTPARA = 1, PTPARO = 2, PTPERS = 3, PTPERO = 4)
      INTEGER PRJTYP
      PARAMETER (MTRN=100, MMAT=100)
      REAL MODACT(8,MTRN,MMAT)
      INTEGER NTRN(MMAT)
      INTEGER NMAT
      LOGICAL DINIT
      CHARACTER*5 EMDISP
      CHARACTER*6 DISPL, TDISP, TEMDIS
      REAL    UWIND(6), UVIEW(6)
      LOGICAL WCLIP, HCLIP, YCLIP
      REAL    SCV(3), TRV(3), VUPNT(3), NORML(3), UPVEC(3)
      REAL    RIGHT, MODMAT(4,4)
      LOGICAL MODEL
      INTEGER KPTRN
      REAL    PAROBX, PAROBY, PAROBZ
      REAL    PEROBX, PEROBY, PEROBZ, PERSP
C
      INTEGER NP
      PARAMETER (NP=50)
      REAL    SVIEW(NP,6), SWIND(NP,6)  
      REAL    CVIEW(6), CWIND(6)  
      LOGICAL EAS_CONN
C
      COMMON /GRFPAR/ DINIT, UWIND, UVIEW, EMDISP, DISPL, TDISP
      COMMON /GRFPAR/ SCV, TRV
      COMMON /GRFPAR/ WCLIP, HCLIP, YCLIP, RIGHT, KPTRN
      COMMON /GRFPAR/ VUPNT, NORML, UPVEC, MODMAT, MODEL
      COMMON /GRFPAR/ MODACT, NTRN, NMAT
      COMMON /GRFPAR/ PRJTYP, PEROBX, PEROBY, PEROBZ, PERSP
      COMMON /GRFPAR/ PAROBX, PAROBY, PAROBZ, TEMDIS
      COMMON /GRFPAR/ CVIEW, CWIND, SVIEW, SWIND,EAS_CONN
