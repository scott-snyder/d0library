C----------------------------------------------------------------------
C- DBL3 stuff
C----------------------------------------------------------------------
C-   Mixed link area for top directory
C----------------------------------------------------------------------
      INTEGER        D3_MXP 
      PARAMETER     (D3_MXP = 310)         
      INTEGER        D3_LT, D3_LK(D3_MXP), D3_LD(D3_MXP)
      INTEGER        D3_NK,D3_NXK
      PARAMETER     (D3_NK = 15)               
      PARAMETER     (D3_NXK = 8)               
      COMMON /D3TD/  D3_LT,D3_LK,D3_LD
C-
C- D3_LT     = Address of the Top Directory
C- D3_LK(*)  = Address of the Key banks
C- D3_LD(*)  = Address of the Data banks
C-
C----------------------------------------------------------------------
C-   Permanent link area addresses of banks for DBL3
C----------------------------------------------------------------------
      INTEGER D3_MXL
      PARAMETER( D3_MXL = 210 )
      INTEGER D3_LNK(D3_MXL)
      COMMON /LKD3/ D3_LNK
C-
C----------------------------------------------------------------------
C-   Common block for dbl3 parameters
C----------------------------------------------------------------------
      INTEGER        D3_DIV
      LOGICAL        D3_END
      INTEGER        D3_UNIT
      CHARACTER *24  D3_PATH
      CHARACTER *12  D3_FSTR,D3_ISTR
      INTEGER        D3_XKI(24)
      INTEGER        D3_NKK
      LOGICAL        D3_NOT
      COMMON /D3PA/  D3_DIV,D3_END,D3_UNIT,D3_XKI,D3_NKK,D3_PATH,
     &  D3_FSTR,D3_ISTR,D3_NOT
C-
C- D3_DIV    = Users Zebra divison (input to DBINIT)
C- D3_END    = Set true if DBEND is called. set false during db-init
C- D3_UNIT   = Unit number for currently used dbl3 file
C- D3_XKI    = Array of extra keys to search on
C- D3_NKK    = Number of extra keys to search on
C- D3_PATH   = Path name
C- D3_FSTR   = Options for fetching elements, argument for DBUSE
C- D3_ISTR   = Options for insering elements 
C- D3_NOT    = True if keys 3 and 4 is NOT time, else false. 
C-
C----------------------------------------------------------------------
C-   Common block for open dbl3 files
C----------------------------------------------------------------------
      INTEGER MXTOPD
      PARAMETER (MXTOPD=48)
      CHARACTER*8 D3_TOPD(MXTOPD)
      INTEGER ND3_TOPD,D3_DUNI(MXTOPD)
      COMMON /D3FI/ ND3_TOPD,D3_DUNI,D3_TOPD
C-
C- MXTOPD     Maximum number of dbl3 files open
C- ND3_TOPD   Number of dbl3 files open
C- D3_TOPD    Array of top directory of open dbl3 files
C- D3_DUNI    Array of unit numbers of open dbl3 files
C----------------------------------------------------------------------
