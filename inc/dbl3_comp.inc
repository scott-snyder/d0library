C----------------------------------------------------------------------
C-
C-   Created  14-NOV-1993   Srini Rajagopalan
C-                  (S. abachi  renamed from dbcomp.inc)
C-   Modified: sss - reorder variables to avoid warnings.
C-
C-   Declarations for DBL3 Compression package.
C-
C----------------------------------------------------------------------
C
      INTEGER MAX_DROP
      PARAMETER (MAX_DROP=300)
C
C ---calib declarations------------------------------------------------
C
      INTEGER MAX_DTYP          
      PARAMETER (MAX_DTYP=10)
C
      INTEGER MAX_PATH
      PARAMETER (MAX_PATH=10)   
C
      CHARACTER*3 DTYP_CALIB(MAX_DTYP)
C
      INTEGER NUM_DTYP
      INTEGER NDROP(MAX_DTYP)
      INTEGER BANK_DROP(MAX_DROP,MAX_DTYP)
C
      INTEGER NPATH(MAX_DTYP)
      CHARACTER*30 CALIB_PATH(MAX_PATH,MAX_DTYP)
C
C num_dtyp          Number of detector types
C dtyp_calib(i)     Name of each detector type
C ndrop(i)          Number of banks to be dropped for each detector type
C bank_drop(j,i)    Bank names (j) to be dropped for each detector type(1..i)
C npath(i)          Path names for each detector type(1..i)
C calib_path(j,i)   Path names (1..j) to keep for each detector type (1..i)
C
C ---dbmon declarations------------------------------------------------
C
      INTEGER MAX_CLS,MAX_DEV
      PARAMETER (MAX_CLS=50)        
      PARAMETER (MAX_DEV=100)       
C
      INTEGER NCLS
      INTEGER NDEV(MAX_CLS)
      CHARACTER*12 CLAS(MAX_CLS)
      CHARACTER*12 DEVI(MAX_DEV,MAX_CLS)
C
C
C ncls              Number of classes
C ndev(i)           Number of devices to keep for each class(1..i)
C clas(i)           Class names to keep (i=1..ncls)
C devi(j,i)         Devices names to keep (j=1..ndev) for each class
C
C---common block-------------------------------------------------------
C
      COMMON /DBL3_COMP/ NUM_DTYP,NDROP,BANK_DROP,
     &                   NPATH,CALIB_PATH,
     &                   NCLS,NDEV,CLAS,DEVI,DTYP_CALIB
C
C----------------------------------------------------------------------
