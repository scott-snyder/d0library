C----------------------------------------------------------------------
C-   Created  28-APR-1995   Harrison B. Prosper
C-   Updated  26-JUN-1995   Jeffrey E. McDonald   
C----------------------------------------------------------------------
      INTEGER NSELECT,NETID,MAXFIL,MAXIN,MAXPAT,MAXCUT, NCUT
      INTEGER INP_NTUPLE_ID,OUT_NTUPLE_ID,CUT_NTUPLE_ID
      INTEGER IUNIT,NLABI,NLABO,NTAG,NFIELD,OUNIT,NFILE,MAXTAG
C----------------------------------------------------------------------
      PARAMETER( NETID  = 77)
      PARAMETER( MAXFIL = 20)
      PARAMETER( MAXIN  = 20)           
      PARAMETER( MAXPAT = 50000)        
      PARAMETER( MAXCUT = 10000)        
      PARAMETER( MAXTAG = 512 )
C----------------------------------------------------------------------
      INTEGER PATTERN_SELECT(MAXIN)
      INTEGER NPATTERN(MAXFIL)
      INTEGER BKGW(MAXFIL),SIGW(MAXFIL)
      REAL    PATTERN_IN (MAXIN, MAXPAT)
      REAL    PATTERN_CUT(MAXIN, MAXCUT)
      REAL    PATTERN_OUT(MAXFIL,MAXCUT)
      REAL    PATTERN_AVE(MAXIN,MAXFIL,MAXCUT)  
      REAL    PATTERN_SIG(MAXIN,MAXFIL,MAXCUT)
      REAL    TOT_IN(MAXFIL)
C----------------------------------------------------------------------
      CHARACTER*32 LABI(MAXIN),TAGS(MAXTAG),LABO(MAXFIL),FIELD(MAXIN)
      CHARACTER*80 FILE(MAXFIL), CUTFILE, OUTFILE
      INTEGER IWGT
C----------------------------------------------------------------------
      LOGICAL USE_ABS(MAXIN),LOWER_LIM(MAXIN),UPPER_LIM(MAXIN)
C----------------------------------------------------------------------
      COMMON /RGSCOM/PATTERN_IN,PATTERN_OUT,PATTERN_CUT,PATTERN_SELECT,
     &  NSELECT,NPATTERN,INP_NTUPLE_ID, CUT_NTUPLE_ID, OUT_NTUPLE_ID,
     &  IUNIT,OUNIT,NLABI,NTAG,NLABO,NFIELD,NFILE,NCUT,IWGT,TOT_IN,
     &  SIGW,BKGW,PATTERN_AVE,PATTERN_SIG
C
      COMMON /RGSCOMC/ LABI, LABO, TAGS, FIELD, CUTFILE, FILE, OUTFILE
C
      COMMON /RGSCOML/ USE_ABS,LOWER_LIM,UPPER_LIM
C----------------------------------------------------------------------
