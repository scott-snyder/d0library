      INTEGER IDMIN,IDMAX
      PARAMETER( IDMIN=2 )      
      PARAMETER( IDMAX=12289 )  
C                               
C
      COMMON/STADDR/NID,NBADFL,STEPN,ACMIN,ACMAX,ACMOM0,ACMOM1,ACMOM2,
     +              ACMOM3,ACMOM4,ACOUT,ACPED
C
      REAL     ACMOM0(IDMIN:IDMAX),ACPED(IDMIN:IDMAX)
      REAL*8   ACMOM1(IDMIN:IDMAX),ACMOM2(IDMIN:IDMAX)
     &        ,ACMOM3(IDMIN:IDMAX),ACMOM4(IDMIN:IDMAX)
      INTEGER ACMIN(IDMIN:IDMAX),ACMAX(IDMIN:IDMAX),ACOUT(IDMIN:IDMAX)
      INTEGER*2 NID(IDMIN:IDMAX),NBADFL(IDMIN:IDMAX),STEPN(IDMIN:IDMAX)
C
C ACMOM0 = NUMBER OF ENTRIES or second peak
C ACMOM1 = AVERAGE /PEAK
C ACMOM2 = SIGMA
C ACMOM3 = SKEWNESS
C ACMOM4 = Kurtosis
C ACPED  = Pedestal value read from database for pedestal subtraction
C
C NID = Flag to determine if Hist being filled, for bad channels,
C or channel looked at in the pulser run
C NBADFL = Bit encoded Badness flag
C STEPN  = Step number for this hist.
C
      INTEGER NADCS,NADCS_TOT
      PARAMETER(NADCS=2*4608)           
      PARAMETER(NADCS_TOT=2*6144)       
