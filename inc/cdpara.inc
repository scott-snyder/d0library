C Initialization parameters for CDC.
      INTEGER LFADC             
      PARAMETER( LFADC= 512)
      INTEGER NBSENS            
      PARAMETER( NBSENS= 7)
      INTEGER NBDELY            
      PARAMETER( NBDELY= 2)
      INTEGER LPULSE
      PARAMETER( LPULSE= 8)
      INTEGER LEN_DHIT
      PARAMETER( LEN_DHIT = 3)
C
C ****  Related parameters
C
      INTEGER MXSENS            
      PARAMETER( MXSENS = NBSENS-1 )
      INTEGER MXFADC            
      PARAMETER( MXFADC= MXSENS + 2*NBDELY )
