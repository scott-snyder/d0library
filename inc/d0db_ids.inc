C
C--S. Abachi  8/91
C
      INTEGER       NUM_DB
      PARAMETER     (NUM_DB=9)
      CHARACTER*2   DB_SNAME (NUM_DB)
      CHARACTER*4   DB_LNAME (NUM_DB)
C
      DATA  DB_SNAME 
     &     /'CA', 'MU', 'CD', 'FD', 'VT', 'TR', 'DM', 'HV', 'LU' /
      DATA  DB_LNAME 
     &     /'CALR', 'MUON', 'CDCD', 'FDCD', 'VTXD', 'TRDD',
     &      'DBMD', 'HVTD', 'LUMD' /
