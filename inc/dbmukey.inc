C----------------------------------------------------------------------
C- Common block for current keys, filled by DBMU_GETDM or DBMU_GETHV
C
      INTEGER NCKEYS
      PARAMETER (NCKEYS = 15)   
      INTEGER DBKEYS(NCKEYS)
      COMMON /DBMCURKEYS/ DBKEYS
C----------------------------------------------------------------------
