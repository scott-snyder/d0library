      INTEGER     NCDCEL
      PARAMETER ( NCDCEL = 7 ) 
      INTEGER     NCDLYR
      PARAMETER ( NCDLYR = 4 ) 
C
C
C ****  New common to be implemented in D0$INC:CDCPAR.INC
C
C
C ****  NAMESW   =   Names of the cells in each layer 
C
      COMMON /CDCPAN/  NAMESW ( NCDCEL, NCDLYR )
C
      CHARACTER*4 NAMESW
C

      COMMON /CDCPAR/ NUCDIM ( NCDCEL )
C
      INTEGER NUCDIM
C
