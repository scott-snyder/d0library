C ****  NAMESW   =   Names of the cells in each layer 
C
C ????  NCDCEL is defined in CDCPAR.INC
C
      INTEGER    NCDLYR 
      PARAMETER (NCDLYR = 4) 
C
      CHARACTER*4 NAMESW
C
      COMMON /NAMESW/NAMESW(NCDCEL,NCDLYR) 
