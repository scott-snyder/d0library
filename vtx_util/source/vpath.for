      CHARACTER*4 FUNCTION VPATH  
C----------------------------------------------
C
C  Returns PATH ('GEAN' or 'RECO') FOR VTX data 
C
C----------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 PATH
      character*4 VTPATH,VGETPA
      save path
      VPATH=PATH 
      RETURN
      ENTRY VGETPA(VTPATH) 
      PATH=VTPATH 
      RETURN
      END
