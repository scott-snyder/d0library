      CHARACTER*4 FUNCTION VPATH  
C----------------------------------------------
C
C  Returns PATH ('GEAN' or 'RECO') FOR VTX data 
C
C----------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 PATH,VTPATH,VGETPA
      VPATH=PATH 
      RETURN
      ENTRY VGETPA(VTPATH) 
      PATH=VTPATH 
      RETURN
      END
