      SUBROUTINE CD_HSTR_FLG(LHSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set flags in CD word in HSTR bank
C-      (bank hanging from GEAN in begin run record).
C-
C-   Inputs  : LHSTR = Pointer to HSTR bank.
C-   Outputs : 
C-
C-   Created  17-AUG-1992   Robert E. Avery
C-   Updated   9-SEP-1992   Robert E. Avery  Add VTX word, and use 
C-      BYTE_ORDER parameters. 
C-   Updated  22-SEP-1993   J.P. Cussonneau  Add trd word for new version 
C-                                                
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:D0LOG.INC' 
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER LHSTR
      INTEGER LASTTRDSTP,LASTFDCSTP
      INTEGER IHSTR_FLGS 
      BYTE BHSTR_FLGS(4)
      EQUIVALENCE(BHSTR_FLGS,IHSTR_FLGS)
C----------------------------------------------------------------------
      BHSTR_FLGS(BYTE1) = LASTTRDSTP()+4            ! TRD. Both low bits are
C                                                   ! for stp version. Both  
C                                                   ! following bits are for
C                                                   ! gains and ped version    
      BHSTR_FLGS(BYTE2) = LASTFDCSTP()              ! FDC
      BHSTR_FLGS(BYTE3) = 0                         ! VTX
      IF (SVTX(6).EQ.1) BHSTR_FLGS(BYTE3) = 1       
      BHSTR_FLGS(BYTE4) = 0                         ! CDC (nothing yet)
      IQ(LHSTR+21) = IHSTR_FLGS 
C      
  999 RETURN
      END
