C DEC/CMS REPLACEMENT HISTORY, Element PRMHTT.FOR
C *1     9-DEC-1986 17:08:12 HEDIN "MUON ZEBRA UTILITY"
C DEC/CMS REPLACEMENT HISTORY, Element PRMHTT.FOR
      SUBROUTINE PRMHTT(PRUNIT,LMUT,NMUT,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT MHTT - MUON TRACK HITS BANK
CC    TO GET THIS BANK ONE SHOULD USE PRMUOT        
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LMUT - BANK ADDRESS
CC    NMUT - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT 
CC    'ALL' OR 'LINEAR' GIVE COMPLETE STRUCTURE
CC    'ONE' GIVE FOR SINGLE TRACK LISAQ OR NISAQ MUST BE PROVIDED
CC    IFL - HOW MUCH TO PRINT 0 IS COMPLETE
CC
CC
CC    DH MARCH 1986 11-86 DH, NEW ZEBRA FORMAT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER PRUNIT,IFL,LMUT,NMUT 
      CHARACTER CFL*(*)
      WRITE(PRUNIT,101)
101   FORMAT('0',10X,' BANK MHTT: MUON TRACK HITS BANK '//
     A ' USE PRMUOT TO GET THIS BANK')
      RETURN
      END
