      SUBROUTINE PRSAHH(PRUNIT,LSAHH,NSAHH,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT SAHH - MUON HITS HEADER   
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LSAHH - BANK ADDRESS
CC    NSAHH - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    THERE WILL ONLY BE A SINGLE MUON HIT HEADER
CC    SO IGNORE CFL
CC
CC    O.Eroshin 18-MAR-1991
C-   Updated  20-JAN-1992   Daria Zieminska  eliminate argument in GZSAHH 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER N_STATION
      PARAMETER (N_STATION=6)
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LSAHH,NSAHH,IFL,LHIT
      CHARACTER CFL*(*)
      INTEGER N,NST,GZSAHH
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      LHIT=LSAHH
      IF(LHIT.EQ.0) THEN
 100      FORMAT(' NO SAHH BANK')
        LHIT=GZSAHH() 
        IF(LHIT.EQ.0) THEN
          WRITE(PRUNIT,100)
          RETURN
        ENDIF
      ENDIF
C
      WRITE(PRUNIT,101)
 101  FORMAT('0',10X,' BANK SAHH: SAMUS HIT HEADER'//,
     +       '    STATION NUMBER   PLANE NUMBER  #TUBE HITS',/)
      DO N=1,IQ(LHIT-1),3
        NST = N/3+1
        WRITE(PRUNIT,102) NST,'X',IQ(LHIT+3*(NST-1)+1),
     +                        'Y',IQ(LHIT+3*(NST-1)+2),
     +                        'U',IQ(LHIT+3*(NST-1)+3)
  102   FORMAT(I13,12X,A,I13/,25X,A,I13,/,25X,A,I13)
      END DO
C
      RETURN
      END
