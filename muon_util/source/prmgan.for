C DEC/CMS REPLACEMENT HISTORY, Element PRMGAN.FOR
C *1    10-FEB-1987 18:14:13 HEDIN "Muon constant print routines"
C DEC/CMS REPLACEMENT HISTORY, Element PRMGAN.FOR
      SUBROUTINE PRMGAN(UNIT,LMGAN,NMGAN,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT MGAN - MUON   GAIN   BANKS
CC
CC    UNIT - UNIT NUMBER FOR PRINTOUT
CC    LMGAN - BANK ADDRESS
CC    NMGAN - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    CFL TELLS WHETHER TO PRINT OUT OLD/NEW/CURRENT/ALL BANK
CC    NMGAN IS MODULE NUMBER IF 0 THEN EVERY MODULE
CC
CC    DH 1-87      SET UP FOR BASEMENT SETUP    
CC    J.Green 5-89  print wire and plane for each channel
CC    J.Green 5-90  correct write statements
C-    Jul-91    J.Green   change I8 to I9 in header format
C-    Nov-93    J.Green   Unix compatability
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER UNIT,LMGAN,NMGAN,IFL
      INTEGER NWIRX
      CHARACTER CFL*(*)
      INTEGER GZMGAN,GZMGAN_N,GZMGAN_R,NMODS,NMOD,I,KMGAN,NP,
     A N1,N2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
C               functions to get channel,wire, plane from position in bank
C                 assume 24 wires per plane
      INTEGER CHAN, WIRE, PLANE
      CHAN(I) = 2*I - MOD(I,2)
      PLANE(I) = CHAN(I)/(4*NWIRX)
      WIRE(I) = 2*( (CHAN(I) - 4*NWIRX*PLANE(I)) /8 )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      DATA NMODS/307/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      IF(NMGAN.EQ.0) THEN
        N1=1                       ! FIRST MODULE
        N2=NMODS                   ! SECOND MODULE
      ELSE
        N1=NMGAN
        N2=NMGAN
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC                                       LOOK FOR OLD CONSTANTS              
        NP=0
        DO 1 NMOD=N1,N2  
        KMGAN=GZMGAN_R(NMOD)
          IF(KMGAN.NE.0) THEN 
            NWIRX = IC(KMGAN+10)/(2*IC(KMGAN+11))
            NP=1
            WRITE(UNIT,100) NMOD  
100         FORMAT('0    REFERENCE GAIN BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMGAN+I),I=1,12),(C(KMGAN+I),I=13,16)
101         FORMAT(' TYPE ',I13,' PERCENT GOOD ',I5,' RESOLUTION ',I12/
     A         ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' MODULE NUMBER  ',I4,' NO. CHANNELS ',I4,
     A         ' NO. PLANES',I3,' SPARE ',I2/
     A         ' AVERAGE PEDS ', 2F9.2, ' AVERAGE GAINS ', 2F9.4 )  
            WRITE(UNIT,102)
102         FORMAT('   CHN PLN WIRE        PEDESTAL      PED WIDTH',
     &            '      GAIN       ERROR ON GAIN  '/)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMGAN+17+4*I),C(KMGAN+18+4*I),
     A            C(KMGAN+19+4*I),C(KMGAN+20+4*I),I=0,IC(KMGAN+10)-1)
103         FORMAT(1X,3I5,2F13.2,2F13.4)
          ENDIF 
 1      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,104)
104       FORMAT('0 NO REFERENCE MUON GAINS     '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
        NP=0
CCC                                         LOOK FOR CURRENT CONSTANTS
        DO 2 NMOD=N1,N2  
        KMGAN=GZMGAN(NMOD)
          IF(KMGAN.NE.0) THEN  
            NWIRX = IC(KMGAN+10)/(2*IC(KMGAN+11))
            NP=1
            WRITE(UNIT,200) NMOD  
200         FORMAT('0   CURRENT GAIN BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMGAN+I),I=1,12),(C(KMGAN+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMGAN+17+4*I),C(KMGAN+18+4*I),
     A            C(KMGAN+19+4*I),C(KMGAN+20+4*I),I=0,IC(KMGAN+10)-1)
          ENDIF
 2      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,204)
204       FORMAT('0 NO CURRENT MUON GAINS     '/)
      ENDIF                  
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
        NP=0
CCC                                           LOOK FOR NEW CONSTANTS
        DO 3 NMOD=N1,N2   
        KMGAN=GZMGAN_N(NMOD)
          IF(KMGAN.NE.0) THEN 
            NWIRX = IC(KMGAN+10)/(2*IC(KMGAN+11))
            NP=1
            WRITE(UNIT,300) NMOD  
300         FORMAT('0       NEW GAIN BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMGAN+I),I=1,12),(C(KMGAN+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMGAN+17+4*I),C(KMGAN+18+4*I),
     A            C(KMGAN+19+4*I),C(KMGAN+20+4*I),I=0,IC(KMGAN+10)-1)
          ENDIF
 3      CONTINUE     
          IF(NP.EQ.0) WRITE(UNIT,304)
304       FORMAT('0 NO NEW MUON GAINS     '/)
      ENDIF
      RETURN
      END
