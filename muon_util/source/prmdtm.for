      SUBROUTINE PRMDTM (UNIT,LMDTM,NMDTM,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out MDTM - muon delta time banks
C-
C-   Inputs  :  UNIT - Unit number for printing
C-              LMDTM - Bank address
C-   Outputs : 
C-   Controls:  NMDTM - Module number, if = 0 then all modules
C-              CFL - Flag for what to print - old/new/current/all
C-              IFL - How much to print
C-
C-   Created  17-OCT-1988   Jim Green
C-                May-89    J.Green   Print wire and plane for each channel
C-                May-90    J.Green   correct write statements
C-                Jul-91    J.Green   change I8 to I9 in header format
C-                Jan-92    J.Green   fix for mods with < 24 wires
C-                Apr-92    J.Green   new bank header
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER UNIT,LMDTM,NMDTM,IFL
      CHARACTER CFL*(*)
      INTEGER GZMDTM,GZMDTM_N,GZMDTM_R,NMODS,NMOD,I,KMDTM,NP
      INTEGER NWIRX, N1, N2
      DATA NMODS/4/
C----------------------------------------------------------------------
C               functions to get channel, wire, plane from position in bank
      INTEGER CHAN, WIRE, PLANE
      CHAN(I) = 4*I + 3*MOD(I,2) - 1
      PLANE(I) = CHAN(I)/(4*NWIRX)
      WIRE(I) = 2*( (CHAN(I) - 4*NWIRX*PLANE(I)) /8 )
C----------------------------------------------------------------------
      IF(NMDTM.EQ.0) THEN
        N1=1                       ! FIRST MODULE
        N2=NMODS                   ! SECOND MODULE
      ELSE
        N1=NMDTM
        N2=NMDTM
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC                                            LOOK FOR OLD CONSTANTS
        NP=0
        DO 1 NMOD=N1,N2  
        KMDTM=GZMDTM_R(NMOD)
          IF(KMDTM.NE.0) THEN
            NWIRX = IC(KMDTM+10)/IC(KMDTM+11)
            NP=1
            WRITE(UNIT,100) NMOD  
100         FORMAT('0   REFERENCE DELTA TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDTM+I),I=1,12),(C(KMDTM+I),I=13,16)
101         FORMAT(' TYPE ',I13,' PERCENT GOOD ',I5,' RESOLUTION ',I12/
     A         ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' MODULE NUMBER  ',I4,' NO. CHANNELS ',I4,
     A         ' NO. PLANES',I3,' MODULE LENGTH ',I10/
     A         ' AVERAGE DT0S ', 2F9.1, ' AVERAGE SLOPES ', 2F9.5 )
            WRITE(UNIT,102)
102         FORMAT('  CHN PLN WIRE  ADC(DELTA T = 0)    NS/COUNT  ',
     &            'ERROR IN ADC_0   ERROR IN SLOPE'/)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMDTM+13+4*I),C(KMDTM+14+4*I),
     A            C(KMDTM+15+4*I),C(KMDTM+16+4*I),I=1,IC(KMDTM+10))    
103         FORMAT(3I5,F13.1,F16.5,F13.1,F13.5)
          ENDIF 
 1      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,104)
104       FORMAT('0 NO REFERENCE MUON DELTA TIMES '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
        NP=0
CCC                                            LOOK FOR CURRENT CONSTANTS
        DO 2 NMOD=N1,N2  
        KMDTM=GZMDTM(NMOD)
          IF(KMDTM.NE.0) THEN  
            NWIRX = IC(KMDTM+10)/IC(KMDTM+11)
            NP=1
            WRITE(UNIT,200) NMOD  
200         FORMAT('0   CURRENT DELTA TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDTM+I),I=1,12),(C(KMDTM+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMDTM+13+4*I),C(KMDTM+14+4*I),
     A            C(KMDTM+15+4*I),C(KMDTM+16+4*I),I=1,IC(KMDTM+10))    
          ENDIF
 2      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,204)
204       FORMAT('0 NO CURRENT MUON DELTA TIMES '/)
      ENDIF                  
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
        NP=0
CCC                                                LOOK FOR NEW CONSTANTS
        DO 3 NMOD=N1,N2   
        KMDTM=GZMDTM_N(NMOD)
          IF(KMDTM.NE.0) THEN 
            NWIRX = IC(KMDTM+10)/IC(KMDTM+11)
            NP=1
            WRITE(UNIT,300) NMOD  
300         FORMAT('0       NEW DELTA TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDTM+I),I=1,12),(C(KMDTM+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMDTM+13+4*I),C(KMDTM+14+4*I),
     A            C(KMDTM+15+4*I),C(KMDTM+16+4*I),I=1,IC(KMDTM+10))    
          ENDIF
 3      CONTINUE     
          IF(NP.EQ.0) WRITE(UNIT,304)
304       FORMAT('0 NO NEW MUON DELTA TIMES '/)
      ENDIF


  999 RETURN
      END
