      SUBROUTINE PRMTIM(UNIT,LMTIM,NMTIM,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print contents of MTIM bank
C-
C-   Inputs  : UNIT  - Unit number for printout
C-             LMTIM - Bank address
C-             NMTIM - Module number ( if 0, print all mods )
C-             CFL   - Print OLD/NEW/CURRENT/ALL bank 
C-             IFL   - How  much to print
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-FEB-1989   J.Green
C-               JUL-89     J.Green more decimals in format
C-               May-90     J.Green corrected write statements
C-               Jul-91     J.Green   change I8 to I9 in header format
C-               Jan-92     J.Green   fix for mods with < 24 wires
C-               Apr-92     J.Green   new bank header
c-               Nov-93     J.Green   Unix compatability
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER UNIT,LMTIM,NMTIM,IFL
      INTEGER NWIRX
      CHARACTER CFL*(*)
      INTEGER GZMTIM,GZMTIM_N,GZMTIM_R,NMODS,NMOD,I,KMTIM,NP,
     A N1,N2
C----------------------------------------------------------------------
C               functions to get channel, wire, plane from position in bank
      INTEGER CHAN, WIRE, PLANE
      CHAN(I) = 4*I +2 -3*MOD(I,2)
      PLANE(I) = CHAN(I)/(4*NWIRX)
      WIRE(I) = 2*( (CHAN(I) - NWIRX*4*PLANE(I)) /8 )
C----------------------------------------------------------------------
      DATA NMODS/207/
C----------------------------------------------------------------------
      IF(NMTIM.EQ.0) THEN
        N1=1                       ! FIRST MODULE
        N2=NMODS                   ! SECOND MODULE
      ELSE
        N1=NMTIM
        N2=NMTIM
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC    LOOK FOR OLD CONSTANTS              
        NP=0
        DO 1 NMOD=N1,N2  
        KMTIM=GZMTIM_R(NMOD)
          IF(KMTIM.NE.0) THEN   
            NWIRX = IC(KMTIM+10)/IC(KMTIM+11)
            NP=1
            WRITE(UNIT,100) NMOD  
100         FORMAT('0    REFERENCE TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMTIM+I),I=1,12),(C(KMTIM+I),I=13,16)
101         FORMAT(' TYPE ',I13,' PERCENT GOOD ',I5,' RESOLUTION ',I12/
     A         ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' MODULE NUMBER  ',I4,' NO. CHANNELS ',I4,
     A         ' NO. PLANES',I3,' CABLE DELAY ',I6/
     A         ' AVERAGE T0S ', 2F9.2, ' AVERAGE SLOPES ', 2F9.4 )
            WRITE(UNIT,102) 
102         FORMAT('  CHN PLN WIRE      TIME ZERO     NS/COUNT    ',
     &            'T 0 ERROR      SLOPE ERROR'/)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMTIM+17+4*I),C(KMTIM+18+4*I),
     A            C(KMTIM+19+4*I),C(KMTIM+20+4*I),I=0,IC(KMTIM+10)-1)
103         FORMAT(1X,3I5,F13.2,F13.4,F13.2,F13.4)
          ENDIF 
 1      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,104)
104       FORMAT('0 NO REFERENCE MUON TIMES     '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
        NP=0
CCC    LOOK FOR CURRENT CONSTANTS
        DO 2 NMOD=N1,N2  
        KMTIM=GZMTIM(NMOD)
          IF(KMTIM.NE.0) THEN  
            NWIRX = IC(KMTIM+10)/IC(KMTIM+11)
            NP=1
            WRITE(UNIT,200) NMOD  
200         FORMAT('0   CURRENT TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMTIM+I),I=1,12),(C(KMTIM+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMTIM+17+4*I),C(KMTIM+18+4*I),
     A            C(KMTIM+19+4*I),C(KMTIM+20+4*I),I=0,IC(KMTIM+10)-1)
          ENDIF
 2      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,204)
204       FORMAT('0 NO CURRENT MUON TIMES     '/)
      ENDIF                  
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
        NP=0
CCC    LOOK FOR NEW CONSTANTS
        DO 3 NMOD=N1,N2   
        KMTIM=GZMTIM_N(NMOD)
          IF(KMTIM.NE.0) THEN 
            NWIRX = IC(KMTIM+10)/IC(KMTIM+11)
            NP=1
            WRITE(UNIT,300) NMOD  
300         FORMAT('0       NEW TIME BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMTIM+I),I=1,12),(C(KMTIM+I),I=13,16)
            WRITE(UNIT,102)
            WRITE(UNIT,103) (CHAN(I),PLANE(I),WIRE(I),
     &            C(KMTIM+17+4*I),C(KMTIM+18+4*I),
     A            C(KMTIM+19+4*I),C(KMTIM+20+4*I),I=0,IC(KMTIM+10)-1)
          ENDIF
 3      CONTINUE     
          IF(NP.EQ.0) WRITE(UNIT,304)
304       FORMAT('0 NO NEW MUON TIMES     '/)
      ENDIF
      RETURN
      END
