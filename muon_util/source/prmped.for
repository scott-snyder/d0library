      SUBROUTINE PRMPED(PRUNIT,LMPED,NMPED,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT MPED - MUON PEDESTAL BANKS
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LMPED - BANK ADDRESS
CC    NMPED - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    CFL TELLS WHETHER TO PRINT OUT OLD/NEW/CURRENT/ALL BANK
CC    NMPED IS MODULE NUMBER IF 0 THEN EVERY MODULE
CC
CC    DH 1-97      SET UP FOR BASEMENT SETUP           
CC    J.Green   11/88   change to count from 0 instead of 1
CC    J.Green    7/89   reformat to aline heads with data
C-    Jul-91    J.Green   change I8 to I9 in header format
C-    Jan-92    J.Green   fix for modules with < 24 wires
C-    Apr-92    J.Green   new bank header 
C-    Nov-93    J.Green   Unix compatability
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LMPED,NMPED,IFL
      INTEGER NWIRX
      CHARACTER CFL*(*)
      INTEGER GZMPED,GZMPED_N,GZMPED_R,NMODS,NMOD,I,KMPED,NP
      INTEGER J,N1,N2
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
C               functions to get wire and plane from position in bank
C                 assume 24 wires per plane
      INTEGER  WIRE, PLANE
      PLANE(I) = I/(4*NWIRX)
      WIRE(I) = 2*( (I - 4*NWIRX*PLANE(I)) /8 )
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      DATA NMODS/307/
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
      IF(NMPED.EQ.0) THEN
        N1=1                       ! FIRST MODULE
        N2=NMODS                   ! SECOND MODULE
      ELSE
        N1=NMPED
        N2=NMPED
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC                                           LOOK FOR OLD CONSTANTS 
        NP=0
        DO 1 NMOD=N1,N2  
        KMPED=GZMPED_R(NMOD)
          IF(KMPED.NE.0) THEN 
            NWIRX = IC(KMPED+10)/(4*IC(KMPED+11))
            NP=1
            WRITE(PRUNIT,100) NMOD  
100         FORMAT('0    REFERENCE PEDESTAL BANK FOR MUON MODULE ',I6/)
            WRITE(PRUNIT,101) (IC(KMPED+I),I=1,12),(C(KMPED+I),I=13,16)
101         FORMAT(' TYPE     ',I9,' PERCENT GOOD ',I5,' QUALITY  ',I14/
     A         ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' MODULE NUMBER  ',I4,' NO. CHANNELS ',I4,
     A         ' NO. PLANES',I3,' MUD1 VERSION ',I12/
     A         ' AVERAGE PEDS ', 4F9.2 )           
            WRITE(PRUNIT,102)
102         FORMAT('   PLN WIRE CHAN    PEDESTAL    WIDTH   CHAN    ',
     &            'PEDESTAL     WIDTH'/)
            DO J=1,IC(KMPED+10)/2
              I = 2*(J-1)
              WRITE(PRUNIT,103) PLANE(I),WIRE(I),I,
     A           C(KMPED+17+2*I), C(KMPED+18+2*I),I+1,
     &           C(KMPED+19+2*I), C(KMPED+20+2*I)
            ENDDO
103         FORMAT(1X,3I5,F12.1,F9.1,I7,F12.1,F10.1)
          ENDIF 
 1      CONTINUE
        IF(NP.EQ.0) WRITE(PRUNIT,104)
104     FORMAT('0 NO REFERENCE MUON PEDESTALS '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
        NP=0
CCC                                          LOOK FOR CURRENT CONSTANTS
        DO 2 NMOD=N1,N2
        KMPED=GZMPED(NMOD)
          IF(KMPED.NE.0) THEN  
            NWIRX = IC(KMPED+10)/(4*IC(KMPED+11))
            NP=1
            WRITE(PRUNIT,200) NMOD  
200         FORMAT('0   CURRENT PEDESTAL BANK FOR MUON MODULE ',I6/)
            WRITE(PRUNIT,101) (IC(KMPED+I),I=1,12),(C(KMPED+I),I=13,16)
            WRITE(PRUNIT,102)
            DO J=1,IC(KMPED+10)/2
              I = 2*(J-1)
              WRITE(PRUNIT,103) PLANE(I),WIRE(I),I,
     A         C(KMPED+17+2*I), C(KMPED+18+2*I),I+1,
     &         C(KMPED+19+2*I), C(KMPED+20+2*I)
            ENDDO
          ENDIF
 2      CONTINUE
        IF(NP.EQ.0) WRITE(PRUNIT,204)
204     FORMAT('0 NO CURRENT MUON PEDESTALS '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
        NP=0
CCC                                           LOOK FOR NEW CONSTANTS
        DO 3 NMOD=N1,N2   
        KMPED=GZMPED_N(NMOD)
          IF(KMPED.NE.0) THEN 
            NWIRX = IC(KMPED+10)/(4*IC(KMPED+11))
            NP=1
            WRITE(PRUNIT,300) NMOD  
300         FORMAT('0       NEW PEDESTAL BANK FOR MUON MODULE ',I6/)
            WRITE(PRUNIT,101) (IC(KMPED+I),I=1,12),(C(KMPED+I),I=13,16)
            WRITE(PRUNIT,102)
            DO J=1,IC(KMPED+10)/2
              I = 2*(J-1)
              WRITE(PRUNIT,103) PLANE(I),WIRE(I),I,
     A         C(KMPED+17+2*I), C(KMPED+18+2*I),I+1,
     &         C(KMPED+19+2*I), C(KMPED+20+2*I)
            ENDDO
          ENDIF
 3      CONTINUE     
        IF(NP.EQ.0) WRITE(PRUNIT,304)
304     FORMAT('0 NO NEW MUON PEDESTALS '/)
      ENDIF
      RETURN
      END
