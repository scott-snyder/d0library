      SUBROUTINE PRMSTC(UNIT,LMSTC,NMSTC,CFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print contents of MSTC bank
C-
C-   Inputs  : UNIT  - Unit number for printout
C-             LMSTC - Bank address
C-             NMSTC - Module number 
C-             CFL   - Print OLD/NEW/CURRENT/ALL bank 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  jul-93   J.Green
C-   Revised  Mar-94   J.Green   Format change
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER UNIT,LMSTC,NMSTC
      INTEGER NWIRX
      CHARACTER CFL*(*)
      INTEGER GZMSTC,GZMSTC_N,GZMSTC_R,I,KMSTC,NP,
     A N1,N2
C----------------------------------------------------------------------
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC                                LOOK FOR OLD CONSTANTS              
        KMSTC=GZMSTC_R(NMSTC)
        IF(KMSTC.NE.0) THEN   
          WRITE(UNIT,100) NMSTC  
100       FORMAT('0  REFERENCE SCINT CONSTANTS BANK FOR MODULE ',I6/)
          WRITE(UNIT,101) (IC(KMSTC+I),I=1,12),(C(KMSTC+I),I=13,16)
101       FORMAT(' TYPE ',I13,' PERCENT GOOD ',I5,' RESOLUTION ',I12/
     A       ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A       ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A       ' MODULE NUMBER  ',I4,' NO. CHANNELS ',I4,
     A       ' SPARE    ',I3,' CABLE DELAY ',I6/
     A       ' AVERAGE T0S ', 2F9.2, ' AVERAGE SLOPES ', 2F9.4 )
          WRITE(UNIT,102) 
102       FORMAT('     PMT      TIME ZERO     NS/COUNT    ',
     &          'T 0 ERROR      SLOPE ERROR'/)
          WRITE(UNIT,103) (I,
     &          C(KMSTC+17+4*I),C(KMSTC+18+4*I),
     A          C(KMSTC+19+4*I),C(KMSTC+20+4*I),I=0,IC(KMSTC+10)-1)
103       FORMAT(1X,I5,F14.2,F14.4,F14.2,F14.4)
        ELSE
          WRITE(UNIT,104)
104       FORMAT('0 NO REFERENCE MUON TIMES     '/)
        ENDIF
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
CCC                                    LOOK FOR CURRENT CONSTANTS
        KMSTC=GZMSTC(NMSTC)
        IF(KMSTC.NE.0) THEN  
          WRITE(UNIT,200) NMSTC  
200       FORMAT('0  CURRENT SCINT CONSTANTS BANK FOR MUON MODULE ',I6/)
          WRITE(UNIT,101) (IC(KMSTC+I),I=1,12),(C(KMSTC+I),I=13,16)
          WRITE(UNIT,102)
          WRITE(UNIT,103) (I,
     &          C(KMSTC+17+4*I),C(KMSTC+18+4*I),
     A          C(KMSTC+19+4*I),C(KMSTC+20+4*I),I=0,IC(KMSTC+10)-1)
        ELSE
          WRITE(UNIT,204)
204       FORMAT('0 NO CURRENT MUON TIMES     '/)
        ENDIF
      ENDIF                  
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
CCC                                          LOOK FOR NEW CONSTANTS
        KMSTC=GZMSTC_N(NMSTC)
        IF(KMSTC.NE.0) THEN 
          WRITE(UNIT,300) NMSTC  
300       FORMAT('0 NEW SCINT CONSTANTS BANK FOR MUON MODULE ',I6/)
          WRITE(UNIT,101) (IC(KMSTC+I),I=1,12),(C(KMSTC+I),I=13,16)
          WRITE(UNIT,102)
          WRITE(UNIT,103) (I,
     &          C(KMSTC+17+4*I),C(KMSTC+18+4*I),
     A          C(KMSTC+19+4*I),C(KMSTC+20+4*I),I=0,IC(KMSTC+10)-1)
        ELSE
          WRITE(UNIT,304)
304       FORMAT('0 NO NEW MUON TIMES     '/)
        ENDIF
      ENDIF
      RETURN
      END
