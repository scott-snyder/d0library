      SUBROUTINE PRMDFT(UNIT,LMDFT,NMDFT,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print contents of MDFT bank
C-
C-   Inputs  : UNIT  - Unit number for printout
C-             LMDFT - Bank address
C-             NMDFT - Module number 
C-             CFL   - Print OLD/NEW/CURRENT/ALL bank 
C-             IFL   - How  much to print
C-   Outputs : 
C-   Controls: 
C-
C-   Created  9-91    D. HEDIN
C    DH 3/92 slight format change
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER UNIT,LMDFT,NMDFT,IFL
      CHARACTER CFL*(*)
      INTEGER GZMDFT,GZMDFT_N,GZMDFT_R,NMODS,NMOD,I,KMDFT,NP,
     A N1,N2
        N1=NMDFT
        N2=NMDFT
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'OLD') THEN                 
CCC    LOOK FOR OLD CONSTANTS              
        NP=0
        DO 1 NMOD=N1,N2  
        KMDFT=GZMDFT_R(NMOD)
          IF(KMDFT.NE.0) THEN 
            NP=1
            WRITE(UNIT,100) NMOD  
100     FORMAT('0    REFERENCE T->D BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDFT+I),I=1,10)
101     FORMAT(' TYPE     ',I9,' STATUS   ',I9,' QUALITY       ',I9/
     A         ' LOW RUN  ',I9,' HIGH RUN ',I9,' RUN GENERATED ',I9/
     A         ' DATE GENERATED ',I10,' TIME GENERATED ',I10/
     A         ' MODULE NUMBER  ',I4,' SPARE ',I4/
     A         ' CORRECTION VALUES '/)
            WRITE(UNIT,102) (C(KMDFT+10+I),I=1,21)
102   FORMAT(7F8.3)
          ENDIF 
 1      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,104)
104       FORMAT('0 NO REFERENCE MUON T-->D BANK     '/)
      ENDIF
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'CURRENT') THEN
        NP=0
CCC    LOOK FOR CURRENT CONSTANTS
        DO 2 NMOD=N1,N2  
        KMDFT=GZMDFT(NMOD)
          IF(KMDFT.NE.0) THEN  
            NP=1
            WRITE(UNIT,200) NMOD  
200     FORMAT('0   CURRENT T-->D BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDFT+I),I=1,10)
            WRITE(UNIT,102) (C(KMDFT+10+I),I=1,21)
          ENDIF
 2      CONTINUE
          IF(NP.EQ.0) WRITE(UNIT,204)
204       FORMAT('0 NO CURRENT MUON T-->D BANKS     '/)
      ENDIF                  
      IF(CFL.EQ.'ALL'.OR.CFL.EQ.'NEW') THEN
        NP=0
CCC    LOOK FOR NEW CONSTANTS
        DO 3 NMOD=N1,N2   
        KMDFT=GZMDFT_N(NMOD)
          IF(KMDFT.NE.0) THEN 
            NP=1
            WRITE(UNIT,300) NMOD  
300     FORMAT('0       NEW T-->D BANK FOR MUON MODULE ',I6/)
            WRITE(UNIT,101) (IC(KMDFT+I),I=1,10)
            WRITE(UNIT,102) (C(KMDFT+10+I),I=1,21)
          ENDIF
 3      CONTINUE     
          IF(NP.EQ.0) WRITE(UNIT,304)
304       FORMAT('0 NO NEW MUON T-->D BANKS     '/)
      ENDIF
      RETURN
      END
