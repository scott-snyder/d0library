      SUBROUTINE MUSIM_BOOK_L1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books MUON TRIGGER histograms
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created:           Kamel Bazizi    10-15-92
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      LOGICAL SIMBK, HARDBK, ISABK
      INTEGER MULTBK
	CHARACTER*72 STRING
C
C-- Read flags from MUSIM.RPC to book histograms for hardware and/or
C   simulator.

      CALL EZPICK('MUSIM_RCP')
      CALL EZERR(IER)     ! Check if error
      IF(IER.EQ.0)THEN
        CALL EZGET('HARD_BK',HARDBK,IER)
        CALL EZGET('SIM_BK',SIMBK,IER)
        CALL EZGET('ISA_BK',ISABK,IER)
        CALL EZGET('MULT_BK',MULTBK,IER)
	CALL EZRSET()
      ELSE
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(' CANNOT PICK MUSIM.RCP',
     &          'MUSIM_BOOK_L1',STRING,'F')
        GOTO 999
      ENDIF

C
C-- Go to MUSIM directory in memory

      CALL HCDIR('//PAWC/MUSIM_L1',' ')

C
C-  SIMULATOR INFORMATION

        IF(SIMBK) THEN

        CALL HBOOK1(1002,' L1 MUON TRIGGER BITS - SIMULATOR$',
     &     16,1.,17.,0.)

C
C- Simulator octant fired
        CALL HBOOK1(1011,' Sim. CF OCT. 0,1,2,3,4,5,6,7$',
     &     8,0.,8.,0.)
        CALL HBOOK1(1012,' Sim. PW QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(1013,' Sim. SW QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(1014,' Sim. SAMUS QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(1015,' Sim. CCT OCTANTS : CF,PW,SW,SAMUS$',
     &     39,0.,39.,0.)

C-
        CALL HBOOK1(1021,' Sim. CCT TRIG. MULT. FOR ETA .LT. 1.0$',
     &     10,0.,10.,0.)
        CALL HBOOK1(1022,' Sim. CCT TRIG. MULT. FOR ETA .LT. 1.7$',
     &     10,0.,10.,0.)
        CALL HBOOK1(1023,' Sim. CCT TRIG. MULT. FOR ETA .LT. 2.4$',
     &     25,0.,25.,0.)
        CALL HBOOK1(1024,' Sim. CCT TRIG. MULT. FOR ETA .LT. 3.4$',
     &     25,0.,25.,0.)
        CALL HBOOK1(1025,' Sim. CCT TRIGGERS -
     & SN,ON,WN,CF,CF,WS,OS,SS$',8,-4.,4.,0.)
        CALL HBOOK1(1026,' Sim. 1 CCT RATIO -
     & SN,ON,WN,CF,CF,WS,OS,SS$',8,-4.,4.,0.)

C
C- CCT roads for monitoring
      CALL HBOOK1(1031,' CF TOP ROADS$'     , 90,0., 90.,0.)
      CALL HBOOK1(1032,' CF BOTTOM ROADS$'  , 70,0., 70.,0.)
      CALL HBOOK1(1033,' EF TOP ROADS$'     , 70,0., 70.,0.)
      CALL HBOOK1(1034,' EF BOTTOM ROADS$'  , 70,0., 70.,0.)
      CALL HBOOK1(1035,' SAM-WAM TOP ROADS$',180,0.,180.,0.)
      CALL HBOOK1(1036,' SAM-WAM BOT ROADS$',180,0.,180.,0.)
      CALL HBOOK1(1037,' SAMUS XXX ROADS$'  ,120,0.,120.,0.)
      CALL HBOOK1(1038,' SAMUS YYY ROADS$'  ,120,0.,120.,0.)
      CALL HBOOK1(1039,' SAMUS UU ROADS$'   , 82,0., 82.,0.)
C
C- CCT outputs ( distributions in theta )
      CALL HBOOK1(1041,' CF TOP CCT TRIGGER OUTPUT$',
     &     12,1.,13.,0.)
      CALL HBOOK1(1042,' CF BOTTOM CCT TRIGGER OUTPUT$',
     &     12,1.,13.,0.)
      CALL HBOOK1(1043,' EW TOP CCT TRIGGER OUTPUT$',
     &     6,1.,7.,0.)
      CALL HBOOK1(1044,' EW BOTTOM CCT TRIGGER OUTPUT$',
     &     6,1.,7.,0.)
      CALL HBOOK1(1045,' SAM-WAM TOP CCT TRIGGER OUTPUT$',
     &     7,0.,7.,0.)
      CALL HBOOK1(1046,' SAM-WAM BOTTOM CCT TRIGGER OUTPUT$',
     &     7,0.,7.,0.)                                
      CALL HBOOK1(1047,' SAMUS CCT TRIGGER OUTPUT (XXX)$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1048,' SAMUS CCT TRIGGER OUTPUT (YYY)$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1049,' SAMUS CCT TRIGGER OUTPUT (UU)$',
     &     32,0.,32.,0.)

C
C- CCT latch information 
      CALL HBOOK1(1051,' Sim. CF CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1052,' Sim. WN CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1053,' Sim. WS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1054,' Sim. ON CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1055,' Sim. OS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1056,' Sim. SN CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(1057,' Sim. SS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)

       ENDIF

C
C- HARDWARE INFORMATION

        IF(HARDBK) THEN

C
C- Trigger Framework information
        CALL HBOOK1(2001,' L1 MUON hardware TRIGGER BITS AT FRAMEWORK$',
     &     16,0.,16.,0.)
C- Global comparison
        CALL HBOOK1(2002,' L1 MUON TRIGGER BITS - HARDWARE$',
     &     16,1.,17.,0.)
C.. Specific trigger bits
        CALL HBOOK1(2003,' L1 SPECIFIC hardware TRIGGER BITS 0-31$',
     &     32,0.,32.,0.)

C
C- translate CCT latch info to octant fired
        CALL HBOOK1(2011,' Hard. CF OCT. 0,1,2,3,4,5,6,7$',
     &     8,0.,8.,0.)
        CALL HBOOK1(2012,' Hard. PW QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(2013,' Hard. SW QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(2014,' Hard. SAMUS QUAD. N:1,2,3,4 - S:1,2,3,4$',
     &     11,0.,11.,0.)
        CALL HBOOK1(2015,' Hard. CCT OCTANTS : CF,PW,SW,SAMUS$',
     &     39,0.,39.,0.)

C-
        CALL HBOOK1(2021,' Hard. CCT TRIG. MULT. FOR ETA .LT. 1.0$',
     &     10,0.,10.,0.)
        CALL HBOOK1(2022,' Hard. CCT TRIG. MULT. FOR ETA .LT. 1.7$',
     &     10,0.,10.,0.)
        CALL HBOOK1(2023,' Hard. CCT TRIG. MULT. FOR ETA .LT. 2.4$',
     &     25,0.,25.,0.)
        CALL HBOOK1(2024,' Hard. CCT TRIG. MULT. FOR ETA .LT. 3.4$',
     &     25,0.,25.,0.)
        CALL HBOOK1(2025,' Hard. CCT TRIGGERS -
     & SN,ON,WN,CF,CF,WS,OS,SS$',8,-4.,4.,0.)
        CALL HBOOK1(2026,' Hard. 1 CCT RATIO -
     & SN,ON,WN,CF,CF,WS,OS,SS$',8,-4.,4.,0.)

C- CCT latch information 
      CALL HBOOK1(2051,' Hard. CF CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2052,' Hard. WN CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2053,' Hard. WS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2054,' Hard. ON CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2055,' Hard. OS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2056,' Hard. SN CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)
      CALL HBOOK1(2057,' Hard. SS CCT LATCH OUTPUT$',
     &     32,0.,32.,0.)

        ENDIF

C
C- ISAJET INFORMATION

       IF(ISABK) THEN

      CALL HBOOK1(3001,'THETA - ISAJET $',72,0.,180.,0.)
      CALL HBOOK1(3002,'PHI - ISAJET $',72,0.,360.,0.)
      CALL HBOOK1(3003,'P - ISAJET $',60,0.,300.,0.)
      CALL HBOOK1(3004,'Pt - ISAJET $',50,0.,50.,0.)
      CALL HBOOK2(3005,'THETA vs PHI - ISAJET $'
     +           ,72,0.,180.,72,0.,360.,0.)
      CALL HBOOK1(3006,'ETA - ISAJET $',40,-4.0,4.0,0.)
      CALL HBOOK2(3007,'ETA vs PHI - ISAJET$'
     +           ,40,-4.,4.,72,0.,360.,0.)

      CALL HBOOK1(3101,'THETA ISAJET, GOOD CCT $',72,0.,180.,0.)
      CALL HBOOK1(3102,'PHI ISAJET, GOOD CCT $',72,0.,360.,0.)
      CALL HBOOK1(3103,'P ISAJET, GOOD CCT $',60,0.,300.,0.)
      CALL HBOOK1(3104,'Pt ISAJET, GOOD CCT $',50,0.,50.,0.)
      CALL HBOOK2(3105,'THETA vs PHI ISAJET, GOOD  CCT $'
     +           ,72,0.,180.,72,0.,360.,0.)
      CALL HBOOK1(3106,'ETA - ISAJET, GOOD CCT $',40,-4.0,4.0,0.)
      CALL HBOOK2(3107,'ETA vs PHI - ISAJET, GOOD  CCT $'
     +           ,40,-4.,4.,72,0.,360.,0.)
      CALL HBOOK1(3015,' ISA CCT OCTANTS : CF,PW,SW,SAMUS$',
     &     39,0.,39.,0.)

      ENDIF

      IF (MULTBK.NE.0) CALL MU_BOOK_CC(MULTBK)
C- Return to the PAWC directory

      CALL HCDIR('//PAWC',' ')

C----------------------------------------------------------------------

  999 RETURN
      END
