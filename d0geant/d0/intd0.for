      LOGICAL FUNCTION INTD0()
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL MEND0

      INTD0=.TRUE.
 
      CALL KUNWG(   0)
      CALL KUCMD(' ','D0','C')
 
      CALL KUCMD('D0',' ','SW')
 
      CALL KUNWG(  74)
      CALL KUCMD(' ','DISPLAY_SWITCH','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISPLAY_SWITCH','DTRK','DTRK FOR TRACKING DISPLAY  -- 
     +>','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISPLAY_SWITCH','DHIT','DHIT FOR HITS              -- 
     +>','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DISPLAY_SWITCH','DDIG','DDIG FOR DIGITISATION (PRINT O
     +NLY)-- >','I','S')
      GUID(  1)='SETTING UP THE DISPLAY SWITCHES :'
      GUID(  2)='DTRK for tracking: (1=print each step)'
      GUID(  3)='                   (2=draw charged track'//
     +'s)'
      GUID(  4)='                   (3=print and draw)'
      GUID(  5)='DHIT for hits    : (1=print hits )'
      GUID(  6)='                   (2=draw  hits)'
      GUID(  7)='DDIG for digitization: (not impl.)'
      CALL KUGUID('DISPLAY_SWITCH',GUID,  7,'S')
      CALL KUACT('DISPLAY_SWITCH',MEND0)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
 
