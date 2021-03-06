C********************************************************************
C          TB90L2.INC
C
      INTEGER NUMFT,NUMOUTCONN,NUMOUTPIN,NUMINCONN,NENTMAX
      PARAMETER(NUMFT=8) 
      PARAMETER(NUMOUTCONN=24)
      PARAMETER(NUMOUTPIN=50)
      PARAMETER(NUMINCONN=24)
      PARAMETER(NENTMAX=3000)
C
      INTEGER EC_LAYER_NORTH(2,23,60)
      INTEGER EC_LAYER_SOUTH(2,23,60)
      INTEGER EC_ETA(2,23,60)
      INTEGER EC_TPHI(2,23,60)
      INTEGER CC_LAYER_NORTH(2,12,50)
      INTEGER CC_LAYER_SOUTH(2,12,50)
      INTEGER CC_ETA(2,12,50)
      INTEGER CC_TPHI(2,12,50)
      INTEGER JPRBRD(NUMFT,NUMOUTCONN)
      INTEGER JPRTOW(NUMFT,NUMOUTCONN,NUMOUTPIN)
      INTEGER JADC(NUMFT,NUMOUTCONN)
      INTEGER JBLS(NUMFT,NUMOUTCONN)
      INTEGER JTOW(NUMFT,NUMOUTCONN,NUMOUTPIN)
      INTEGER JDEP(NUMOUTCONN,NUMOUTPIN)
      INTEGER IFIC(24,25)
      INTEGER IFIP(24,25)
      INTEGER JFIC(24,25)
      INTEGER JFIP(24,25)
      INTEGER JFIC_EX(24,25)
      INTEGER JFIP_EX(24,25)
      INTEGER KFIC(24,25)
      INTEGER KFIP(24,25)
      INTEGER KFIC_EX(24,25)
      INTEGER KFIP_EX(24,25)
      INTEGER FT_INPUT_F0(NUMFT,NUMINCONN)
      INTEGER FT_INPUT_F1(NUMFT,NUMINCONN)
      INTEGER KFOC(24,60),KFOP(24,60),KGANG(24,60)
      INTEGER IFOC(24,60),IFOP(24,60),IGANG(24,60)
      INTEGER JFOC(24,60),JFOP(24,60),JGANG(24,60)
      INTEGER ARRAY1_SEQN(NENTMAX)
      INTEGER ARRAY1_ADC(NENTMAX)
      INTEGER ARRAY1_BLS(NENTMAX)
      INTEGER ARRAY1_TOW(NENTMAX)
      INTEGER ARRAY1_DEP(NENTMAX)
      INTEGER ARRAY1_ADCA(NENTMAX)
      INTEGER ARRAY1_FTBRD(NENTMAX)
      INTEGER ARRAY1_FTOC(NENTMAX)
      INTEGER ARRAY1_FTOP(NENTMAX)
      INTEGER ARRAY1_GANGINGS(NENTMAX)
      INTEGER ARRAY1_SECTOR(NENTMAX)
      INTEGER ARRAY1_ETA(NENTMAX)
      INTEGER ARRAY1_PHI(NENTMAX)
      INTEGER ARRAY1_LAYER(NENTMAX)
      INTEGER ELEC_SORT_SEQN(NENTMAX)
      INTEGER ELEC_SORT_ADC(NENTMAX)
      INTEGER ELEC_SORT_BLS(NENTMAX)
      INTEGER ELEC_SORT_TOW(NENTMAX)
      INTEGER ELEC_SORT_DEP(NENTMAX)
      INTEGER ELEC_SORT_ADCA(NENTMAX)
      INTEGER ELEC_SORT_FTBRD(NENTMAX)
      INTEGER ELEC_SORT_FTOC(NENTMAX)
      INTEGER ELEC_SORT_FTOP(NENTMAX)
      INTEGER ELEC_SORT_GANGINGS(NENTMAX)
      INTEGER ELEC_SORT_SECTOR(NENTMAX)
      INTEGER ELEC_SORT_ETA(NENTMAX)
      INTEGER ELEC_SORT_PHI(NENTMAX)
      INTEGER ELEC_SORT_LAYER(NENTMAX)
      INTEGER PHY_SORT_SEQN(NENTMAX)
      INTEGER PHY_SORT_ADC(NENTMAX)
      INTEGER PHY_SORT_BLS(NENTMAX)
      INTEGER PHY_SORT_TOW(NENTMAX)
      INTEGER PHY_SORT_DEP(NENTMAX)
      INTEGER PHY_SORT_ADCA(NENTMAX)
      INTEGER PHY_SORT_FTBRD(NENTMAX)
      INTEGER PHY_SORT_FTOC(NENTMAX)
      INTEGER PHY_SORT_FTOP(NENTMAX)
      INTEGER PHY_SORT_GANGINGS(NENTMAX)
      INTEGER PHY_SORT_SECTOR(NENTMAX)
      INTEGER PHY_SORT_ETA(NENTMAX)
      INTEGER PHY_SORT_PHI(NENTMAX)
      INTEGER PHY_SORT_LAYER(NENTMAX)
      INTEGER NENTRIES
      INTEGER NCHANGANG
      INTEGER NCHANSUSED
      INTEGER SEQN,ADC,BLS,TOW,DEP,ADCA,FTBRD,FTOC,FTOP,GANGINGS
      INTEGER SECTOR,ETA,PHI,LAYER
      INTEGER ICDMG_F01(60)
      INTEGER ICDMG_ETA(60)
      INTEGER ICD_ETA(3,50,2)
      INTEGER ICD_PHI(3,50,2)
C
      INTEGER*4 ARRAY1_MODID(NENTMAX)
      INTEGER*4 ELEC_SORT_MODID(NENTMAX)
      INTEGER*4 PHY_SORT_MODID(NENTMAX)
C
      CHARACTER*7 FTOUTCONN_NAME(NUMFT,NUMOUTCONN)
      CHARACTER*2 FT_TYPE(NUMFT)
      CHARACTER*7 FT_INPUT_CONTENTS(NUMFT,NUMINCONN)
      CHARACTER*2 MODID
      CHARACTER*4 ARRAY1_CMODID(NENTMAX)
      CHARACTER*3 ICDMG_MOD(60)
      CHARACTER*4 ICDMG_EO(60)
      CHARACTER*1 PBRDTYPE(0:95)
      CHARACTER*20 PBRDINPUT(0:95)
C
C&IF VAXVMS
C&      EQUIVALENCE(ARRAY1_CMODID,ARRAY1_MODID)
C&ELSE
C NOT EQUIVALENCED UNLESS VAX
C&ENDIF
C
      COMMON/TB90L2/EC_LAYER_NORTH,EC_LAYER_SOUTH,EC_ETA,EC_TPHI,
     &              CC_LAYER_NORTH,CC_LAYER_SOUTH,CC_ETA,CC_TPHI,
     &              JPRBRD,JPRTOW,JADC,JBLS,JTOW,JDEP,
     &              IFIC,IFIP,JFIC,JFIP,JFIC_EX,JFIP_EX,
     &              KFIC,KFIP,KFIC_EX,KFIP_EX,
     &              FT_INPUT_F0,FT_INPUT_F1,
     &              KFOC,KFOP,KGANG,
     &              IFOC,IFOP,IGANG,
     &              JFOC,JFOP,JGANG,
     &              NENTRIES, ARRAY1_SEQN,ARRAY1_ADC,ARRAY1_BLS,
     &              ARRAY1_TOW,ARRAY1_DEP,ARRAY1_ADCA,ARRAY1_FTBRD,
     &              ARRAY1_FTOC,ARRAY1_FTOP,ARRAY1_GANGINGS,
     &              ARRAY1_SECTOR,ARRAY1_ETA,ARRAY1_PHI,ARRAY1_LAYER,
     &              ELEC_SORT_SEQN,ELEC_SORT_ADC,ELEC_SORT_BLS,
     &              ELEC_SORT_TOW,ELEC_SORT_DEP,ELEC_SORT_ADCA,
     &              ELEC_SORT_FTBRD,
     &              ELEC_SORT_FTOC,ELEC_SORT_FTOP,ELEC_SORT_GANGINGS,
     &              ELEC_SORT_SECTOR,ELEC_SORT_ETA,ELEC_SORT_PHI,
     &              ELEC_SORT_LAYER,
     &              PHY_SORT_SEQN,PHY_SORT_ADC,PHY_SORT_BLS,
     &              PHY_SORT_TOW,PHY_SORT_DEP,PHY_SORT_ADCA,
     &              PHY_SORT_FTBRD,
     &              PHY_SORT_FTOC,PHY_SORT_FTOP,PHY_SORT_GANGINGS,
     &              PHY_SORT_SECTOR,PHY_SORT_ETA,PHY_SORT_PHI,
     &              PHY_SORT_LAYER,
     &              SEQN,ADC,BLS,TOW,DEP,ADCA,FTBRD,FTOC,FTOP,GANGINGS,
     &              SECTOR,ETA,PHI,LAYER,
     &              ICDMG_F01,ICDMG_ETA,
     &              ICD_ETA,ICD_PHI,
     &              NCHANGANG,NCHANSUSED
C
      COMMON/TB90l2C/FTOUTCONN_NAME,FT_TYPE,FT_INPUT_CONTENTS,
     &        MODID,ARRAY1_CMODID,ICDMG_MOD,ICDMG_EO,PBRDTYPE,PBRDINPUT
C
C-----------------------------------------------------------------------
C OUTPUT VARIABLES:
C  NENTRIES  - NUMBER ENTRIES IN TABLES TOTAL (INCLUDING GANGED CHANS)
C  NCHANGANG - NUMBER OF CHANNELS THAT HAVE GANGED INPUTS
C  NCHANSUSED - NUMBER OF CHANNELS USED
C
C OUTPUT ARRAYS:
C   ARRAY1_* IS IN FEEDTHRU INPUT ORDER
C   ELEC_SORT_* IS IN SEQUENTIAL ADC (OR PACKED ADC ADDRESS) ORDER
C   PHY_SORT_*  IS IN ETA,PHI,LAYER ORDER:
C
C  FOR EACH OF THE 3 TYPES OF ARRAYS THERE ARE THE ARRAYS:
C   ARRAY1_SEQN - SEQUENTIAL ADC #
C   ARRAY1_ADC  - ADC CARD # 0-11
C   ARRAY1_BLS  - BLS CARD # 0-7
C   ARRAY1_TOW  - HARDWARE TOWER # 0-3
C   ARRAY1_DEP  - HARDWARE DEPTH # 0-11
C   ARRAY1_ADCA - PACKED ADC ADDRESS (SAME AS 16 MSB OF ADC WORDS)
C   ARRAY1_FTBRD- FEEDTHRU BOARD # 1-8
C   ARRAY1_FTOC - FEEDTHRU OUTPUT CONNECTOR # 1-24
C   ARRAY1_FTOP - FEEDTHRU OUTPUT PIN # 1-49 (ODD NUMBER ONLY) 
C   ARRAY1_GANGINGS - # PADS GANGED TOGETHER, THAT GO TO 1 ADC CHANNEL
C   ARRAY1_SECTOR - HARDWARE SECTOR # (0-31)
C   ARRAY1_ETA  - SOFTWARE ETA INDEX
C   ARRAY1_PHI - SOFTWARE PHI INDEX
C   ARRAY1_LAYER - SOFTWARE LAYER INDEX
C-
C---------------------------------------------------------------------
C- ARRAYS: EC_LAYER_NORTH(IS,IC,IP) - LAYER # FOR NORTH CRYOSTAT
C                 EC_LAYER_SOUTH(IS,IC,IP) - LAYER # FOR SOUTH CRYOSTAT
C                 EC_ETA (IS,IC,IP)  - ETA VALUE
C                 EC_TPHI(IS,IC,IP)  - TEMPORARY PHI VALUE = 0 FOR F0
C                                                            1 FOR F1
C                                                            3 FOR F0,F1
C
C        ARRAY VALUES = 99 IF NOT THERE
C-       FT OUTPUT PIN #'S ARE REALLY PIN PAIR #'S
C-       FT INPUT PIN #'S ARE REALLY THE PIN #'S
C        IS - IS SECTOR TYPE EVEN OR ODD
C        IC - IS FT INPUT CONN. # (1-23)
C        IP - IS FT INPUT PIN # (1-60 - ODD PINS ONLY)
C        ETA & LAYER ARE IN SOFTWARE COORDINATES
C
C- ARRAYS: CC_LAYER_NORTH(IS,IC,IP) - LAYER # FOR NORTH CRYOSTAT
C                 CC_LAYER_SOUTH(IS,IC,IP) - LAYER # FOR SOUTH CRYOSTAT
C                 CC_ETA (IS,IC,IP)  - ETA VALUE
C                 CC_TPHI(IS,IC,IP)  - TEMPORARY PHI VALUE = 0 FOR F0
C                                                            1 FOR F1
C
C        ARRAY VALUES = 99 IF NOT THERE
C-       FT OUTPUT PIN #'S ARE REALLY PIN PAIR #'S
C-       FT INPUT PIN #'S ARE REALLY THE PIN #'S
C        IS - IS SECTOR TYPE EVEN(2) OR ODD(1)
C        IC - IS FT INPUT CONN. # (1-12)
C        IP - IS FT INPUT PIN # (1-50 - ODD PINS ONLY)
C        ETA & LAYER ARE IN SOFTWARE COORDINATES
C
C-
C- CC FEEDTHRU:
C- ARRAYS: IFIC(24,25) - FT INPUT CONN # AS FUNCTION OF FT OUTPUT 
C-                                 CONN. #, PIN # 
C-         IFIP(24,25) - FT INPUT PIN # AS FUNCTION OF FT OUTPUT
C-                                 CONN. #, PIN #
C-
C-        FT OUTPUT PIN #'S ARE REALLY PIN PAIR #'S
C-        FT INPUT PIN #'S ARE REALLY THE PIN #'S
C-        IFIC GOES FROM 1-24
C-        IFIP GOES FROM 1-49 - TRUE PIN # - ONLY ODD PINS USED
C
C-
C- EC EVEN FEEDTHRU:
C- ARRAYS: KFIC(24,25) - FT INPUT CONN # AS FUNCTION OF FT OUTPUT 
C-                              CONN. #, PIN # 
C-         KFIP(24,25) - FT INPUT PIN # AS FUNCTION OF FT OUTPUT
C-                                 CONN. #, PIN #
C-         KFIC_EX(24,25) - FT EXTRA INPUT CONN # AS FUNCTION OF FT 
C-                                 OUTPUT CONN. #, PIN # - GANGED CHANS.
C-         KFIP_EX(24,25) - FT EXTRA INPUT PIN # AS FUNCTION OF FT 
C-                                 OUTPUT CONN. #, PIN # - GANGED CHANS.
C-
C-        FT OUTPUT PIN #'S ARE REALLY PIN PAIR #'S
C-        FT INPUT PIN #'S ARE REALLY THE PIN #'S
C-        KFIC & KFIC_EX GO FROM 1-23
C-        KFIP & KFIP_EX GO FROM 1-59 - TRUE PIN # - ONLY ODD PINS USED
C
C-
C- EC ODD FEEDTHRU:
C- ARRAYS: JFIC(24,25) - FT INPUT CONN # AS FUNCTION OF FT OUTPUT 
C-                                 CONN. #, PIN # 
C-         JFIP(24,25) - FT INPUT PIN # AS FUNCTION OF FT OUTPUT
C-                                 CONN. #, PIN #
C-         JFIC_EX(24,25) - FT EXTRA INPUT CONN # AS FUNCTION OF FT 
C-                                 OUTPUT CONN. #, PIN # - GANGED CHANS.
C-         JFIP_EX(24,25) - FT EXTRA INPUT PIN # AS FUNCTION OF FT 
C-                                 OUTPUT CONN. #, PIN # - GANGED CHANS.
C-
C-        FT OUTPUT PIN #'S ARE REALLY PIN PAIR #'S
C-        FT INPUT PIN #'S ARE REALLY THE PIN #'S
C-        JFIC & JFIC_EX GO FROM 1-23
C-        JFIP & JFIP_EX GO FROM 1-59 - TRUE PIN # - ONLY ODD PINS USED
C
C- ARRAYS:
C-  FT_TYPE(8) - FT_TYPE (EC OR CC) FOR EACH OF THE 8 FT BOARDS - BLANK SPACES
C-               IF NOT FILLED
C-  FT_INPUT_CONTENTS(8,24) - CONTENTS (CHARACTER NAME) OF WHAT IS GOING
C-              INTO FT # (1-8), INPUT CONN. # (1-24) - FILLED WITH
C-              'NONE   ' IF THERE IS NOTHING THERE
C-  FT_INPUT_F0(8,24) - SECTOR # OF THE 'F0' PART OF THE CONNECTOR FOR
C-               FT #, INPUT CONN# - FILLED WITH 99 IF NOTHING THERE
C-  FT_INPUT_F1(8,24) - SECTOR # OF THE 'F1' PART OF THE CONNECTOR FOR
C-               FT #, INPUT CONN# - FILLED WITH 99 IF NOTHING THERE
C
C- ARRAYS:
C-  JPRBRD - PREAMP BOARD # FOR A GIVEN FT BOARD,FT OUTPUT CONNECTOR - GOES
C-           FROM 0-95
C-  JPRTOW - PREAMP TOWER # FOR A GIVEN FT BOARD,FT OUTPUT CONNECTOR,
C-           PIN # (ONLY ODD # PINS USED - #25 NOT USED) - GOES FROM 0-3
C-  JADC   - ADC # (FROM 0-11) FOR A GIVEN FT BOARD, FT OUTPUT CONN.
C-  JBLS   - BLS # (FROM 0-7) FOR A GIVEN FT BOARD, FT OUTPUT CONN.
C-  JTOW   - TOWER # (FROM 0-3) FOR A GIVEN FT BOARD,FT OUTPUT CONN.,
C-           PIN # (ONLY ODD # PINS USED - #25 NOT USED)
C-  JDEP   - DEPTH # (FROM 0-11) FOR A GIVEN FT OUTPUT CONN.,PIN # (ONLY
C-           ODD # PINS USED - #25 NOT USED)
C-  FTOUTPUTCONN_NAME - FT OUTPUT CONNECTOR NAME FOR A GIVEN FT BOARD,
C-           FT OUTPUT CONNECTOR
C-
C-  ALL INTEGERS SET TO 999 IF NOT FILLED - FTOUTPUTCONN_NAME SET TO 'NONE   '
C
C
C
C
C.. FOR EC_EVEN..........................................................
C  KFIC(24,25) - FT INPUT CONN. AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR #
C  KFIP(24,25) - FT INPUT PIN # AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR # 
C  KFOC(24,60) - FT OUTPUT CONN. AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  KFOP(24,60) - FT OUTPUT PIN # AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  KGANG(24,60)- NUMBER OF GANGINGS AS A FUNCTION OF FT OUTPUT CONN, 
C                FT OUTPUT PIN # (ODD) - SEVERAL INPUTS MAY GO TO 1 OUTPUT
C
C..INPUT CONN.(KFIC)FROM 1-23
C..INPUT PIN  (KFIP)FROM 1-60 (ODD)
C..OUTPUT CONN.(KFOC)FROM 1-24
C..OUTPUT PIN   FROM 1-60 (ODD)
C..
C..DEFAULT VALUES ARE = 0 (IF PIN NOT USED)
C
C.. FOR EC_ODD................................................................
C  JFIC(24,25) - FT INPUT CONN. AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR #
C  JFIP(24,25) - FT INPUT PIN # AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR # 
C  JFOC(24,60) - FT OUTPUT CONN. AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  JFOP(24,60) - FT OUTPUT PIN # AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  JGANG(24,60)- NUMBER OF GANGINGS AS A FUNCTION OF FT OUTPUT CONN, 
C                FT OUTPUT PIN # (ODD) - SEVERAL INPUTS MAY GO TO 1 OUTPUT
C
C..INPUT CONN.(JFIC)FROM 1-23
C..INPUT PIN  (JFIP)FROM 1-60 (ODD)
C..OUTPUT CONN.(JFOC)FROM 1-24
C..OUTPUT PIN   FROM 1-60 (ODD)
C..
C..DEFAULT VALUES ARE = 0 (IF PIN NOT USED)
C
C.. FOR CC.....................................................................
C  IFIC(24,25) - FT INPUT CONN. AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR #
C  IFIP(24,25) - FT INPUT PIN # AS FUNCTION OF OUTPUT CONN#, OUTPUT PIN PAIR # 
C  IFOC(24,60) - FT OUTPUT CONN. AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  IFOP(24,60) - FT OUTPUT PIN # AS FUNCTION OF INPUT CONN#, INPUT PIN # (ODD)
C  IGANG(24,60)- NUMBER OF GANGINGS AS A FUNCTION OF FT OUTPUT CONN, 
C                FT OUTPUT PIN # (ODD) - SEVERAL INPUTS MAY GO TO 1 OUTPUT
C
C..INPUT CONN.(IFIC)FROM 1-24
C..INPUT PIN  (IFIP)FROM 1-50 (ODD)
C..OUTPUT CONN.(IFOC)FROM 1-24
C..OUTPUT PIN   FROM 1-60 (ODD)
C..
C..DEFAULT VALUES ARE = 0 (IF PIN NOT USED)
C
C...ICD-MASSLESS GAP:  - GOES INTO A EC-EVEN F.T. INPUT CONN. #6 (FTBOARD #1)
C   ICDMG_MOD(60) - MODULE NAME FOR EACH OF THE 60 FT INPUT PINS (DEF='NON')
C   ICDMG_EO(6)   - EVEN OR ODD SECTOR FOR EACH OF THE 60 FT INPUTS(DEF='NONE')
C   ICDMG_F01     - 0 OR 1 FOR F0 OR F1 FOR EACH OF 60 FT INPUT PINS(DEF=999)
C   ICDMG_ETA     - ETA VALUE FOR EACH OF 60 FT INPUT PINS(DEF=999)
C
C..ICD: - GOES DIRECTLY INTO PREAMP BOARD
C         CABLE #1 GOES INTO PREAMP 72 TOWER 0,1
C         CABLE #2 GOES INTO PREAMP 72 TOWER 2,3
C         CABLE #3 GOES INTO PREAMP 73 TOWER 0,1
C         ONLY ODD PIN #'S USED!
C         GANG# = 1 OR 2
C         DEFAULT VALUES ARE 999 IF NO SIGNAL ON THAT CABLE
C    ICD_ETA(CABLE#,PIN#,GANG#) - SOFTWARE ETA INDEX
C    ICD_PHI(CABLE#,PIN#,GANG#) - SOFTWARE PHI INDEX
C
C  
C**************************************************************************
