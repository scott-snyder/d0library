!------------------------------------------------------------
!
! Name      : D0USER.MMS
!
! Purpose   : Update target: D0USER.TSP
!
! Created  14-AUG-97   08:33:27  USERLIB V6.00 18-FEB-1991
!------------------------------------------------------------
!

 
.FIRST
  @ D0$WARNING == 0
  @ D0$ERROR == 0
  @ D0$FATAL == 0
  @ ON WARNING THEN GOSUB ERROR_COUNT
 
.LAST
  @ EXIT
  @ ERROR_COUNT:
  @ IF $SEVERITY .EQ. 0 THEN D0$WARNING == D0$WARNING + 1
  @ IF $SEVERITY .EQ. 2 THEN D0$ERROR == D0$ERROR + 1
  @ IF $SEVERITY .EQ. 4 THEN D0$FATAL == D0$FATAL + 1
  @ ON WARNING THEN GOSUB ERROR_COUNT
  @ RETURN
  
.IFDEF DO_PRE
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Pre-processing Commands"
.ENDIF
  
  
 
.IFDEF DO_INTER
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Intermediate-processing Commands"
.ENDIF
  
  
 
.IFDEF DO_POST
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Post-processing Commands"
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_HOOKS.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_HOOKS.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_MENUS.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_MENUS.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_PBD.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_PBD.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_SETUP.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_SETUP.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]EVENT_DISPLAY.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]EVENT_DISPLAY.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]SETUP.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]SETUP.COM D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER.LNK") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER.LNK D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_LNK.CSH") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_LNK.CSH D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_MENUS.CSH") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_MENUS.CSH D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.MISC]D0USER_SETUP.CSH") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.MISC]D0USER_SETUP.CSH D0$RELEASE:[D0USER]
  @ IF F$SEARCH("D0$RELEASE:[D0USER.PHYSICS]DST.RCP") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[D0USER.PHYSICS]DST.RCP D0$RELEASE:[D0USER]
  @ PBD/FRAME=D0USER/NAME=ALL_DISPLAY/NOCOMP
  @ PBD/FRAME=D0USER/NAME=ALL_DUMP/NOCOMP
  @ FORT ALL_DISPLAY_D0USER
  @ FORT/OBJ=DEB_ALL_DISPLAY_D0USER/NOOP/DEBUG=ALL ALL_DISPLAY_D0USER
  @ FORT ALL_DUMP_D0USER
  @ FORT/OBJ=DEB_ALL_DUMP_D0USER/NOOP/DEBUG=ALL ALL_DUMP_D0USER
  @ set noon
  @ @ALL_DUMP_D0USER.LNK
  @ gosub ERROR_COUNT
.ENDIF
 
 
.IFDEF DO_OFFICIAL
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Officializing Commands"
  @ set noon
  @ @ALL_DUMP_D0USER.LNK
  @ gosub ERROR_COUNT
.ENDIF
 
.IFDEF D0LIB_RELEASE
D0USER.TSP : -
               D0$D0USER$MENU:D0H_OPTIONS.MENU,-
               D0$D0USER$MENU:DONE_WITH_DATA.MENU,-
               D0$D0USER$MENU:EXAMINE.MENU,-
               D0$D0USER$MENU:INTERRUPT.MENU,-
               D0$D0USER$MENU:SETUP.MENU,-
               D0$D0USER$MENU:SUMMARIES.MENU,-
               D0$D0USER:D0USER_GUIDE.MEM,-
               D0$D0USER:$(PREFIX)PHYSICS.OLB(-
                  ALL_DISPLAY_DDF=D0$SCRATCH:[D0USER]ALL_DISPLAY_DDF.OBJ,-
                  ALL_DISPLAY_DMP=D0$SCRATCH:[D0USER]ALL_DISPLAY_DMP.OBJ,-
                  ALL_DISPLAY_EVT=D0$SCRATCH:[D0USER]ALL_DISPLAY_EVT.OBJ,-
                  ALL_DISPLAY_EVZ=D0$SCRATCH:[D0USER]ALL_DISPLAY_EVZ.OBJ,-
                  ALL_DISPLAY_INI=D0$SCRATCH:[D0USER]ALL_DISPLAY_INI.OBJ,-
                  ALL_DISPLAY_PAR=D0$SCRATCH:[D0USER]ALL_DISPLAY_PAR.OBJ,-
                  CADEFD=D0$SCRATCH:[D0USER]CADEFD.OBJ,-
                  CANALI=D0$SCRATCH:[D0USER]CANALI.OBJ,-
                  CANINI=D0$SCRATCH:[D0USER]CANINI.OBJ,-
                  DST_ANALYSIS=D0$SCRATCH:[D0USER]DST_ANALYSIS.OBJ,-
                  DST_DEFD=D0$SCRATCH:[D0USER]DST_DEFD.OBJ,-
                  DST_DUMP=D0$SCRATCH:[D0USER]DST_DUMP.OBJ,-
                  MCDATA=D0$SCRATCH:[D0USER]MCDATA.OBJ-
                  ),-
               D0$D0USER$STRUC:CATDIA.STRUC,-
               D0$D0USER$STRUC:CATEST.STRUC,-
               D0$D0USER$STRUC:CATNDR.STRUC,-
               D0$D0USER$STRUC:D0USER_0.STRUC,-
               D0$D0USER$STRUC:D0USER_1.STRUC,-
               D0$D0USER$STRUC:D0USER_2.STRUC,-
               D0$D0USER$STRUC:D0USER_3.STRUC,-
               D0$D0USER$STRUC:D0USER_4.STRUC,-
               D0$D0USER$STRUC:D0USER_5.STRUC,-
               D0$D0USER$STRUC:D0USER_6.STRUC,-
               D0$D0USER$STRUC:D0USER_7.STRUC,-
               D0$D0USER:$(PREFIX)FRAME.OLB(-
                  D0USER=D0$SCRATCH:[D0USER]D0USER.OBJ,-
                  DIAL_EVENTS=D0$SCRATCH:[D0USER]DIAL_EVENTS.OBJ,-
                  DISHIS=D0$SCRATCH:[D0USER]DISHIS.OBJ,-
                  DISUSR=D0$SCRATCH:[D0USER]DISUSR.OBJ,-
                  ENDJOB=D0$SCRATCH:[D0USER]ENDJOB.OBJ,-
                  ENDRUN=D0$SCRATCH:[D0USER]ENDRUN.OBJ,-
                  EVENTS=D0$SCRATCH:[D0USER]EVENTS.OBJ,-
                  EXAMIN=D0$SCRATCH:[D0USER]EXAMIN.OBJ,-
                  FINISH=D0$SCRATCH:[D0USER]FINISH.OBJ,-
                  GET_DAQ_EVENT=D0$SCRATCH:[D0USER]GET_DAQ_EVENT.OBJ,-
                  GOON=D0$SCRATCH:[D0USER]GOON.OBJ,-
                  HBDIAL=D0$SCRATCH:[D0USER]HBDIAL.OBJ,-
                  HISPAK=D0$SCRATCH:[D0USER]HISPAK.OBJ,-
                  INIFLG=D0$SCRATCH:[D0USER]INIFLG.OBJ,-
                  INIGEN=D0$SCRATCH:[D0USER]INIGEN.OBJ,-
                  INIGRA=D0$SCRATCH:[D0USER]INIGRA.OBJ,-
                  INIJOB=D0$SCRATCH:[D0USER]INIJOB.OBJ,-
                  INIMEN=D0$SCRATCH:[D0USER]INIMEN.OBJ,-
                  INIRUN=D0$SCRATCH:[D0USER]INIRUN.OBJ,-
                  INTERR=D0$SCRATCH:[D0USER]INTERR.OBJ,-
                  INTRPT=D0$SCRATCH:[D0USER]INTRPT.OBJ,-
                  MKRECO=D0$SCRATCH:[D0USER]MKRECO.OBJ,-
                  MTISTA=D0$SCRATCH:[D0USER]MTISTA.OBJ,-
                  MTITLE=D0$SCRATCH:[D0USER]MTITLE.OBJ,-
                  NEWRUN=D0$SCRATCH:[D0USER]NEWRUN.OBJ,-
                  NOMORE=D0$SCRATCH:[D0USER]NOMORE.OBJ,-
                  OSTRM=D0$SCRATCH:[D0USER]OSTRM.OBJ,-
                  OUT_ONLY_EVENTS=D0$SCRATCH:[D0USER]OUT_ONLY_EVENTS.OBJ,-
                  PROCES=D0$SCRATCH:[D0USER]PROCES.OBJ,-
                  PTUSEV=D0$SCRATCH:[D0USER]PTUSEV.OBJ,-
                  QUIT=D0$SCRATCH:[D0USER]QUIT.OBJ,-
                  RUNS_SUMMARY=D0$SCRATCH:[D0USER]RUNS_SUMMARY.OBJ,-
                  STDOUT=D0$SCRATCH:[D0USER]STDOUT.OBJ,-
                  STRHST=D0$SCRATCH:[D0USER]STRHST.OBJ,-
                  SUMARY=D0$SCRATCH:[D0USER]SUMARY.OBJ,-
                  UFLSET=D0$SCRATCH:[D0USER]UFLSET.OBJ,-
                  UREQST=D0$SCRATCH:[D0USER]UREQST.OBJ,-
                  USNVRN=D0$SCRATCH:[D0USER]USNVRN.OBJ,-
                  WRHEAD=D0$SCRATCH:[D0USER]WRHEAD.OBJ,-
                  WROSTR=D0$SCRATCH:[D0USER]WROSTR.OBJ,-
                  ZBINF2=D0$SCRATCH:[D0USER]ZBINF2.OBJ,-
                  ZBINPF=D0$SCRATCH:[D0USER]ZBINPF.OBJ,-
                  ZBOUTF=D0$SCRATCH:[D0USER]ZBOUTF.OBJ,-
                  ZEROEV=D0$SCRATCH:[D0USER]ZEROEV.OBJ-
                  )
     @ WRITE SYS$OUTPUT "  "
     @ WRITE SYS$OUTPUT " USERLIB - Updated target: D0USER.TSP"
 
D0$D0USER$MENU:D0H_OPTIONS.MENU      : -
   D0$D0USER$MENU:D0H_OPTIONS.SET
  
D0$D0USER$MENU:DONE_WITH_DATA.MENU   : -
   D0$D0USER$MENU:DONE_WITH_DATA.SET
  
D0$D0USER$MENU:EXAMINE.MENU  : -
   D0$D0USER$MENU:EXAMINE.SET
  
D0$D0USER$MENU:INTERRUPT.MENU        : -
   D0$D0USER$MENU:INTERRUPT.SET
  
D0$D0USER$MENU:SETUP.MENU    : -
   D0$D0USER$MENU:SETUP.SET
  
D0$D0USER$MENU:SUMMARIES.MENU        : -
   D0$D0USER$MENU:SUMMARIES.SET
  
D0$D0USER:D0USER_GUIDE.MEM   : -
   D0$D0USER$MISC:D0USER_GUIDE.RNO
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_DDF.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_DDF.FOR
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_DMP.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_DMP.FOR
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_EVT.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_EVT.FOR
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_EVZ.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_EVZ.FOR
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_INI.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_INI.FOR
  
D0$SCRATCH:[D0USER]ALL_DISPLAY_PAR.OBJ     : -
   D0$D0USER$PHYSICS:ALL_DISPLAY_PAR.FOR
  
D0$SCRATCH:[D0USER]CADEFD.OBJ        : -
   D0$D0USER$PHYSICS:CADEFD.FOR
  
D0$SCRATCH:[D0USER]CANALI.OBJ        : -
   D0$D0USER$PHYSICS:CANALI.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:ZLINKA.INC,-
   D0$INC:PI.DEF
  
D0$SCRATCH:[D0USER]CANINI.OBJ        : -
   D0$D0USER$PHYSICS:CANINI.FOR
  
D0$SCRATCH:[D0USER]DST_ANALYSIS.OBJ  : -
   D0$D0USER$PHYSICS:DST_ANALYSIS.FOR,-
   D0$INC:ZEBCOM.INC
  
D0$SCRATCH:[D0USER]DST_DEFD.OBJ      : -
   D0$D0USER$PHYSICS:DST_DEFD.FOR
  
D0$SCRATCH:[D0USER]DST_DUMP.OBJ      : -
   D0$D0USER$PHYSICS:DST_DUMP.FOR
  
D0$SCRATCH:[D0USER]MCDATA.OBJ        : -
   D0$D0USER$PHYSICS:MCDATA.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$LINKS:IZGEAN.LINK,-
   D0$LINKS:IZISAE.LINK,-
   D0$LINKS:IZFAKE.LINK,-
   D0$LINKS:IZRECO.LINK
  
D0$D0USER$STRUC:CATDIA.STRUC         : -
   D0$D0USER$STRUC:CATDIA.STR
  
D0$D0USER$STRUC:CATEST.STRUC         : -
   D0$D0USER$STRUC:CATEST.STR
  
D0$D0USER$STRUC:CATNDR.STRUC         : -
   D0$D0USER$STRUC:CATNDR.STR
  
D0$D0USER$STRUC:D0USER_0.STRUC       : -
   D0$D0USER$STRUC:D0USER_0.STR
  
D0$D0USER$STRUC:D0USER_1.STRUC       : -
   D0$D0USER$STRUC:D0USER_1.STR
  
D0$D0USER$STRUC:D0USER_2.STRUC       : -
   D0$D0USER$STRUC:D0USER_2.STR
  
D0$D0USER$STRUC:D0USER_3.STRUC       : -
   D0$D0USER$STRUC:D0USER_3.STR
  
D0$D0USER$STRUC:D0USER_4.STRUC       : -
   D0$D0USER$STRUC:D0USER_4.STR
  
D0$D0USER$STRUC:D0USER_5.STRUC       : -
   D0$D0USER$STRUC:D0USER_5.STR
  
D0$D0USER$STRUC:D0USER_6.STRUC       : -
   D0$D0USER$STRUC:D0USER_6.STR
  
D0$D0USER$STRUC:D0USER_7.STRUC       : -
   D0$D0USER$STRUC:D0USER_7.STR
  
D0$SCRATCH:[D0USER]D0USER.OBJ        : -
   D0$D0USER$FRAME:D0USER.FOR
  
D0$SCRATCH:[D0USER]DIAL_EVENTS.OBJ   : -
   D0$D0USER$FRAME:DIAL_EVENTS.FOR
  
D0$SCRATCH:[D0USER]DISHIS.OBJ        : -
   D0$D0USER$FRAME:DISHIS.FOR
  
D0$SCRATCH:[D0USER]DISUSR.OBJ        : -
   D0$D0USER$FRAME:DISUSR.FOR
  
D0$SCRATCH:[D0USER]ENDJOB.OBJ        : -
   D0$D0USER$FRAME:ENDJOB.FOR
  
D0$SCRATCH:[D0USER]ENDRUN.OBJ        : -
   D0$D0USER$FRAME:ENDRUN.FOR,-
   D0$INC:ZEBCOM.INC
  
D0$SCRATCH:[D0USER]EVENTS.OBJ        : -
   D0$D0USER$FRAME:EVENTS.FOR
  
D0$SCRATCH:[D0USER]EXAMIN.OBJ        : -
   D0$D0USER$FRAME:EXAMIN.FOR
  
D0$SCRATCH:[D0USER]FINISH.OBJ        : -
   D0$D0USER$FRAME:FINISH.FOR
  
D0$SCRATCH:[D0USER]GET_DAQ_EVENT.OBJ       : -
   D0$D0USER$FRAME:GET_DAQ_EVENT.FOR
  
D0$SCRATCH:[D0USER]GOON.OBJ  : -
   D0$D0USER$FRAME:GOON.FOR
  
D0$SCRATCH:[D0USER]HBDIAL.OBJ        : -
   D0$D0USER$FRAME:HBDIAL.FOR
  
D0$SCRATCH:[D0USER]HISPAK.OBJ        : -
   D0$D0USER$FRAME:HISPAK.FOR
  
D0$SCRATCH:[D0USER]INIFLG.OBJ        : -
   D0$D0USER$FRAME:INIFLG.FOR
  
D0$SCRATCH:[D0USER]INIGEN.OBJ        : -
   D0$D0USER$FRAME:INIGEN.FOR
  
D0$SCRATCH:[D0USER]INIGRA.OBJ        : -
   D0$D0USER$FRAME:INIGRA.FOR
  
D0$SCRATCH:[D0USER]INIJOB.OBJ        : -
   D0$D0USER$FRAME:INIJOB.FOR
  
D0$SCRATCH:[D0USER]INIMEN.OBJ        : -
   D0$D0USER$FRAME:INIMEN.FOR
  
D0$SCRATCH:[D0USER]INIRUN.OBJ        : -
   D0$D0USER$FRAME:INIRUN.FOR,-
   D0$INC:ZEBCOM.INC
  
D0$SCRATCH:[D0USER]INTERR.OBJ        : -
   D0$D0USER$FRAME:INTERR.FOR
  
D0$SCRATCH:[D0USER]INTRPT.OBJ        : -
   D0$D0USER$FRAME:INTRPT.FOR
  
D0$SCRATCH:[D0USER]MKRECO.OBJ        : -
   D0$D0USER$FRAME:MKRECO.FOR
  
D0$SCRATCH:[D0USER]MTISTA.OBJ        : -
   D0$D0USER$FRAME:MTISTA.FOR
  
D0$SCRATCH:[D0USER]MTITLE.OBJ        : -
   D0$D0USER$FRAME:MTITLE.FOR
  
D0$SCRATCH:[D0USER]NEWRUN.OBJ        : -
   D0$D0USER$FRAME:NEWRUN.FOR
  
D0$SCRATCH:[D0USER]NOMORE.OBJ        : -
   D0$D0USER$FRAME:NOMORE.FOR
  
D0$SCRATCH:[D0USER]OSTRM.OBJ         : -
   D0$D0USER$FRAME:OSTRM.FOR
  
D0$SCRATCH:[D0USER]OUT_ONLY_EVENTS.OBJ     : -
   D0$D0USER$FRAME:OUT_ONLY_EVENTS.FOR
  
D0$SCRATCH:[D0USER]PROCES.OBJ        : -
   D0$D0USER$FRAME:PROCES.FOR,-
   D0$INC:FATCOM.INC,-
   D0$PARAMS:FATPARA.DEF
  
D0$SCRATCH:[D0USER]PTUSEV.OBJ        : -
   D0$D0USER$FRAME:PTUSEV.FOR
  
D0$SCRATCH:[D0USER]QUIT.OBJ  : -
   D0$D0USER$FRAME:QUIT.FOR
  
D0$SCRATCH:[D0USER]RUNS_SUMMARY.OBJ  : -
   D0$D0USER$FRAME:RUNS_SUMMARY.FOR
  
D0$SCRATCH:[D0USER]STDOUT.OBJ        : -
   D0$D0USER$FRAME:STDOUT.FOR
  
D0$SCRATCH:[D0USER]STRHST.OBJ        : -
   D0$D0USER$FRAME:STRHST.FOR
  
D0$SCRATCH:[D0USER]SUMARY.OBJ        : -
   D0$D0USER$FRAME:SUMARY.FOR
  
D0$SCRATCH:[D0USER]UFLSET.OBJ        : -
   D0$D0USER$FRAME:UFLSET.FOR
  
D0$SCRATCH:[D0USER]UREQST.OBJ        : -
   D0$D0USER$FRAME:UREQST.FOR
  
D0$SCRATCH:[D0USER]USNVRN.OBJ        : -
   D0$D0USER$FRAME:USNVRN.FOR
  
D0$SCRATCH:[D0USER]WRHEAD.OBJ        : -
   D0$D0USER$FRAME:WRHEAD.FOR,-
   D0$INC:ZEBCOM.INC
  
D0$SCRATCH:[D0USER]WROSTR.OBJ        : -
   D0$D0USER$FRAME:WROSTR.FOR
  
D0$SCRATCH:[D0USER]ZBINF2.OBJ        : -
   D0$D0USER$FRAME:ZBINF2.FOR
  
D0$SCRATCH:[D0USER]ZBINPF.OBJ        : -
   D0$D0USER$FRAME:ZBINPF.FOR,-
   D0$INC:FATCOM.INC,-
   D0$PARAMS:FATPARA.DEF
  
D0$SCRATCH:[D0USER]ZBOUTF.OBJ        : -
   D0$D0USER$FRAME:ZBOUTF.FOR
  
D0$SCRATCH:[D0USER]ZEROEV.OBJ        : -
   D0$D0USER$FRAME:ZEROEV.FOR
  
 
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "%RELEASE-I-No main processing to be executed
 
.ENDIF