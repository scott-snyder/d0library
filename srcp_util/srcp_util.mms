!------------------------------------------------------------
!
! Name      : SRCP_UTIL.MMS
!
! Purpose   : Update target: SRCP_UTIL.TSP
!
! Created  11-JUL-97   12:36:51  USERLIB V6.00 18-FEB-1991
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
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.TEST]SETUP.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.TEST]SETUP.COM D0$SRCP_UTIL:
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.TEST]SETUP_DISPATCH_BUILDER.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.TEST]SETUP_DISPATCH_BUILDER.COM D0$SRCP_UTIL:
.ENDIF
  
  
 
.IFDEF DO_INTER
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Intermediate-processing Commands"
.ENDIF
  
  
 
.IFDEF DO_POST
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Post-processing Commands"
  @ set noon
  @ @D0$LIBRARY_UTIL:MAKE_TEMP_GENERAL SRCP_UTIL NODEB
  @ gosub ERROR_COUNT
  @ DEFINE D0$GENERAL D0$RELEASE:[SRCP_UTIL]
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.DOCS]RCP_MANUAL.RNO") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.DOCS]RCP_MANUAL.RNO D0$SRCP_UTIL:
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.DOCS]RCP_MANUAL2.RNO") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.DOCS]RCP_MANUAL2.RNO D0$SRCP_UTIL:
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.DOCS]MAKE_RCP_MANUAL.COM") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.DOCS]MAKE_RCP_MANUAL.COM D0$SRCP_UTIL:
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.DOCS]EZ_SETUP_COMPACK.DOC") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.DOCS]EZ_SETUP_COMPACK.DOC D0$SRCP_UTIL:
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.DOCS]FLAGS_GUIDE.DOC") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.DOCS]FLAGS_GUIDE.DOC D0$SRCP_UTIL:
  @ set noon
  @ @D0$SRCP_UTIL:MAKE_RCP_MANUAL
  @ gosub ERROR_COUNT
  @ IF F$SEARCH("D0$RELEASE:[SRCP_UTIL.SRCP]DISPATCH_BUILDER.RCP") .NES. "" THEN -
  COPY/NOCONF/LOG D0$RELEASE:[SRCP_UTIL.SRCP]DISPATCH_BUILDER.RCP D0$SRCP_UTIL:
  @ set noon
  @ @D0$SRCP_UTIL$TEST:BUILD_EXES
  @ gosub ERROR_COUNT
  @ set noon
  @ @D0$LIBRARY_UTIL:MAKE_TEMP_GENERAL DELETE
  @ gosub ERROR_COUNT
.ENDIF
 
 
.IFDEF DO_OFFICIAL
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "Executing Officializing Commands"
.ENDIF
 
.IFDEF D0LIB_RELEASE
SRCP_UTIL.TSP : -
                  D0$SRCP_UTIL:$(PREFIX)SRCP_UTIL.OLB(-
                     FLGABO=D0$SCRATCH:[SRCP_UTIL]FLGABO.OBJ,-
                     FLGBK=D0$SCRATCH:[SRCP_UTIL]FLGBK.OBJ,-
                     FLGCHK=D0$SCRATCH:[SRCP_UTIL]FLGCHK.OBJ,-
                     FLGERR=D0$SCRATCH:[SRCP_UTIL]FLGERR.OBJ,-
                     FLGPR=D0$SCRATCH:[SRCP_UTIL]FLGPR.OBJ,-
                     FLGSET=D0$SCRATCH:[SRCP_UTIL]FLGSET.OBJ,-
                     FLGUBK=D0$SCRATCH:[SRCP_UTIL]FLGUBK.OBJ,-
                     FLGVAL=D0$SCRATCH:[SRCP_UTIL]FLGVAL.OBJ,-
                     FLSETS=D0$SCRATCH:[SRCP_UTIL]FLSETS.OBJ-
                     ),-
                  D0$SRCP_UTIL:$(PREFIX)SRCP_UTIL.OLB(-
                     EZSWAP=D0$SCRATCH:[SRCP_UTIL]EZSWAP.OBJ-
                     ),-
                  D0$SRCP_UTIL:$(PREFIX)SRCP_UTIL.OLB(-
                     EZGET_BANK_PARAMS=D0$SCRATCH:[SRCP_UTIL]EZGET_BANK_PARAMS.OBJ,-
                     EZ_STORE_NAME=D0$SCRATCH:[SRCP_UTIL]EZ_STORE_NAME.OBJ,-
                     INRCP=D0$SCRATCH:[SRCP_UTIL]INRCP.OBJ,-
                     INRCPE=D0$SCRATCH:[SRCP_UTIL]INRCPE.OBJ,-
                     RCP_TO_FZ=D0$SCRATCH:[SRCP_UTIL]RCP_TO_FZ.OBJ,-
                     SET_SWITCHES=D0$SCRATCH:[SRCP_UTIL]SET_SWITCHES.OBJ-
                     ),-
                  D0$SRCP_UTIL:$(PREFIX)SRCP_UTIL.OLB(-
                     BKCRCP=D0$SCRATCH:[SRCP_UTIL]BKCRCP.OBJ,-
                     EZACHV=D0$SCRATCH:[SRCP_UTIL]EZACHV.OBJ,-
                     EZADD=D0$SCRATCH:[SRCP_UTIL]EZADD.OBJ,-
                     EZASIZ=D0$SCRATCH:[SRCP_UTIL]EZASIZ.OBJ,-
                     EZBOOK=D0$SCRATCH:[SRCP_UTIL]EZBOOK.OBJ,-
                     EZCDAT=D0$SCRATCH:[SRCP_UTIL]EZCDAT.OBJ,-
                     EZCHAIN=D0$SCRATCH:[SRCP_UTIL]EZCHAIN.OBJ,-
                     EZCHEK=D0$SCRATCH:[SRCP_UTIL]EZCHEK.OBJ,-
                     EZCONVERT=D0$SCRATCH:[SRCP_UTIL]EZCONVERT.OBJ,-
                     EZCOPY=D0$SCRATCH:[SRCP_UTIL]EZCOPY.OBJ,-
                     EZCRCP=D0$SCRATCH:[SRCP_UTIL]EZCRCP.OBJ,-
                     EZCRCP_TO_SRCP=D0$SCRATCH:[SRCP_UTIL]EZCRCP_TO_SRCP.OBJ,-
                     EZDBUG=D0$SCRATCH:[SRCP_UTIL]EZDBUG.OBJ,-
                     EZDECODE=D0$SCRATCH:[SRCP_UTIL]EZDECODE.OBJ,-
                     EZDELETE=D0$SCRATCH:[SRCP_UTIL]EZDELETE.OBJ,-
                     EZDIR=D0$SCRATCH:[SRCP_UTIL]EZDIR.OBJ,-
                     EZDROP=D0$SCRATCH:[SRCP_UTIL]EZDROP.OBJ,-
                     EZDUMP=D0$SCRATCH:[SRCP_UTIL]EZDUMP.OBJ,-
                     EZEND=D0$SCRATCH:[SRCP_UTIL]EZEND.OBJ,-
                     EZERR=D0$SCRATCH:[SRCP_UTIL]EZERR.OBJ,-
                     EZERR_CHECK=D0$SCRATCH:[SRCP_UTIL]EZERR_CHECK.OBJ,-
                     EZFETCH=D0$SCRATCH:[SRCP_UTIL]EZFETCH.OBJ,-
                     EZFILL=D0$SCRATCH:[SRCP_UTIL]EZFILL.OBJ,-
                     EZFLIS=D0$SCRATCH:[SRCP_UTIL]EZFLIS.OBJ,-
                     EZFSIZ=D0$SCRATCH:[SRCP_UTIL]EZFSIZ.OBJ,-
                     EZGET=D0$SCRATCH:[SRCP_UTIL]EZGET.OBJ,-
                     EZGET1=D0$SCRATCH:[SRCP_UTIL]EZGET1.OBJ,-
                     EZGET2=D0$SCRATCH:[SRCP_UTIL]EZGET2.OBJ,-
                     EZGETA=D0$SCRATCH:[SRCP_UTIL]EZGETA.OBJ,-
                     EZGETC=D0$SCRATCH:[SRCP_UTIL]EZGETC.OBJ,-
                     EZGETC1=D0$SCRATCH:[SRCP_UTIL]EZGETC1.OBJ,-
                     EZGETC2=D0$SCRATCH:[SRCP_UTIL]EZGETC2.OBJ,-
                     EZGETD=D0$SCRATCH:[SRCP_UTIL]EZGETD.OBJ,-
                     EZGETI=D0$SCRATCH:[SRCP_UTIL]EZGETI.OBJ,-
                     EZGETN=D0$SCRATCH:[SRCP_UTIL]EZGETN.OBJ,-
                     EZGETNAME=D0$SCRATCH:[SRCP_UTIL]EZGETNAME.OBJ,-
                     EZGETR=D0$SCRATCH:[SRCP_UTIL]EZGETR.OBJ,-
                     EZGETS=D0$SCRATCH:[SRCP_UTIL]EZGETS.OBJ,-
                     EZGETT=D0$SCRATCH:[SRCP_UTIL]EZGETT.OBJ,-
                     EZGET_NEXT_NAME=D0$SCRATCH:[SRCP_UTIL]EZGET_NEXT_NAME.OBJ,-
                     EZGET_NEXT_VALUE_TYPE=D0$SCRATCH:[SRCP_UTIL]EZGET_NEXT_VALUE_TYPE.OBJ,-
                     EZGET_NUMBER_STRINGS=D0$SCRATCH:[SRCP_UTIL]EZGET_NUMBER_STRINGS.OBJ,-
                     EZGET_SIZE=D0$SCRATCH:[SRCP_UTIL]EZGET_SIZE.OBJ,-
                     EZGET_VALUE_TYPE=D0$SCRATCH:[SRCP_UTIL]EZGET_VALUE_TYPE.OBJ,-
                     EZGNXT=D0$SCRATCH:[SRCP_UTIL]EZGNXT.OBJ,-
                     EZGREM=D0$SCRATCH:[SRCP_UTIL]EZGREM.OBJ,-
                     EZGSET=D0$SCRATCH:[SRCP_UTIL]EZGSET.OBJ,-
                     EZHDRC=D0$SCRATCH:[SRCP_UTIL]EZHDRC.OBJ,-
                     EZHDRI=D0$SCRATCH:[SRCP_UTIL]EZHDRI.OBJ,-
                     EZIN=D0$SCRATCH:[SRCP_UTIL]EZIN.OBJ,-
                     EZINIT=D0$SCRATCH:[SRCP_UTIL]EZINIT.OBJ,-
                     EZLOC=D0$SCRATCH:[SRCP_UTIL]EZLOC.OBJ,-
                     EZMAKE=D0$SCRATCH:[SRCP_UTIL]EZMAKE.OBJ,-
                     EZMERGE_BANKS=D0$SCRATCH:[SRCP_UTIL]EZMERGE_BANKS.OBJ,-
                     EZMERGE_PARAMS=D0$SCRATCH:[SRCP_UTIL]EZMERGE_PARAMS.OBJ,-
                     EZMOVE=D0$SCRATCH:[SRCP_UTIL]EZMOVE.OBJ,-
                     EZMOVE_ZEBCOM=D0$SCRATCH:[SRCP_UTIL]EZMOVE_ZEBCOM.OBJ,-
                     EZMRCP=D0$SCRATCH:[SRCP_UTIL]EZMRCP.OBJ,-
                     EZNAME=D0$SCRATCH:[SRCP_UTIL]EZNAME.OBJ,-
                     EZOUT=D0$SCRATCH:[SRCP_UTIL]EZOUT.OBJ,-
                     EZPAR=D0$SCRATCH:[SRCP_UTIL]EZPAR.OBJ,-
                     EZPICK=D0$SCRATCH:[SRCP_UTIL]EZPICK.OBJ,-
                     EZPICK_AND_SIGNAL=D0$SCRATCH:[SRCP_UTIL]EZPICK_AND_SIGNAL.OBJ,-
                     EZPICK_CHECK=D0$SCRATCH:[SRCP_UTIL]EZPICK_CHECK.OBJ,-
                     EZPICK_CHECK_STACK=D0$SCRATCH:[SRCP_UTIL]EZPICK_CHECK_STACK.OBJ,-
                     EZPICK_NOMSG=D0$SCRATCH:[SRCP_UTIL]EZPICK_NOMSG.OBJ,-
                     EZPRINT=D0$SCRATCH:[SRCP_UTIL]EZPRINT.OBJ,-
                     EZRDA=D0$SCRATCH:[SRCP_UTIL]EZRDA.OBJ,-
                     EZRDAR=D0$SCRATCH:[SRCP_UTIL]EZRDAR.OBJ,-
                     EZRDF=D0$SCRATCH:[SRCP_UTIL]EZRDF.OBJ,-
                     EZREAD=D0$SCRATCH:[SRCP_UTIL]EZREAD.OBJ,-
                     EZRNAM=D0$SCRATCH:[SRCP_UTIL]EZRNAM.OBJ,-
                     EZSETC=D0$SCRATCH:[SRCP_UTIL]EZSETC.OBJ,-
                     EZSETS=D0$SCRATCH:[SRCP_UTIL]EZSETS.OBJ,-
                     EZSHUNT=D0$SCRATCH:[SRCP_UTIL]EZSHUNT.OBJ,-
                     EZSIZE=D0$SCRATCH:[SRCP_UTIL]EZSIZE.OBJ,-
                     EZSQUEEZE=D0$SCRATCH:[SRCP_UTIL]EZSQUEEZE.OBJ,-
                     EZSRCP_TO_CRCP=D0$SCRATCH:[SRCP_UTIL]EZSRCP_TO_CRCP.OBJ,-
                     EZSTRG=D0$SCRATCH:[SRCP_UTIL]EZSTRG.OBJ,-
                     EZTELL=D0$SCRATCH:[SRCP_UTIL]EZTELL.OBJ,-
                     EZUDMP=D0$SCRATCH:[SRCP_UTIL]EZUDMP.OBJ,-
                     EZUNNAME=D0$SCRATCH:[SRCP_UTIL]EZUNNAME.OBJ,-
                     EZVERS=D0$SCRATCH:[SRCP_UTIL]EZVERS.OBJ,-
                     EZZAND=D0$SCRATCH:[SRCP_UTIL]EZZAND.OBJ,-
                     EZZBK=D0$SCRATCH:[SRCP_UTIL]EZZBK.OBJ,-
                     EZZCHK=D0$SCRATCH:[SRCP_UTIL]EZZCHK.OBJ,-
                     EZZCLK=D0$SCRATCH:[SRCP_UTIL]EZZCLK.OBJ,-
                     EZZDCD=D0$SCRATCH:[SRCP_UTIL]EZZDCD.OBJ,-
                     EZZDEC=D0$SCRATCH:[SRCP_UTIL]EZZDEC.OBJ,-
                     EZZDMP=D0$SCRATCH:[SRCP_UTIL]EZZDMP.OBJ,-
                     EZZDRC=D0$SCRATCH:[SRCP_UTIL]EZZDRC.OBJ,-
                     EZZEXT=D0$SCRATCH:[SRCP_UTIL]EZZEXT.OBJ,-
                     EZZGPT=D0$SCRATCH:[SRCP_UTIL]EZZGPT.OBJ,-
                     EZZGRM=D0$SCRATCH:[SRCP_UTIL]EZZGRM.OBJ,-
                     EZZIRC=D0$SCRATCH:[SRCP_UTIL]EZZIRC.OBJ,-
                     EZZLOC=D0$SCRATCH:[SRCP_UTIL]EZZLOC.OBJ,-
                     EZZNID=D0$SCRATCH:[SRCP_UTIL]EZZNID.OBJ,-
                     EZZRFM=D0$SCRATCH:[SRCP_UTIL]EZZRFM.OBJ,-
                     EZZSTO=D0$SCRATCH:[SRCP_UTIL]EZZSTO.OBJ,-
                     EZZWRT=D0$SCRATCH:[SRCP_UTIL]EZZWRT.OBJ,-
                     EZ_ADD_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_ADD_ARRAY.OBJ,-
                     EZ_ADD_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_ADD_ELEMENT.OBJ,-
                     EZ_ADD_PARAM=D0$SCRATCH:[SRCP_UTIL]EZ_ADD_PARAM.OBJ,-
                     EZ_ARRAY_LENGTH=D0$SCRATCH:[SRCP_UTIL]EZ_ARRAY_LENGTH.OBJ,-
                     EZ_BUILD_ALL_DISPATCH=D0$SCRATCH:[SRCP_UTIL]EZ_BUILD_ALL_DISPATCH.OBJ,-
                     EZ_BUILD_ONE_DISPATCH=D0$SCRATCH:[SRCP_UTIL]EZ_BUILD_ONE_DISPATCH.OBJ,-
                     EZ_COPY_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_COPY_ARRAY.OBJ,-
                     EZ_CVT_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_CVT_ELEMENT.OBJ,-
                     EZ_DISPATCH_BUILDER=D0$SCRATCH:[SRCP_UTIL]EZ_DISPATCH_BUILDER.OBJ,-
                     EZ_FILE_OPEN=D0$SCRATCH:[SRCP_UTIL]EZ_FILE_OPEN.OBJ,-
                     EZ_FILE_OPEN1=D0$SCRATCH:[SRCP_UTIL]EZ_FILE_OPEN1.OBJ,-
                     EZ_GET_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_GET_ARRAY.OBJ,-
                     EZ_GET_CHARS=D0$SCRATCH:[SRCP_UTIL]EZ_GET_CHARS.OBJ,-
                     EZ_GET_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_ELEMENT.OBJ,-
                     EZ_GET_FLOAT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT.OBJ,-
                     EZ_GET_FLOAT_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT_ARRAY.OBJ,-
                     EZ_GET_FLOAT_ELT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT_ELT.OBJ,-
                     EZ_GET_HELPER=D0$SCRATCH:[SRCP_UTIL]EZ_GET_HELPER.OBJ,-
                     EZ_GET_INTEGER=D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER.OBJ,-
                     EZ_GET_INTEGER_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER_ARRAY.OBJ,-
                     EZ_GET_INTEGER_ELT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER_ELT.OBJ,-
                     EZ_GET_LOGICAL=D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL.OBJ,-
                     EZ_GET_LOGICAL_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL_ARRAY.OBJ,-
                     EZ_GET_LOGICAL_ELT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL_ELT.OBJ,-
                     EZ_GET_MENUS_TITLES=D0$SCRATCH:[SRCP_UTIL]EZ_GET_MENUS_TITLES.OBJ,-
                     EZ_GET_NEXT_BUTTON=D0$SCRATCH:[SRCP_UTIL]EZ_GET_NEXT_BUTTON.OBJ,-
                     EZ_GET_NEXT_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_NEXT_ELEMENT.OBJ,-
                     EZ_GET_REM=D0$SCRATCH:[SRCP_UTIL]EZ_GET_REM.OBJ,-
                     EZ_GET_STRING=D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING.OBJ,-
                     EZ_GET_STRINGS=D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRINGS.OBJ,-
                     EZ_GET_STRING_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING_ARRAY.OBJ,-
                     EZ_GET_STRING_ELT=D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING_ELT.OBJ,-
                     EZ_MODIFY_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_ARRAY.OBJ,-
                     EZ_MODIFY_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_ELEMENT.OBJ,-
                     EZ_MODIFY_PARAM=D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_PARAM.OBJ,-
                     EZ_PUT_FIFO=D0$SCRATCH:[SRCP_UTIL]EZ_PUT_FIFO.OBJ,-
                     EZ_READ_RCP=D0$SCRATCH:[SRCP_UTIL]EZ_READ_RCP.OBJ,-
                     EZ_REMOVE_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_REMOVE_ELEMENT.OBJ,-
                     EZ_REMOVE_PARAM=D0$SCRATCH:[SRCP_UTIL]EZ_REMOVE_PARAM.OBJ,-
                     EZ_RENAME_PARAM=D0$SCRATCH:[SRCP_UTIL]EZ_RENAME_PARAM.OBJ,-
                     EZ_REPLACE_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_REPLACE_ARRAY.OBJ,-
                     EZ_SET_ARRAY=D0$SCRATCH:[SRCP_UTIL]EZ_SET_ARRAY.OBJ,-
                     EZ_SKIP_ELEMENT=D0$SCRATCH:[SRCP_UTIL]EZ_SKIP_ELEMENT.OBJ,-
                     PRCRCP=D0$SCRATCH:[SRCP_UTIL]PRCRCP.OBJ,-
                     RCPCHECK=D0$SCRATCH:[SRCP_UTIL]RCPCHECK.OBJ-
                     ),-
                  D0$SRCP_UTIL:$(PREFIX)SRCP_UTIL.OLB(-
                     TESTRCP=D0$SCRATCH:[SRCP_UTIL]TESTRCP.OBJ-
                     )
     @ WRITE SYS$OUTPUT "  "
     @ WRITE SYS$OUTPUT " USERLIB - Updated target: SRCP_UTIL.TSP"
 
D0$SCRATCH:[SRCP_UTIL]FLGABO.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGABO.FOR
  
D0$SCRATCH:[SRCP_UTIL]FLGBK.OBJ      : -
   D0$SRCP_UTIL$FLAGS:FLGBK.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGCHK.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGCHK.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGERR.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGERR.FOR,-
   D0$INC:FLAGS.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGPR.OBJ      : -
   D0$SRCP_UTIL$FLAGS:FLGPR.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGSET.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGSET.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGUBK.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGUBK.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLGVAL.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLGVAL.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]FLSETS.OBJ     : -
   D0$SRCP_UTIL$FLAGS:FLSETS.FOR,-
   D0$INC:FLAGS.INC,-
   D0$INC:FLAGNM.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSWAP.OBJ     : -
   D0$SRCP_UTIL$NONSTANDARD:EZSWAP.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGET_BANK_PARAMS.OBJ : -
   D0$SRCP_UTIL$OFFLINE:EZGET_BANK_PARAMS.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_STORE_NAME.OBJ    : -
   D0$SRCP_UTIL$OFFLINE:EZ_STORE_NAME.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]INRCP.OBJ      : -
   D0$SRCP_UTIL$OFFLINE:INRCP.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]INRCPE.OBJ     : -
   D0$SRCP_UTIL$OFFLINE:INRCPE.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]RCP_TO_FZ.OBJ  : -
   D0$SRCP_UTIL$OFFLINE:RCP_TO_FZ.FOR
  
D0$SCRATCH:[SRCP_UTIL]SET_SWITCHES.OBJ     : -
   D0$SRCP_UTIL$OFFLINE:SET_SWITCHES.FOR
  
D0$SCRATCH:[SRCP_UTIL]BKCRCP.OBJ     : -
   D0$SRCP_UTIL$SRCP:BKCRCP.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:ZLINKA.INC,-
   D0$INC:STP_ZLINKA.INC
  
D0$SCRATCH:[SRCP_UTIL]EZACHV.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZACHV.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZADD.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZADD.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZASIZ.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZASIZ.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZBOOK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZBOOK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZCDAT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZCDAT.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZCHAIN.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZCHAIN.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$LINKS:IZSRCP.LINK
  
D0$SCRATCH:[SRCP_UTIL]EZCHEK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZCHEK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZCONVERT.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZCONVERT.FOR,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:STP_ZLINKA.INC
  
D0$SCRATCH:[SRCP_UTIL]EZCOPY.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZCOPY.FOR,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:QUEST.INC
  
D0$SCRATCH:[SRCP_UTIL]EZCRCP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZCRCP.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:BFSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZCRCP_TO_SRCP.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZCRCP_TO_SRCP.FOR,-
   D0$INC:STP_ZLINKA.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZDBUG.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZDBUG.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZDECODE.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZDECODE.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZDELETE.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZDELETE.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZDIR.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZDIR.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZDROP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZDROP.FOR,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:NMSRCP.INC,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZDUMP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZDUMP.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:BFSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZEND.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZEND.FOR,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:BFSRCP.INC,-
   D0$INC:BFSRCP1.INC
  
D0$SCRATCH:[SRCP_UTIL]EZERR.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZERR.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZERR_CHECK.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZERR_CHECK.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZFETCH.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZFETCH.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:BFSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZFILL.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZFILL.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:BFSRCP.INC,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZFLIS.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZFLIS.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZFSIZ.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZFSIZ.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$UTIL:CLIST.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGET.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZGET.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGET1.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGET1.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGET2.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGET2.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGETA.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETA.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGETC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETC.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGETC1.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZGETC1.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGETC2.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZGETC2.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZGETD.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETD.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGETI.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETI.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGETN.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETN.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGETNAME.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZGETNAME.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGETR.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETR.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGETS.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETS.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZGETT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGETT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGET_NEXT_NAME.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZGET_NEXT_NAME.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:BFSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGET_NEXT_VALUE_TYPE.OBJ : -
   D0$SRCP_UTIL$SRCP:EZGET_NEXT_VALUE_TYPE.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZGET_NUMBER_STRINGS.OBJ : -
   D0$SRCP_UTIL$SRCP:EZGET_NUMBER_STRINGS.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZGET_SIZE.OBJ       : -
   D0$SRCP_UTIL$SRCP:EZGET_SIZE.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGET_VALUE_TYPE.OBJ : -
   D0$SRCP_UTIL$SRCP:EZGET_VALUE_TYPE.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZGNXT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGNXT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZGREM.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGREM.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZGSET.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZGSET.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZHDRC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZHDRC.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZHDRI.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZHDRI.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZIN.OBJ       : -
   D0$SRCP_UTIL$SRCP:EZIN.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$LINKS:IZSRCP.LINK
  
D0$SCRATCH:[SRCP_UTIL]EZINIT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZINIT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZLOC.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZLOC.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZMAKE.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZMAKE.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZMERGE_BANKS.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZMERGE_BANKS.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZMERGE_PARAMS.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZMERGE_PARAMS.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZMOVE.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZMOVE.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZMOVE_ZEBCOM.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZMOVE_ZEBCOM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$INC:ZEBCOM.INC
  
D0$SCRATCH:[SRCP_UTIL]EZMRCP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZMRCP.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZNAME.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZNAME.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$LINKS:IZSRCP.LINK,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZOUT.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZOUT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZPAR.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZPAR.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZPICK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZPICK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZPICK_AND_SIGNAL.OBJ : -
   D0$SRCP_UTIL$SRCP:EZPICK_AND_SIGNAL.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZPICK_CHECK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZPICK_CHECK.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZPICK_CHECK_STACK.OBJ : -
   D0$SRCP_UTIL$SRCP:EZPICK_CHECK_STACK.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZPICK_NOMSG.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZPICK_NOMSG.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZPRINT.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZPRINT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:BFSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZRDA.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZRDA.FOR,-
   D0$LINKS:IZSCPH.LINK,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZRDAR.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZRDAR.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZRDF.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZRDF.FOR,-
   D0$LINKS:IZSCPH.LINK,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZREAD.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZREAD.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZRNAM.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZRNAM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSETC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZSETC.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZSETS.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZSETS.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZSHUNT.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZSHUNT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSIZE.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZSIZE.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:BFSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSQUEEZE.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZSQUEEZE.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSRCP_TO_CRCP.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZSRCP_TO_CRCP.FOR,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZSTRG.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZSTRG.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZTELL.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZTELL.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZUDMP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZUDMP.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZUNNAME.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZUNNAME.FOR,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:NMSRCP.INC,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZVERS.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZVERS.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZZAND.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZAND.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZZBK.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZZBK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZCHK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZCHK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZCLK.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZCLK.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZDCD.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZDCD.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZZDEC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZDEC.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZZDMP.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZDMP.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZZDRC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZDRC.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZZEXT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZEXT.FOR,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZGPT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZGPT.FOR,-
   D0$LINKS:IZSRCP.LINK,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZGRM.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZGRM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZIRC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZIRC.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:BFSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZLOC.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZLOC.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZNID.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZNID.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZRFM.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZRFM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:NMSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZSTO.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZSTO.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:BFSRCP.INC,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZZWRT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZZWRT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_ADD_ARRAY.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_ADD_ARRAY.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_ADD_ELEMENT.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_ADD_ELEMENT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_ADD_PARAM.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_ADD_PARAM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_ARRAY_LENGTH.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_ARRAY_LENGTH.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_BUILD_ALL_DISPATCH.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_BUILD_ALL_DISPATCH.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_BUILD_ONE_DISPATCH.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_BUILD_ONE_DISPATCH.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_COPY_ARRAY.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZ_COPY_ARRAY.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_CVT_ELEMENT.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_CVT_ELEMENT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_DISPATCH_BUILDER.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_DISPATCH_BUILDER.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_FILE_OPEN.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_FILE_OPEN.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_FILE_OPEN1.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZ_FILE_OPEN1.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_ARRAY.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_GET_ARRAY.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_CHARS.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_GET_CHARS.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_ELEMENT.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_GET_ELEMENT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_GET_FLOAT.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT_ARRAY.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_FLOAT_ARRAY.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_FLOAT_ELT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_FLOAT_ELT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_HELPER.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZ_GET_HELPER.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_GET_INTEGER.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER_ARRAY.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_INTEGER_ARRAY.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_INTEGER_ELT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_INTEGER_ELT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_GET_LOGICAL.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL_ARRAY.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_LOGICAL_ARRAY.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_LOGICAL_ELT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_LOGICAL_ELT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_MENUS_TITLES.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_MENUS_TITLES.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_NEXT_BUTTON.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_NEXT_BUTTON.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_NEXT_ELEMENT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_NEXT_ELEMENT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_REM.OBJ       : -
   D0$SRCP_UTIL$SRCP:EZ_GET_REM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING.OBJ    : -
   D0$SRCP_UTIL$SRCP:EZ_GET_STRING.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRINGS.OBJ   : -
   D0$SRCP_UTIL$SRCP:EZ_GET_STRINGS.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING_ARRAY.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_STRING_ARRAY.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_GET_STRING_ELT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_GET_STRING_ELT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_ARRAY.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_MODIFY_ARRAY.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_ELEMENT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_MODIFY_ELEMENT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_MODIFY_PARAM.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_MODIFY_PARAM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_PUT_FIFO.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZ_PUT_FIFO.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:ZLINKA.INC
  
D0$SCRATCH:[SRCP_UTIL]EZ_READ_RCP.OBJ      : -
   D0$SRCP_UTIL$SRCP:EZ_READ_RCP.FOR
  
D0$SCRATCH:[SRCP_UTIL]EZ_REMOVE_ELEMENT.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_REMOVE_ELEMENT.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_REMOVE_PARAM.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_REMOVE_PARAM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_RENAME_PARAM.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_RENAME_PARAM.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]EZ_REPLACE_ARRAY.OBJ : -
   D0$SRCP_UTIL$SRCP:EZ_REPLACE_ARRAY.FOR,-
   D0$PARAMS:SRCP.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_SET_ARRAY.OBJ     : -
   D0$SRCP_UTIL$SRCP:EZ_SET_ARRAY.FOR,-
   D0$PARAMS:SRCP.DEF,-
   D0$INC:LKSRCP.INC,-
   D0$INC:ZEBSTP.INC,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]EZ_SKIP_ELEMENT.OBJ  : -
   D0$SRCP_UTIL$SRCP:EZ_SKIP_ELEMENT.FOR,-
   D0$OFFLINE_UTIL$GENERAL:VALUE.DEF
  
D0$SCRATCH:[SRCP_UTIL]PRCRCP.OBJ     : -
   D0$SRCP_UTIL$SRCP:PRCRCP.FOR,-
   D0$INC:ZEBCOM.INC,-
   D0$INC:ZEBSTP.INC
  
D0$SCRATCH:[SRCP_UTIL]RCPCHECK.OBJ   : -
   D0$SRCP_UTIL$SRCP:RCPCHECK.FOR
  
D0$SCRATCH:[SRCP_UTIL]TESTRCP.OBJ    : -
   D0$SRCP_UTIL$TEST:TESTRCP.FOR
  
 
NOTHING.NL : SYS$LOGIN:LOGIN.COM
  @ WRITE SYS$OUTPUT "%RELEASE-I-No main processing to be executed
 
.ENDIF