$!========================================================================
$!
$! Name      : SETUP_NEURAL
$!
$! Purpose   : Setup logicals for Neural Network
$!
$! Arguments : None
$!
$! Created   4-NOV-1991   Boaz Klima, Harrison B. Prosper
$! Modified 21-AUG-1992   Harrison B. Prosper 
$!      Shorten logicals
$! Modified  5-FEB-1995   Harrison B. Prosper 
$!      Add call to setup_neural_build.com
$! Modified 28-JUN-1995   Harrison B. Prosper 
$!      Add symbols for neural_build and build_comis_nn
$! Modified 13-JAN-1996   Harrison B. Prosper 
$!      Tidy up -- add arguments to setup_user.com
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$!================================================
$!   General logicals
$!================================================
$   DEFINE/NOLOG    NEURAL_RCP          NEURAL.RCP
$   DEFINE/NOLOG    NEURAL_RCPE         NEURAL.RCPE
$   DEFINE/NOLOG    NEURAL_WEIGHTS      NEURAL.WEIGHT
$   DEFINE/NOLOG    FOR000  NL:
$   DEFINE/NOLOG    FOR003  NL:
$!================================================
$!   Training data  (See NEURAL_RCP)
$!================================================
$   DEFINE/NOLOG    TRAIN_OUT_FILE      NL:
$   DEFINE/NOLOG    TRAIN_HSAVE_FILE    TRAIN.HBOOK
$   
$   IF F$SEARCH("SETUP_USER.COM") .NES. ""
$   THEN
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT "  Execute @SETUP_USER"
$       WRITE SYS$OUTPUT "  "
$       @SETUP_USER "''p1'" "''P2'" "''P3'" "''P4'"
$   ELSE
$       WRITE SYS$OUTPUT  -
            "  Please define the logicals for your ntuple files in" 
$       WRITE SYS$OUTPUT  -
            "  a local procedure called SETUP_USER.COM"
$       WRITE SYS$OUTPUT "  "
$   ENDIF
$!================================================
$!   Symbols
$!================================================
$   NEU*RAL     :== $D0$NEURAL:NEURAL.EXE
$   DNEU*RAL    :== $D0$NEURAL:DEB_NEURAL.EXE
$   NB*UILD     :== $D0$NEURAL:NEURAL_BUILD.EXE
$   NM*AKE      :== @D0$NEURAL:BUILD_COMIS_NN
$   
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  Type "
$   WRITE SYS$OUTPUT "      NEURAL      to run NEURAL.EXE "
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  Type"
$   WRITE SYS$OUTPUT "      NBUILD  WeightFile SubRoutineName "
$   WRITE SYS$OUTPUT "                  to run NEURAL_BUILD.EXE"
$   WRITE SYS$OUTPUT "  Type"
$   WRITE SYS$OUTPUT "      NMAKE   WeightFile ComisFunctionName RcpFile"   
$   WRITE SYS$OUTPUT "                  to run BUILD_COMIS_NN.COM"
$   WRITE SYS$OUTPUT "  "
$EXIT:
$   EXIT
