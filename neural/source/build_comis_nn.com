$!========================================================================
$!
$! Name      : BUILD_COMIS_NN.COM
$!
$! Purpose   : Create a COMIS function to run a neural network
$!             specified by a weight file.
$!
$! Arguments :
$!             ComisFunctName   = p1
$!             Weightfile       = p2
$!             RcpFile          = p3
$!
$! Created  21-JUN-1995   Jeffrey E. McDonald
$! Modified  1-JUL-1995   Jeffrey E. McDonald
$! Modified  2-JUL-1995   Harrison B. Prosper
$!  If a UwFuncName exists use it Else RunPaw.
$! Modified 13-JAN-1996   Harrison B. Prosper
$!  Simplify COMIS function by using INCLUDE '?'
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   Version = "V1.0  1-July-1995"
$
$!================================================
$!   STEP 1:    ASK USER FOR
$!              NET OUTPUT
$!                  a)  Function to Create ComisFunctName
$!              NEURAL_BUILD
$!                  a)  Weight file name
$!                  b)  Optional:  RcpFile to get default variables
$!================================================
$   FdlFile         :== D0$NEURAL$SOURCE:BUILD_COMIS_NN.FDL
$   JNFEEDFILE      :== D0$NEURAL$SOURCE:JNFEED_FORWARD.FOR
$   GJNFUNCFILE     :== D0$NEURAL$SOURCE:GJNFUNC.FOR
$   Date            = F$EXTRACT(0,11,F$TIME())
$
$   ComisFunctName  = p1
$   Weightfile      = p2
$   RcpFile         = p3
$   IF ComisFunctName .EQS. ""
$   THEN
$       GOSUB INFORM
$   ENDIF
$!================================================
$!   check ComisFunctName
$!================================================
$  IF ComisFunctName .EQS. ""
$  THEN
$     INQUIRE   ComisFunctName  "Name of COMIS Function to build "
$  ENDIF
$  IF ComisFunctName .EQS. "" THEN GOTO EXIT
$!================================================
$! Check Weightfile
$!================================================
$CheckWeightFile:
$
$  IF Weightfile .EQS. ""
$  THEN
$      INQUIRE Weightfile "Weight File Name "
$  ENDIF
$  IF Weightfile .EQS. "" THEN GOTO EXIT
$  IF F$SEARCH(WeightFile) .NES. ""
$     THEN
$          GOTO CheckRCPFile
$     ELSE
$          wr "*** I could not find the weight file ''WeightFile'"
$          wr "*** Try again!"
$          GOTO EXIT
$  ENDIF
$!================================================
$!   Check RCP file
$!================================================
$CheckRCPFile:
$
$  IF RcpFile .EQS. ""
$  THEN
$      INQUIRE RcpFile  "Name of RCP file (or <CR> to call EVE) "
$  ENDIF
$  IF RcpFile .NES. ""
$  THEN
$      IF F$SEARCH(RcpFile) .NES. ""
$      THEN
$           GOTO CreateHeaders
$      ELSE
$          wr "*** I could not find the RCP file ''RcpFile'"
$          wr "*** So the editor will be called"
$          RcpFile = ""
$      ENDIF
$  ENDIF
$!================================================
$!   Create output fortran file with some standard
$!   headers
$!================================================
$CreateHeaders:
$
$   ComisFunctName  = F$PARSE(ComisFunctName,,,"NAME")
$   ComisFunction   = ComisFunctName + ".F"
$   NetFuncName     = "Compute"+ComisFunctName
$   NetFunction     = NetFuncName + ".FOR"
$
$!================================================
$!   Create header of COMIS Function
$!================================================
$   wr "Building COMIS function header"
$   COPY/NOLOG/NOCONFIRM NL: 'ComisFunction'
$   OPEN/APPEND outfortran   'ComisFunction'
$   WRITE outfortran "      REAL FUNCTION ''ComisFunctName'(XDUMMY)"
$   WRITE outfortran "*---------------------------------------------------------------------"
$   WRITE outfortran "* Purpose:    Compute network output directly from an ntuple"
$   WRITE outfortran "* Created:    ''Date' by BUILD_COMIS_NN  ''Version'"
$   WRITE outfortran "*---------------------------------------------------------------------"
$   WRITE outfortran "      IMPLICIT NONE "
$   WRITE outfortran "      REAL XDUMMY"
$   WRITE outfortran "      INCLUDE '?'     ! The correct COMMON block will be loaded by PAW"
$   WRITE outfortran "*---------------------------------------------------------------------"
$   CLOSE outfortran
$   CONVERT/FDL='FDLFILE'  'ComisFunction' 'ComisFunction'
$
$!================================================
$!   Create code fragment, that fills the input
$!   array to the neural network, from an RCP
$!   file if given, otherwise call EVE
$!================================================
$
$   wr "Building COMIS function body"
$   IF RcpFile .NES. ""
$       THEN
$           GOSUB ReadRcp
$       ELSE
$           GOSUB Editfile
$   ENDIF
$
$   wr "Building COMIS function tail"
$   OPEN/APPEND outfortran 'ComisFunction'
$   WRITE outfortran "      CALL ''NetFuncName'(OIN,OUT)"
$   WRITE outfortran "      ''ComisFunctName' = OUT"
$   WRITE outfortran "      RETURN"
$   WRITE outfortran "      END"
$   WRITE outfortran "*---------------------------------------------------------------------"
$   CLOSE outfortran
$   GOSUB NeuralBuild
$   GOSUB CatFiles
$   GOTO EXIT
$
$INFORM:
$!================================================
$!   INFORMATION ABOUT BUILD_COMIS_NN
$!================================================
$    wr :== WRITE SYS$OUTPUT
$    wr "       *****************************************************************************  "
$    wr ""
$    wr "            Welcome to BUILD_COMIS_NN -- COMIS Neural Network FUNCTION BUILDER        "
$    wr "            Version ''Version'"
$    wr ""
$    wr "            INPUTS: "
$    wr ""
$    wr "               NAME of COMIS function to build"
$    wr "               NAME of JETNET neural network weight file"
$    wr "               NAME of RCP file (optional)"
$    wr ""
$    wr "            If RCP file is not given, the EVE editor is called"
$    wr "            to allow you to enter the variables required for the"
$    wr "            network.
$    wr ""
$    wr "       **************************************************************  "
$    INQUIRE PAUSE "<CR> TO CONTINUE"
$RETURN
$
$EditFile:
$!================================================
$!   Give user interface to the file
$!   to edit array variable (no RCP file)
$!================================================
$   num = 1
$   OPEN/APPEND outfortran 'ComisFunction'
$   OPEN/READ   weight_in  'Weightfile'
$
$GETNEXTREC:
$
$   READ/END_OF_FILE=Closeweight weight_in record
$   Length = F$LENGTH(record)
$   I = F$LOCATE("nodes in layer number 1",record)
$   IF I .GE. Length THEN GOTO GETNEXTREC
$
$   READ/END_OF_FILE=Closeweight weight_in record
$   record = F$EDIT(record,"COLLAPSE,TRIM")
$   numvar = F$EXTRACT(0,1,record)
$
$Closeweight:
$   CLOSE weight_in
$
$   GOSUB Header
$
$   LOOP2:
$       WRITE outfortran "      OIN(''num') ="
$       num = num+1
$       IF num .GT. numvar THEN GOTO EDITOR
$       GOTO LOOP2

$   EDITOR:
$       CLOSE outfortran
$       write sys$output " "
$       write sys$output " Entering EVE to edit (incomplete) file ''ComisFunction'"
$       write sys$output " "
$       SPAWN EVE 'ComisFunction'
$RETURN
$
$Header:
$!================================================
$!   Some headers for the fortran file
$!================================================
$   IF RcpFile  .EQS. ""
$       THEN
$           WRITE outfortran "      REAL OIN(''numvar'), OUT"
$       ELSE
$           WRITE outfortran "      REAL OIN(''numrec'), OUT"
$   ENDIF
$   WRITE outfortran "*---------------------------------------------------------------------"
$RETURN
$
$ReadRcp:
$!================================================
$!   Get Variables from Default RCP FILE
$!================================================
$   gofield :== GOTO GetNextField
$   string = ""
$   OPEN/READ infile 'RcpFile'
$   CLOSE infile
$   OPEN/READ infile 'RcpFile'
$   numrec = 0
$
$GetNextRecord:
$
$   READ/END_OF_FILE=CloseRCP infile record  ! Read records until EOF
$   Length = F$LENGTH(record)
$   I = F$LOCATE ("PATTERNS_INPUTS",record)
$   IF I .GE. Length THEN Goto GetNextRecord
$
$GetNextField:
$
$   READ/END_OF_FILE=CloseRCP infile record  ! Read records until EOF
$   Length = F$LENGTH(record)
$   I = F$LOCATE ("\END",record)
$   IF I .LT. Length THEN Goto CloseRCP
$!================================================
$!   Check record for comments, ampersands and
$!   operators
$!================================================
$   IF F$LOCATE("*",record) .LT. Length THEN gofield
$   IF F$LOCATE("&",record) .LT. Length THEN gofield
$   IF F$EXTRACT(0,1,record) .EQS. "!" THEN gofield
$   J = F$LOCATE("!",record)
$   IF J .LT. Length THEN record = F$EXTRACT(0,J,record)
$!================================================
$!   remove leading and trailing blanks and
$!   remove blanks
$!================================================
$   record = F$EDIT(record,"COLLAPSE,TRIM")
$!================================================
$!   take out single quotation marks in order to
$!   easily work with variables replace with \
$!================================================
$   Length = F$LENGTH(record)
$   Length = Length - 2
$   record = F$EXTRACT(1,Length,record)
$   numrec = numrec + 1
$   string = string + record + "\"
$!================================================
$!   Now we have a record with fieldnames
$!================================================
$   Goto GetNextField
$
$CloseRCP:
$   Close infile
$   GOTO Writefile
$!================================================
$!  Write to file names and array values for rcp
$!================================================
$Writefile:
$   count = 0
$   number = 1
$   OPEN/APPEND outfortran 'ComisFunction'
$
$   GOSUB Header
$
$   LOOP:
$       elem = F$ELEMENT(count,"\",string)
$       elem2 =F$ELEMENT(number,"\",string)
$       IF elem2 .EQS. "\"
$           THEN
$               CLOSE outfortran
$               RETURN
$           ENDIF
$       WRITE outfortran "      OIN(''number') = ''elem'"
$       number = number + 1
$       count = count + 1
$       GOTO LOOP
$   CLOSE outfortran
$RETURN
$
$NeuralBuild:
$!================================================
$!   STEP 3:    DO NBUILD
$!================================================
$   wr "Building neural network subroutine... "
$   nbuild 'Weightfile' 'NetFuncName'
$RETURN
$
!================================================
$!   STEP 4:    CONCATENATE
$!================================================
$CatFiles:
$
$   wr "Appending neural network subroutine..."
$  TEST = 1
$  PASSOVER =0
$
$  OPEN/READ infile1 'NetFunction'
$  OPEN/READ infile2 'JNFEEDFILE'
$  OPEN/APPEND outfortran 'ComisFunction'
$
$READSUBRTNE:
$  READ/END_OF_FILE=SECOND infile1 record
$  Length = F$LENGTH(record)
$  IF TEST .EQ. 1  THEN J = F$LOCATE("REAL",record)
$  K = F$LOCATE("CALL JNFEED_FORWARD",record)
$  IF J .LT. Length
$       THEN
$           IF TEST .EQ. 1
$              THEN
$                WRITE outfortran "      INTEGER I,J,K,IL,MI,MIJ"
$                WRITE outfortran "      REAL GJN,BETA"
$                WRITE outfortran "C----------------------------------------------"
$                TEST = 0
$           ENDIF
$       ENDIF
$  IF K .LT. Length THEN GOTO SECOND
$  WRITE outfortran record
$  GOTO READSUBRTNE
$SECOND:
$     READ/END_OF_FILE=FINAL infile2 record
$     Length = F$LENGTH(record)
$     J = F$LOCATE("Get inverse",record)
$     IF J .LT. Length THEN GOTO FPRINT
$     GOTO SECOND
$     GOTO FINAL
$FPRINT:
$     WRITE outfortran record
$     PASSOVER = 0
$     READ/END_OF_FILE=FINAL infile2 record
$     Length = F$LENGTH(record)
$     J = F$LOCATE ("RETURN",record)
$     IF J .LT. Length THEN READ/END_OF_FILE=FINAL infile2 record
$GOTO FPRINT
$FINAL:
$  CLOSE outfortran
$  CLOSE infile1
$  CLOSE infile2
$  APPEND/NONEW_VERSION 'GJNFUNCFILE' 'ComisFunction'
$RETURN
$
$EXIT:
$!================================================
$!   Do Some Cleaning up
$!================================================
$  IF F$SEARCH("tmp.for") .NES. ""
$  THEN
$      DELETE/NOCONFIRM/NOLOG tmp.for;*
$  ENDIF
$  IF F$SEARCH("hdr.for") .NES. ""
$  THEN
$      DELETE/NOCONFIRM/NOLOG hdr.for;*
$  ENDIF
$  IF F$SEARCH(NetFunction) .NES. ""
$  THEN
$      DELETE/NOCONFIRM/NOLOG 'NetFunction';*
$  ENDIF
$  EXIT
