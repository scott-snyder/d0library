C----------------------------------------------------------------------
C
C                   DDDDDDDD           0000   0
C                   D       D         0    0 0
C                   D        D       0      0
C                   D         D     0      0 0
C                   D         D     0     0  0
C                   D         D     0    0   0
C                   D         D     0   0    0
C                   D         D     0  0     0
C                   D        D       00     0
C                   D       D        00    0
C                   DDDDDDDD        0  0000
C
C
C----------------------------------------------------------------------
C
      PROGRAM D0FLAVOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce a different flavor of the input file name,
C-               by commenting/uncommenting lines tagged by a C&IF C&ENDIF
C-               sequence.
C-
C-   Inputs  : a file name as command argument.
C-             one ( and only one ) machine qualifier and at most one graphic
C-             qualifier to define the wanted flavor.
C-   Outputs : the modified file, with machine_dependent extension
C-   Controls: well, will see...
C-
C-   Created   3-OCT-1988   Olivier Callot
C-   Updated  15-NOV-1988   Olivier Callot  Add graphic choices...
C-   Updated  22-SEP-1989   Harrison B. Prosper
C-   Add D0CHECK in do_command
C-   Updated  10-JAN-1990   Serban D. Protopopescu
C-   added UFFARM, replaced FNAL_ACP with SIUNIX
C-   Updated  16-MAR-1990   Steve Adler
C-   Added the /OUTPUT qualifier
C-   Updated   3-MAR-1992   Rich Mueller
C-   Removed commenting of 'Implicit None' statements
C-   Added optional /EXPAND qualifier to expand include files
C-   Added C&ELSEIF capability
C-   Added generic flavors checking
C-   Added OPEN statement checking if /CHECK qualifier present
C-   Added /ONLINE, /OFFLINE, /LEVEL2 qualifiers
C-   Added and changed code for more detailed error reporting
C-   Added and changed code for improved inline comment handling
C-   Added commenting of blank lines
C-   Updated  22-DEC-1992   Hyon-Joo Kehayias for D0CHECK return status
C-   Updated   7-JUL-1997   Alan M. Jonckheere  Add ALFOSF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER stat, status, sevrty, st, dot, predot, dprdot
      INTEGER st_suc, st_inf, st_wrn, st_err, st_ferr
      INTEGER lib$spawn
      INTEGER lenocc, machi_pkd, opos, in_quot, in_cmnt, j, llen
      INTEGER length, lon, first, rd_unit, wr_unit, out_fl_len
      CHARACTER*140 line, upline, test_string, str_temp
      CHARACTER*140 spaces
      CHARACTER*255 file_name, input_file, out_file_name, output
      CHARACTER*80  def_input_file
      CHARACTER*12  clnum
      INTEGER nb_machines, machine, i, newlon, ifst, ilst, im
      INTEGER k,loutput,bracket,colon,dcolon,lnum,exl,cnl,mbl,cml,bll
      LOGICAL selected, process, debug, exp_inc, prc_stnd, satisfied
      LOGICAL offline, online, level2, elsfnd, warning, ok
      LOGICAL stnd_input, stnd_output, insert_arg, double_backslash
      INTEGER cnt,linput
C
      INTEGER nqual,vldim,pdim,pldim
      PARAMETER(nqual=38, vldim=2, pdim=2, pldim=2)
      CHARACTER*32 qualifier(nqual)
      LOGICAL present(nqual)
      CHARACTER*(255) param(pdim,pldim)
      INTEGER lenp(pdim,pldim),nplist(pdim),nparam
      CHARACTER*(255) value(nqual,vldim)
      INTEGER lenv(nqual,vldim),nvlist(nqual)
C
C&IF VAXVMS
      PARAMETER(st_suc=1,st_inf=3,st_wrn=0,st_err=2,st_ferr=4)
C&ELSE
C&      PARAMETER(st_suc=0,st_inf=0,st_wrn=1,st_err=2,st_ferr=2)
C&ENDIF
C
      PARAMETER( nb_machines= 12 )
      CHARACTER*8 machine_names(  0:nb_machines )
      CHARACTER*8 machine_exten(  0:nb_machines )
      INTEGER     machine_nml(    0:nb_machines )
      INTEGER     machine_extenl( 0:nb_machines )
      INTEGER     machine_proc(   0:nb_machines )
      DATA ( machine_names(k), machine_nml(k), machine_exten(k),
     &       machine_extenl(k), machine_proc(k), k=0,nb_machines ) /
     &     'CHECK',    5,   '.check',   6, 1,
     &     'VAXVMS',   6,   '.for',     4, 0,   ! 1
     &     'VAXELN',   6,   '.for_eln', 8, 0,   ! 2
     &     'IBM3090',  7,   '.fortran', 8, 1,   ! 4
     &     'AMDAHL',   6,   '.fortran', 8, 1,   ! 8
     &     'ETA10',    5,   '.for_eta', 8, 1,   ! 16
     &     'SIUNIX',   6,   '.f',       2, 1,   ! 32
     &     'IBMAIX',   6,   '.f',       2, 1,   ! 64
     &     'ULTRIX',   6,   '.f',       2, 1,   ! 128
     &     'UFFARM',   6,   '.for_uff', 8, 0,   ! 256
     &     'HPUX',     4,   '.f',       2, 1,   ! 512
     &     'SUNOS',    5,   '.f',       2, 1,   ! 1024
     &     'ALFOSF',   6,   '.f',       2, 1/   ! 2048
      INTEGER nb_generic
      PARAMETER(nb_generic=10)
      CHARACTER*8 generic_names(nb_generic)
      INTEGER gen_pkd(nb_generic),generic_nml(nb_generic)
C
C  The bit packing for the machines in the array gen_pkd in the data
C  statement below correspond to the order of the machine entries in the
C  preceding data statement above.  Element 1 of the array machine_names
C  (with the value 'VAXVMS') corresponds to the 0 bits in the values of the
C  gen_pkd array.  Element 2 of machine_names (with value 'VAXELN')
C  corresponds to the 1 bits in the gen_pkd values, and so on...
C  If a generic flavor is added or changed, check statements with the
C  variable machi_pkd for validity.
C
C  The generic names correspond as follows:
C    VAX     == VAXVMS, VAXELN
C    VMS     == VAXVMS
C    UNIX    == SIUNIX, IBMAIX, ULTRIX, HPUX, SUNOS, ALFOSF
C    VMXA    == AMDAHL, IBM3090
C    LITTLE  == VAXVMS, VAXELN, ULTRIX, ALFOSF
C    BIG     == SIUNIX, IBMAIX, AMDAHL, IBM3090, HPUX, SUNOS
C    ONLINE  == VAXVMS
C    OFFLINE == VAXVMS, SIUNIX, IBMAIX, ULTRIX, ALFOSF
C    LEVEL2  == VAXELN
C    ANY     == All machines listed in the data statement above
C
      DATA ( generic_names(k), generic_nml(k), gen_pkd(k),
     &       k=1,nb_generic )
     &   / 'VAX',      3,      3,
     &     'VMS',      3,      1,
     &     'UNIX',     4,      3808,
     &     'VMXA',     4,      12,
     &     'LITTLE',   6,      2179,
     &     'BIG',      3,      1644,
     &     'ONLINE',   6,      1,
     &     'OFFLINE',  7,      2273,
     &     'LEVEL2',   6,      2,
     &     'ANY',      3,      65535  /
      INTEGER nb_graph, graph, machi_ok, graph_ok
      PARAMETER( nb_graph= 2 )
      INTEGER           graph_nml ( 0:nb_graph )
      CHARACTER*8       graph_name( 0:nb_graph )
      DATA graph_name / ' ',    'DI3000',       'MGKS' /
      DATA graph_nml  /  1,      6,              4     /
C
C ****  Declare D0CHECK error message symbols
C
C&IF VAXVMS
      EXTERNAL D0CK_BADEXTN,D0CK_VIOLATES,D0CK_NOTFOUND,D0CK_CONFORMS
C&ELSE
C&ENDIF
C
C----------------------------------------------------------------------
C
C&IF VAXVMS
C  Open the error & status message output device
        CALL D0OPEN_TEXT( 0, 'sys$error', 'OFN', ok )
C&ELSE
C&ENDIF
C
C **** Extract information from the command line ****
C
C The following qualifiers do not allow values
C
C  Machine qualifier names
      DO i=1,nb_machines+1
        qualifier(i)=machine_names(i-1)
      ENDDO
C  Spares for machine qualifier names
      DO i=nb_machines+2,17
        qualifier(i)=' '
      ENDDO
C  Graphic qualifier names
      qualifier(18)='DI3000'
      qualifier(19)='MGKS'
C  Spares for graphic qualifier names
      qualifier(20)=' '
      qualifier(21)=' '
C  Minimum requirement qualifier names
      qualifier(22)='ONLINE'
      qualifier(23)='OFFLINE'
      qualifier(24)='LEVEL2'
C  Spares for minimum requirement qualifier names
      DO i=25,27
        qualifier(i)=' '
      ENDDO
C  Other qualifier names
      qualifier(28)='DEBUG'
      qualifier(29)='PROCESS'
      qualifier(30)='NOPROCESS'
      qualifier(31)='EXPAND'
      qualifier(32)='INSERT'
      qualifier(33)='NOINSERT'
C  Spares
      DO i=34,37
        qualifier(i)=' '
      ENDDO
C
C The following qualifier can take an optional value
      qualifier(38)='OUTPUT'
C
      CALL DECODE_CMD_LINE(qualifier,nqual,vldim,pdim,pldim,present,
     &                     value,lenv,nvlist,param,lenp,nplist,nparam)
C
      DO i=1,37
        IF( nvlist(i) .NE. 0 ) THEN
          WRITE(0,*) 'ERROR!  Value not allowed for qualifier '
     &           // qualifier(i)
          CALL EXIT(st_err)
        ENDIF
      ENDDO
      stnd_output = .false.
      wr_unit=2
      IF( present(38) .AND. nvlist(38) .EQ. 0 ) THEN
        stnd_output = .true.
        wr_unit=6
C&IF VAXVMS
C  Open the standard output device
        CALL D0OPEN_TEXT( wr_unit, 'sys$output', 'OFNL', ok )
C&ELSE
C&ENDIF
      ELSEIF( present(38) .AND. nvlist(38) .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one value allowed for qualifier '
     &         // qualifier(38)
        CALL EXIT(st_err)
      ENDIF
      IF( nvlist(38) .GT. 0 ) THEN
        output=value(38,1)
        loutput=lenv(38,1)
      ELSE
        output=' '
        loutput=1
      ENDIF
C  Use Cernlib routine to convert output file name to lower case
      CALL cutol(output(1:loutput))
C
      stnd_input = .false.
      IF( nparam .EQ. 0 ) THEN
        stnd_input = .true.
C&IF VAXVMS
C  Open the standard input device
        CALL D0OPEN_TEXT( 5, 'sys$input', 'IFN', ok )
C&ELSE
C&ENDIF
      ELSEIF( nparam .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one parameter allowed'
        CALL EXIT(st_err)
      ELSEIF( nplist(nparam) .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one file name allowed'
        CALL EXIT(st_err)
      ENDIF
      IF( nparam .GT. 0 ) THEN
        file_name=param(1,1)
        length=lenp(1,1)
      ELSE
        file_name=' '
        length=1
      ENDIF
C  Convert input file name to lower case
      CALL cutol(file_name(1:length))
C
      debug = present(28)
C
C  Check which machine qualifier selected and
C  make sure there is no more than one
      cnt=0
      DO i = 0 , nb_machines
        IF ( present(i+1) ) THEN
          machine=i
          cnt=cnt+1
        ENDIF
      ENDDO
      IF( cnt .EQ. 1 ) THEN
        GOTO 100
      ELSEIF( cnt .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one machine qualifier allowed'
        CALL EXIT(st_err)
      ENDIF
C
C  VAXVMS is the default machine qualifier
      machine=1
C
  100 CONTINUE
C
C  Check for any selected graphic qualifier
C  and make sure there are no more than one
      cnt=0
      DO i = 1 , nb_graph
        IF( present(17+i) ) THEN
          graph=i
          cnt=cnt+1
        ENDIF
      ENDDO
      IF( cnt .EQ. 1 ) THEN
        GOTO 120
      ELSEIF( cnt .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one graphic qualifier allowed'
        CALL EXIT(st_err)
      ENDIF
      graph = 0
  120 CONTINUE
C
C  Setup flag to indicate if %VAL(0) dummy arguments should be inserted where
C  subroutine and function arguments are missing
      IF( present(33) ) THEN
C  NOINSERT qualifier present
        insert_arg = .false.
      ELSEIF( present(32) .OR. present(8) ) THEN
C  INSERT qualifier present or IBMAIX qualifier selected
        insert_arg = .true.
      ELSE
        insert_arg = .false.
      ENDIF
C
C  Double backslashes for IBMAIX
C
      double_backslash = present(8)
C
C  Setup flags to indicate if inline comments are to be processed
      IF( present(30) ) THEN
C  NOPROCESS qualifier present
        process = .false.
        prc_stnd = .false.
      ELSEIF( present(29) .OR. machine_proc( machine ) .NE. 0 ) THEN
C  PROCESS qualifier present or implied as default for particular machine
        process = .true.
        prc_stnd = .true.
      ELSE
        process = .false.
        prc_stnd = .false.
      ENDIF
C
C  Setup flags to indicate if include files are to be expanded
      IF( present(31) ) THEN
        process = .true.
        exp_inc = .true.
      ELSE
        exp_inc = .false.
      ENDIF
C
C  Determine which minimum requirement qualifer present, if any
      cnt=0
      DO i=22,24
        IF( present(i) ) cnt=cnt+1
      ENDDO
      online = present(22)
      offline = present(23)
      level2 = present(24)
      IF( cnt .EQ. 0 .AND. machine .EQ. 0 ) THEN
C  Default if /CHECK qualifier present
        offline = .true.
      ELSEIF( cnt .GT. 1 ) THEN
        WRITE(0,*) 'ERROR!  Only one qualifier from the set' //
     &         ' (ONLINE, OFFLINE, LEVEL2) is allowed'
        CALL EXIT(st_err)
      ENDIF
C
C ** Command line information extraction and validity testing completed
C
C  Add the default ".for" extension to the
C  input file name, if it does not have one
C
C&IF VAXVMS
      st = index(file_name(1:length),']') + 1
C&ELSE
C&      DO st=length,1,-1
C&        IF( file_name(st:st) .EQ. '/' ) GOTO 125
C&      ENDDO
C&      st=1
C&  125 CONTINUE
C&ENDIF
      IF( index(file_name(st:length),'.') .EQ. 0 ) THEN
        input_file = file_name(1:length) // '.for'
        linput = length + 4
      ELSE
        input_file = file_name(1:length)
        linput = length
      ENDIF
C
      rd_unit = 10
      IF( .NOT. stnd_input ) THEN
C  Open the input file
        CALL D0OPEN_TEXT( rd_unit, input_file, 'IFN', ok )
        IF( .NOT. ok ) GOTO 980
      ENDIF
C
C  Extract the input file name root and extension from
C  the complete name and path.  The root name will be used
C  as the default root name for the output file.
C
C&IF VAXVMS
      IF (index(input_file(1:linput),']').NE.0) THEN
        bracket = index(input_file,']')
        def_input_file = input_file(bracket+1:)
      ELSE IF ( (index(input_file(1:linput),'::').EQ.0) .AND.
     &          (index(input_file(1:linput),':').NE.0) ) THEN
        colon = index(input_file,':')
        def_input_file = input_file(colon+1:)
      ELSE IF (index(input_file(1:linput),'::') .NE. 0) THEN
        dcolon = index(input_file,'::')
        def_input_file = input_file(dcolon+2:)
        IF (index(def_input_file,':') .NE. 0) THEN
          colon = index(def_input_file,':')
          def_input_file = def_input_file(colon+1:)
        end IF
      ELSE
        def_input_file = input_file
      ENDIF
C&ELSE
C&      IF( index(input_file(1:linput),'/') .NE. 0 ) THEN
C&        DO st=linput,1,-1
C&          IF( file_name(st:st) .EQ. '/' ) GOTO 130
C&        ENDDO
C&  130   CONTINUE
C&        def_input_file = input_file(st+1:)
C&      ELSE
C&        def_input_file = input_file
C&      ENDIF
C&ENDIF
C
C  Add the machine dependent extension default to
C  the output file name, if one is not present
C
C&IF VAXVMS
      st = index(output(1:loutput),']') + 1
C&ELSE
C&      DO st=loutput,1,-1
C&        IF( output(st:st) .EQ. '/' ) GOTO 135
C&      ENDDO
C&      st=1
C&  135 CONTINUE
C&ENDIF
      IF (index(output(st:loutput),'.').EQ.0) THEN
C  Use Cernlib routine LENOCC to get length a string without trailing spaces
        IF( lenocc(output(1:loutput)) .EQ. 0 ) THEN
          output = machine_exten(machine)(1:machine_extenl(machine))
          loutput = machine_extenl(machine)
        ELSE
          output = output(1:loutput)//
     &      machine_exten(machine)(1:machine_extenl(machine))
          loutput = loutput+machine_extenl(machine)
        ENDIF
        IF( machine_names(machine)(1:machine_nml(machine)) .EQ. 'VAXVMS'
     &      .AND. (process .OR. insert_arg) ) THEN
          output = output(1:loutput)//'_vms'
          loutput = loutput + 4
        ENDIF
      ENDIF
C
C  If the output file name does not contain a root name, then
C  insert the default root name, from the variable def_input_file
C
      dot = index(output(st:loutput),'.')
      predot = st + dot - 2
      IF( predot .EQ. 0 ) THEN
        dprdot = index(def_input_file,'.') - 1
        out_file_name = def_input_file(1:dprdot) // output(1:loutput)
        out_fl_len = dprdot + loutput
C&IF VAXVMS
      ELSEIF( output(predot:predot) .EQ. ']'
     &        .OR. output(predot:predot) .EQ. ':' ) THEN
C&ELSE
C&      ELSEIF( output(predot:predot) .EQ. '/' ) THEN
C&ENDIF
        dprdot = index(def_input_file,'.') - 1
        out_file_name = output(1:predot) //
     &            def_input_file(1:dprdot) // output(predot+1:loutput)
        out_fl_len = dprdot + loutput
      ELSE
        out_file_name = output(1:loutput)
        out_fl_len = loutput
      ENDIF
C
      IF( .NOT. stnd_output ) THEN
C  Open the output file
        CALL D0OPEN_TEXT( wr_unit, out_file_name, 'OFNL', ok )
        IF( .NOT. ok ) GOTO 970
      ENDIF
C
      IF( DEBUG ) WRITE(0,*) 'Process '//file_name(1:length)//' with '//
     &       machine_names(machine)//graph_name(graph)//
     &       ' flavor, Output on '//out_file_name(1:out_fl_len)
C
C **** Start processing the input file ****
C
C  Start the main loop
C
      lnum = 0
      bll = 0
      exl = 0
      cnl = 0
      cml = 0
      mbl = 0
      warning = .false.
  200 CONTINUE
      IF( stnd_input .AND. rd_unit .EQ. 10 ) THEN
C  Read from standard input
        READ( 5, 2000, end=900) line
      ELSE
        READ( rd_unit, 2000, end=900) line
      ENDIF
 2000 FORMAT(a)
      lon=lenocc(line)
      IF( lon .EQ. 0 ) THEN
C  Blank line -- Replace with a comment
        line = 'C<<'
        lon = 3
        IF( rd_unit .EQ. 10 ) THEN
C  Main input file -- Tally line count and blank line count
          lnum = lnum + 1
          bll = bll + 1
        ENDIF
        GOTO 300
      ENDIF
C
C  Use Cernlib routine CLTOU to convert a string to upper case
      upline = line
      CALL cltou(upline(1:lon))
      IF( rd_unit .EQ. 10 ) THEN
        lnum = lnum + 1
        WRITE(clnum,'(I10)') lnum
C Use Cernlib routine SPACES to remove leading spaces from a string
C and Cernlib routine LENOCC to get the length of a string
        clnum = spaces(clnum,0)
        llen = lenocc(clnum)
        IF( upline(1:1) .NE. 'C' ) THEN
          DO j=1,lon
            IF( upline(j:j) .NE. ' ' ) GOTO 202
          ENDDO
  202     CONTINUE
          IF( upline(j:j) .NE. '!' .OR. j .EQ. 6 ) THEN
C  Executable line
            exl = exl + 1
            IF( upline(6:6) .NE. ' ' ) cnl = cnl + 1
          ELSE
C  Line commented with a "!"
            cml = cml + 1
          ENDIF
        ELSEIF( upline(1:2) .NE. 'C&' ) THEN
C  Comment line
          cml = cml + 1
        ELSE
C  Machine block line
          mbl = mbl + 1
        ENDIF
      ENDIF
C
C  Check for "OPEN" statements if /CHECK qualifier present
C
      IF ( machine .EQ. 0 ) THEN
        opos=index(upline(1:lon),'OPEN')
        IF ( upline(1:1) .NE. 'C' .AND.
     &       (upline(j:j) .NE. '!' .OR. j .EQ. 6) ) THEN
C  Line is executable (not commented)
C  If not a continuation line, initialize quote flag (in_quot)
          IF( upline(6:6) .EQ. ' ' ) in_quot = 0
          in_cmnt = 0
          DO j = 1 , lon
            IF( line(j:j) .EQ. '''' .AND. j .GT. 6 ) in_quot = 1-in_quot
            IF( line(j:j) .EQ. '!' .AND. j .NE. 6
     &                       .AND. in_quot .EQ. 0 ) in_cmnt = 1
            IF( j .EQ. opos .AND. in_quot .EQ. 0
     &                      .AND. in_cmnt .EQ. 0 ) THEN
C  Found the consecutive characters "OPEN", which are not in
C  a quote or comment -- They may be part of a variable name,
C  a function name, a soubroutine name, or an "OPEN" statement
              IF( line(j+4:j+4) .EQ. ' ' .OR.
     &                                 line(j+4:j+4) .EQ. '(' ) THEN
                IF( j .EQ. 7 .OR. line(j-1:j-1) .EQ. ' ' .OR.
     &                                 line(j-1:j-1) .EQ. ')' ) THEN
                  WRITE(0,*) 'WARNING!  OPEN statement found in line '//
     &                    clnum(1:llen) // ' : ' // line(1:lon)
                  warning = .true.
                  GOTO 205
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C
  205 CONTINUE
      IF( upline(1:2) .EQ. 'C&' .AND. upline(3:5) .NE. 'IF ' ) THEN
        IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
        WRITE(0,*) 'ERROR!  Invalid C& line placement.  Expecting C&IF'
     &          // ' -- Line ' // clnum(1:llen) // ' : ' // line(1:lon)
        CALL EXIT(st_err)
      ENDIF
      IF( upline(1:5) .EQ. 'C&IF ' ) THEN
C  Start of a machine block
        first = 6
        satisfied = .false.
        elsfnd = .false.
        machi_pkd = 0
  215   CONTINUE
C
C  Use Cernlib function SPACES to remove all leading and internal
C  spaces in a string and use Cernlib function LENOCC to get the
C  length of a string without its trailing spaces
C
        IF( first .LE. lon ) THEN
C  Extract list of names from C&IF or C&ELSEIF clause
          test_string = spaces(upline(first:lon),0)
          newlon = lenocc(test_string(1:lon-first+1))
        ELSE
          newlon = 0
        ENDIF
C
C  Check that every element of the C&IF or C&ELSEIF clause is a valid
C  machine name, and set the flags machi_ok and graph_ok to tag if the
C  selected machine and/or graphic name(s) was found.
C  -1 means no such item, 0 = an item, but not the selected one, 1 = found
C
        machi_ok = -1
        graph_ok = -1
        ifst = 1
  245   CONTINUE
        IF( newlon .LT. ifst ) THEN
          IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
          WRITE(0,*) 'ERROR!  Missing name after the C&IF or C&ELSEIF '
     &        // 'statement in line '//clnum(1:llen)//' : '//line(1:lon)
          CALL EXIT(st_err)
        ENDIF
C
C  Check first (or next) element in name list
C
C  Find ending position of name in the list string
        ilst = index( test_string(ifst:newlon) ,',' )
        IF ( ilst .EQ. 0 ) THEN
          ilst = newlon
        ELSEIF( ilst .GT. 1 ) THEN
          ilst = ilst + ifst - 2
        ELSE
          IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
          WRITE(0,*) 'ERROR!  Missing name after the C&IF or C&ELSEIF '
     &        // 'statement in line '//clnum(1:llen)//' : '//line(1:lon)
          CALL EXIT(st_err)
        ENDIF
C
C  Check if element is a machine name
        DO im = 1 , nb_machines
          IF( test_string(ifst:ilst) .EQ.
     &      machine_names(im)(1:machine_nml(im))  ) GOTO 250
        ENDDO
C
C  Check if element is a generic machine name
        DO im = 1 , nb_generic
          IF( test_string(ifst:ilst) .EQ.
     &      generic_names(im)(1:generic_nml(im))  ) GOTO 255
        ENDDO
C
C  If element is first graphic name encountered, preset graphic flag to zero
        IF( graph_ok .LT. 0 ) graph_ok = 0
C
C  Check if element is really a graphic name
        DO im = 1 , nb_graph
          IF( test_string(ifst:ilst) .EQ.
     &      graph_name(im)(1:graph_nml(im))  ) GOTO 260
        ENDDO
C
        IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
        WRITE(0,*) 'ERROR!  Invalid machine or graphic name ' //
     &          test_string(ifst:ilst) // ' in line ' //
     &          clnum(1:llen) // ' : ' // line(1:lon)
        CALL EXIT(st_err)
  250   CONTINUE
        IF( machi_ok .LT. 0 ) machi_ok = 0
        IF( im .EQ. machine ) machi_ok = 1
C  machi_pkd is a bit packed variable to indicate which
C  machine names accounted for in a machine block
        machi_pkd = IOR(machi_pkd,ISHFT(1,im-1))
        GOTO 265
  255   CONTINUE
        IF( machi_ok .LT. 0 ) machi_ok = 0
C  gen_pkd is an array of packed variables, each indicating which machines
C  are represented by the corresponding generic machine name
        IF( IAND(gen_pkd(im),ISHFT(1,machine-1)) .NE. 0 ) machi_ok = 1
        machi_pkd = IOR(machi_pkd,gen_pkd(im))
        GOTO 265
  260   IF( im .EQ. graph ) graph_ok = 1
  265   IF( ilst .LT. newlon ) THEN
          ifst = ilst+2
          GOTO 245
        ENDIF
C
C  Determine if C&IF or C&ELSEIF test is valid : machi_ok <> 0, graph_ok <> 0
C
        IF ( machi_ok .NE. 0 .AND. graph_ok .NE. 0 ) THEN
          IF( debug ) WRITE(0,*) 'Accepted the test => '//line(1:lon)
          selected = .true.
          satisfied = .true.
        ELSE
          IF( debug ) WRITE(0,*) 'Skipped the test => '//line(1:lon)
          selected = .false.
        ENDIF
C
C  Process the next line(s) up to a C&ELSE, C&ELSEIF, or a C&ENDIF statement
C
  270   CONTINUE
        CALL writeln(wr_unit,line,lon)
  280   CONTINUE
        IF( stnd_input .AND. rd_unit .EQ. 10 ) THEN
C  Read from standard input
          READ( 5, 2000, end=285) line
        ELSE
          READ( rd_unit, 2000, end=285) line
        ENDIF
        GO TO 290
C
C- Come here for end-of-file
C
  285   CONTINUE
        IF( .NOT. stnd_input .OR. rd_unit .GT. 10 ) CLOSE( rd_unit )
        rd_unit = rd_unit - 1
        IF( rd_unit .LT. 10 ) GO TO 940
        CALL writeln(wr_unit,'C<< End-of-include-file >>',26)
        GOTO 280
C
C- Come here if line is successfully read
C
  290   lon=lenocc(line)
        IF( lon .EQ. 0 ) THEN
C  Blank line -- Replace with a comment
          line = 'C<<'
          lon = 3
          IF( rd_unit .EQ. 10 ) THEN
C  Main input file -- Tally total line count and blank line count
            lnum = lnum + 1
            bll = bll + 1
          ENDIF
          GOTO 270
        ENDIF
        IF( rd_unit .EQ. 10 ) THEN
C  Tally total line count and machine block line count
          lnum = lnum + 1
          mbl = mbl + 1
          WRITE(clnum,'(I10)') lnum
          clnum = spaces(clnum,0)
          llen = lenocc(clnum)
        ENDIF
        upline = line
        CALL cltou(upline(1:lon))
        IF( upline(1:8) .EQ. 'C&ELSEIF' )  THEN
          IF( upline(9:9) .NE. ' ' ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Invalid C& command line -- Line ' //
     &              clnum(1:llen) // ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( elsfnd ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Invalid C& line placement.  Expecting '
     &          // 'C&ENDIF -- Line '//clnum(1:llen)//' : '//line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( lenocc(upline(1:lon)) .EQ. 8 ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Missing name after the C&IF or C&ELSEIF'
     &        //' statement in line '//clnum(1:llen)//' : '//line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( satisfied ) THEN
C  A previous "C&" command statement in the machine block already accepted
            selected = .false.
            IF( debug ) WRITE(0,*) 'Skipped the test => '//line(1:lon)
            GOTO 270
          ELSE
C  Check if the C&ELSEIF command line passes its test
            first=10
            GOTO 215
          ENDIF
        ELSEIF( upline(1:6) .EQ. 'C&ELSE' )  THEN
          IF( lenocc(upline(1:lon)) .NE. 6 ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Invalid C& command line -- Line ' //
     &              clnum(1:llen) // ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( elsfnd ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Multiple C&ELSE statements found in ' //
     &             'machine block -- Line ' // clnum(1:llen) //
     &             ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( satisfied .OR. machine .EQ. 0 ) THEN
C  Either a previous "C&" command statement in the machine block
C  already accepted or the /CHECK qualifier present
            selected = .false.
            IF( debug ) WRITE(0,*) 'Skipped the test => '//line(1:lon)
          ELSE
            selected = .true.
            IF( debug ) WRITE(0,*) 'Accepted the test => '//line(1:lon)
          ENDIF
          elsfnd = .true.
C  Set machi_pkd variable to indicate that all machines
C  have been accounted for in the machine block
          machi_pkd = 65535
          GOTO 270
        ELSEIF( upline(1:7) .EQ. 'C&ENDIF' ) THEN
          IF( lenocc(upline(1:lon)) .NE. 7 ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Invalid C& command line -- Line ' //
     &              clnum(1:llen) // ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
C
C  Check if the required flavors were specified in the machine block.
C  The values of 225, 1, and 2 correspond to the bit packing of machines
C  for the generic flavors offline, online, and level2, repectively.
C
          IF( offline ) THEN
            IF( IAND(machi_pkd,225) .NE. 225 ) THEN
              WRITE(0,*) 'WARNING!  Not all required flavors for ' //
     &          'OFFLINE option accounted for in machine block -- Line '
     &          // clnum(1:llen) // ' : ' // line(1:lon)
              warning = .true.
            ENDIF
          ELSEIF( online ) THEN
            IF( IAND(machi_pkd,1) .NE. 1 ) THEN
              WRITE(0,*) 'WARNING!  Not all required flavors for ' //
     &          'ONLINE option accounted for in machine block -- Line '
     &          // clnum(1:llen) // ' : ' // line(1:lon)
              warning = .true.
            ENDIF
          ELSEIF( level2 ) THEN
            IF( IAND(machi_pkd,2) .NE. 2 ) THEN
              WRITE(0,*) 'WARNING!  Not all required flavors for ' //
     &          'LEVEL2 option accounted for in machine block -- Line '
     &          // clnum(1:llen) // ' : ' // line(1:lon)
              warning = .true.
            ENDIF
          ENDIF
          GOTO 300
        ELSEIF( upline(1:2) .EQ. 'C&' .AND. upline(3:3) .NE. ' '
     &     .AND. upline(3:3) .NE. '!' .AND. upline(3:3) .NE. 'C' ) THEN
C  Either a valid statement number immediately
C  follows the "C&" or illegal character(s)
          IF( upline(1:5) .EQ. 'C&IF ' ) THEN
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Illegal nested C&IF statement found ' //
     &             'in line ' // clnum(1:llen) // ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
          IF( LLT(upline(3:3),'0') .OR. LGT(upline(3:3),'9') ) THEN
C  Illegal character immediately follows the "C&"
            IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
            WRITE(0,*) 'ERROR!  Invalid C& command line -- Line ' //
     &              clnum(1:llen) // ' : ' // line(1:lon)
            CALL EXIT(st_err)
          ENDIF
        ENDIF
C
C  Select or unselect the machine block statement line
C
        IF( selected ) THEN
          IF( upline(1:2) .EQ. 'C&' ) THEN
            line = line(3:lon)
            upline = upline(3:lon)
            lon = lon-2
            IF( lon .EQ. 0 ) THEN
              line = 'C<<'
              upline = 'C<<'
              lon = 3
            ENDIF
          ENDIF
        ELSE
          IF( upline(1:2) .NE. 'C&' ) THEN
            str_temp = 'C&'//line(1:lon)
            line = str_temp
            lon = lon + 2
            upline = 'C*'
          ENDIF
        ENDIF
C
        IF(insert_arg) CALL proc_arg( lon,line,upline,
     &                                wr_unit,debug,clnum )
        IF(process) CALL proc_line( lon,line,upline,rd_unit,
     &                              wr_unit,debug,exp_inc,prc_stnd,
     &                              double_backslash )
        GOTO 270
      ENDIF
      IF(insert_arg) CALL proc_arg( lon,line,upline,
     &                              wr_unit,debug,clnum )
      IF(process) CALL proc_line( lon,line,upline,rd_unit,
     &                            wr_unit,debug,exp_inc,prc_stnd,
     &                            double_backslash )
  300 CONTINUE
      CALL writeln(wr_unit,line,lon)
      GOTO 200
C
C  End of processing
C
  900 CONTINUE
      IF( .NOT. stnd_input .OR. rd_unit .GT. 10 ) CLOSE( rd_unit )
      rd_unit = rd_unit - 1
      IF( rd_unit .GE. 10 ) THEN
        CALL writeln(wr_unit,'C<< End-of-include-file >>',26)
        GOTO 200
      ENDIF
      IF( .NOT. stnd_output ) CLOSE (wr_unit)
      WRITE(0,*) ' '
      WRITE(0,*) 'Line count summary for file ',file_name(1:length),' :'
      WRITE(0,*) '  Total number of lines               :',lnum
      WRITE(0,*) '    - Number of executable lines      :',exl
      WRITE(0,*) '      -- Number of continuation lines :',cnl
      WRITE(0,*) '    - Number of machine block lines   :',mbl
      WRITE(0,*) '    - Number of comment lines         :',cml
      WRITE(0,*) '    - Number of blank lines           :',bll
      WRITE(0,*) ' '
      IF ( machine .NE. 0 ) THEN
        IF( warning ) THEN
          WRITE(0,*) '**** D0FLAVOR completed with WARNING(S)! ****'
          WRITE(0,*) ' '
          CALL EXIT(st_wrn)
        ELSE
          WRITE(0,*) '**** D0FLAVOR completed successfully! ****'
          GOTO 999
        ENDIF
      ELSE
C&IF VAXVMS
C  Run D0CHECK on the output file
        WRITE(0,*) 'The FORTRAN compiler will ' //
     &             'now check the standards...'
        status = lib$spawn(
     &    '$ D0CHECK '//out_file_name(1:lenocc(out_file_name))
     &    ,,,,,,stat)
C
        WRITE(0,*) ' '
        IF( warning ) THEN
          WRITE(0,*) '**** D0FLAVOR completed with WARNING(S)! ****'
        ELSE
          WRITE(0,*) '**** D0FLAVOR completed successfully! ****'
        ENDIF
C
C  Check the completion status of D0CHECK and determine its severity
C
        sevrty = st_wrn
        IF ( stat .EQ. %LOC(D0CK_CONFORMS) ) sevrty = st_suc
        IF ( stat .EQ. %LOC(D0CK_VIOLATES) ) sevrty = st_ferr
        IF ( stat .EQ. %LOC(D0CK_NOTFOUND) .OR.
     &       stat .EQ. %LOC(D0CK_BADEXTN)  ) sevrty = st_err
C
        IF( sevrty .EQ. st_err ) THEN
          WRITE(0,*) '**** ERROR occurred with D0CHECK! ****'
        ELSEIF( sevrty .EQ. st_ferr ) THEN
          WRITE(0,*) '**** FATAL ERROR occurred with D0CHECK! ****'
        ELSEIF( sevrty .EQ. st_wrn ) THEN
          WRITE(0,*) '**** D0CHECK completed with WARNING(S)! ****'
        ELSE
          WRITE(0,*) '**** D0CHECK completed successfully! ****'
        ENDIF
        WRITE(0,*) ' '
C
C  Exit with the most worse severity between this program and D0CHECK
        IF( warning .AND.
     &      (sevrty .EQ. st_suc .OR. sevrty .EQ. st_inf) ) THEN
          CALL EXIT(st_wrn)
        ELSE
          CALL EXIT(sevrty)
        ENDIF
C&ELSE
C&        WRITE(0,*) ' '
C&        WRITE(0,*) '**** D0CHECK only implemented ' //
C&     &             'on VAX/VMS system. ****'
C&        WRITE(0,*) ' '
C&        IF( warning ) THEN
C&          WRITE(0,*) '**** D0FLAVOR completed with WARNING(S)! ****'
C&          CALL EXIT(st_wrn)
C&        ELSE
C&          WRITE(0,*) '**** D0FLAVOR completed successfully! ****'
C&          GOTO 999
C&        ENDIF
C&ENDIF
      ENDIF
C
C  Error -- End of file inside a C&IF block
C
  940 IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
      WRITE(0,*) 'ERROR!  C&IF block not closed before End-Of-File'
      CALL EXIT(st_err)
C
C  Error opening output file
C
  970 IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
      WRITE(0,*) 'ERROR!  Can not open output file ' //
     &        out_file_name(1:out_fl_len)
      CALL EXIT(st_err)
C
C  Error opening input file
C
  980 IF( .NOT. stnd_output ) CLOSE( wr_unit, status = 'DELETE' )
      WRITE(0,*) 'ERROR!  Can not open input file '
     &           // input_file(1:linput)
      CALL EXIT(st_err)
  999 END
C
      SUBROUTINE proc_line( lon, line, upline, rd_unit, wr_unit,
     &                      debug, exp_inc, prc_stnd,
     &                      double_backslash )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process lines for INCLUDE statements
C-                         and inline comments, provided the corresponding
C-                         options are selected.
C-
C-   Inputs  : line [C*]    : the line just read
C-             upline [C*]  : the upper case version of the line
C-             lon  [I]     : the line length
C-             rd_unit [I]  : the input unit
C-             wr_unit [I]  : the output unit
C-             debug [L]    : option for debug information
C-             exp_inc [L]  : option for expanding include statements
C-             prc_stnd [L] : option for standard processing of statements
C-                            (option for removing inline comments)
C-             double_backslash : Double backslash flag
C-
C-   Outputs : rd_unit [I]  : the unit where to read the next record. This is
C-                           also an input argument, and is incremented if an
C-                           INCLUDE file is opened by the line
C-   Controls:
C-
C-   Created   8-DEC-1988   Olivier Callot
C-   Updated   3-MAR-1992   Rich Mueller
C-   Removed commenting of 'Implicit None' statements
C-   Made expanding of include files optional
C-   Updated  30-Sep-1992   Herbert Greenlee
C-      Convert single backslash to double backslash on machines that
C-      use backslash as an escape character.  There is no protection
C-      against overflowing column 72.
C-   Updated  18-Nov-1992   Herbert Greenlee
C-      Add empty argument list "()"to function declarations if necessary.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*140 line,upline
      LOGICAL debug, exp_inc, prc_stnd, double_backslash, ok
      INTEGER lon,rd_unit,wr_unit,lnew,ltmp,j,in_quote,npos,epos
      CHARACTER*255 file_name
C&IF VAXVMS
C&ELSE
C&      CHARACTER*255 local_name
C&      CHARACTER*330 temp_line
C&      INTEGER context, tlen
C&ENDIF
      CHARACTER*140 str_temp, spaces
      CHARACTER*1 BACKSLASH
      INTEGER lenocc, st_err
      PARAMETER(st_err=2)
      SAVE in_quote
C----------------------------------------------------------------------
      IF( upline(1:1) .EQ. 'C' ) GOTO 999
C
C&IF VAXVMS
      IF (exp_inc) THEN
C&ELSE
C&ENDIF
C
C  Process the INCLUDE statements
C
        IF( index(upline(1:lon),'INCLUDE') .NE. 0 ) THEN
C  Use Cernlib function SPACES to remove all
C  leading and internal spaces in a string
          str_temp = spaces(upline(1:lon),0)
          IF( index(str_temp,'INCLUDE''D0$') .EQ. 1 ) THEN
            epos = index(upline(1:lon),'!')
            npos = index(upline(1:lon),'D0$')
C  Extract the include file name
            IF( epos .EQ. 0 ) THEN
              file_name = upline(npos:lon)
            ELSE
              file_name = upline(npos:epos-1)
            ENDIF
            ltmp = index( file_name, '''' )
            IF( ltmp .NE. 0 ) file_name = file_name( 1:ltmp-1 )
            ltmp = index( file_name, '/' )
            IF( ltmp .NE. 0 ) file_name = file_name( 1:ltmp-1 )
C&IF VAXVMS
C&ELSE
C&C  Convert to the local file name
C&            context = 0
C&            CALL LIB$FIND_FILE(file_name, local_name, context)
C&            CALL LIB$FIND_FILE_END(context)
C&            IF( debug ) WRITE(0,*) 'Include file name '//
C&     &        file_name(1:lenocc(file_name))//' changed to local name '
C&     &        //local_name(1:lenocc(local_name))
C&            file_name=local_name
C&ENDIF
            lnew=lenocc(file_name)
C
C&IF VAXVMS
C&ELSE
C&          IF (exp_inc) THEN
C&ENDIF
C  Open the include file
            rd_unit = rd_unit+1
            CALL D0OPEN_TEXT( rd_unit, file_name, 'IFN', ok )
            IF( .NOT. ok ) GOTO 100
C
            IF( epos .EQ. 0 ) THEN
              line = 'C<< Beginning-of-include-file '//
     &                file_name(1:lnew)//' >>'
              lon = lnew+33
            ELSE
              CALL writeln(wr_unit,'C<< Beginning-of-include-file '//
     &                     file_name(1:lnew)//' >>',lnew+33)
C  Place inline comment on a separate comment line
              line(1:epos-1) = 'C<<'
            ENDIF
            upline = 'C*'
            IF( debug ) WRITE(0,*) 'Reading from INCLUDE file '//
     &                       file_name(1:lnew)
            GOTO 999
C&IF VAXVMS
C&ELSE
C&          ELSE
C&C  Replace original file name with local name in INCLUDE statement line
C&            temp_line=line(1:npos-1)//file_name(1:lnew)//''''
C&  150       CONTINUE
C&            tlen=lenocc(temp_line)
C&            IF( tlen .GT. 72 ) THEN
C&C  Break up new line with a continuation line
C&              CALL writeln(wr_unit,temp_line(1:72),72)
C&              str_temp='     &'//temp_line(73:)
C&              temp_line=str_temp
C&              GOTO 150
C&            ENDIF
C&            IF( epos .EQ. 0 ) THEN
C&              line=temp_line(1:tlen)
C&              lon=tlen
C&            ELSE
C&              CALL writeln(wr_unit,temp_line(1:tlen),tlen)
C&C  Place inline comment on a separate comment line
C&              line(1:epos-1) = 'C<<'
C&            ENDIF
C&            upline = 'C*'
C&            GOTO 999
C&          ENDIF
C&ENDIF
          ENDIF
        ENDIF
C&IF VAXVMS
      ENDIF
C&ELSE
C&ENDIF
C
      IF (prc_stnd) THEN
CC
C  Process the IMPLICIT NONE statement
C
C  Use Cernlib function SPACES to remove leading spaces in a string and
C  to replace multiple space groups within the string with single spaces
C
C        IF( index(upline(1:lon),'IMPLICIT') .NE. 0 ) THEN
C          str_temp = spaces(upline(1:lon),1)
C          IF ( index(str_temp,'IMPLICIT NONE') .EQ. 1 ) THEN
C            line = 'C<<' // line(1:lon) // ' >>'
C            lon = lon + 6
C            upline = 'C*'
C            GOTO 999
C          ENDIF
C        ENDIF
CC
C-
C- Process backslashes
C-
      IF(DOUBLE_BACKSLASH)THEN
        BACKSLASH = '\\'
C-
C-  Look for single backslashes
C-
        DO 300 J=1,LON
          IF(LINE(J:J).EQ.BACKSLASH
     &      .AND. (J.EQ.1.OR.LINE(J-1:J-1).NE.BACKSLASH)
     &      .AND. (J.EQ.LON.OR.LINE(J+1:J+1).NE.BACKSLASH))THEN
            STR_TEMP = BACKSLASH//LINE(J:)
            LINE(J:) = STR_TEMP
            STR_TEMP = BACKSLASH//UPLINE(J:)
            UPLINE(J:) = STR_TEMP
            LON = LON + 1
          ENDIF
300     CONTINUE
      ENDIF
C
C  Process the inline comments
C
        str_temp = spaces(upline(1:lon),0)
C  Initialize quote flag (in_quote) if line
C  is executable and not a continuation line
        IF( upline(6:6) .EQ. ' ' .AND.
     &          str_temp(1:1) .NE. '!' ) in_quote = 0
        lnew = 0
        DO j = 1 , lon
          IF( line(j:j) .EQ. '''' .AND. j .GT. 6 )
     &      in_quote = 1-in_quote
          IF( line(j:j) .EQ. '!' .AND. j .NE. 6
     &                      .AND. in_quote .EQ. 0 ) THEN
C  Inline comment found
            lnew = j
            GOTO 200
          ENDIF
        ENDDO
  200   CONTINUE
        IF( lnew .NE. 0 ) THEN
C  Place inline comment on a separate comment line
          IF( lnew .GT. 1 ) THEN
            IF( lenocc(line(1:lnew-1)) .NE. 0 ) THEN
              CALL writeln(wr_unit,line,lnew-1)
            ENDIF
          ENDIF
          IF( lnew .GT. 3 ) THEN
            line(1:lnew-1) = 'C<<'
          ELSE
            str_temp = 'C<<' // line(lnew:lon)
            line = str_temp
            lon = lon - lnew + 4
          ENDIF
          upline = 'C*'
        ENDIF
      ENDIF
C-
C- Function declaration argument list
C-
      IF(UPLINE(1:6).EQ.'      '
     &  .AND. INDEX(UPLINE(1:LON),' FUNCTION ').NE.0
     &  .AND. INDEX(UPLINE(1:LON),'(').EQ.0
     &  .AND. INDEX(UPLINE(1:LON),'=').EQ.0)THEN
        LINE(LON+1:LON+2) = '()'
        UPLINE(LON+1:LON+2) = '()'
        LON = LON + 2
      ENDIF
  999 RETURN
C
C  Error opening include file
C
  100 IF( wr_unit .NE. 6 ) CLOSE( wr_unit, status = 'DELETE' )
      WRITE(0,*) 'ERROR!  Can not open include file '
     &           // file_name(1:lnew)
      CALL EXIT(st_err)
      END
C
      SUBROUTINE proc_arg( lon, line, upline, wr_unit, debug, clnum )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process lines for missing arguments in
C-                         function and subroutine calls, and insert
C-                         a %VAL(0) wherever an argument is missing.
C-
C-   Inputs  : line [C*]    : the line just read
C-             upline [C*]  : the upper case version of the line
C-             lon  [I]     : the line length
C-             wr_unit [I]  : the output unit
C-             debug [L]    : option for debug information
C-             clnum [C*]   : input file line number in character form
C-
C-   Outputs :
C-   Controls:
C-
C-   Created   10-Aug-1992   Rich Mueller
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*140 line,upline
      CHARACTER*(12) clnum, cadd_val
      CHARACTER*140 str_temp, spaces
      CHARACTER*550 temp_line
      CHARACTER*1 last
      INTEGER lon, wr_unit, in_quote, cont_next,lenocc
      INTEGER add_val, lnew, i, j, k, tlen
      LOGICAL debug
      DATA cont_next / 0 /
      SAVE in_quote, cont_next, last
C----------------------------------------------------------------------
      IF( upline(1:1) .EQ. 'C' ) GOTO 999
      IF( index(upline(1:lon),'(') .NE. 0
     &    .OR. index(upline(1:lon),',') .NE. 0
     &    .OR. index(upline(1:lon),')') .NE. 0 ) THEN
        str_temp = spaces(upline(1:lon),0)
        IF( upline(6:6) .EQ. ' ' .AND. str_temp(1:1) .NE. '!' ) THEN
C  Initialize in_quote, the quote flag
          in_quote = 0
        ENDIF
        add_val = 0
        lnew = 0
        k=1
        j=1
        temp_line=' '
        DO WHILE( j .LE. lon )
          IF( line(j:j) .EQ. '''' .AND. j .GT. 6 )
     &      in_quote = 1-in_quote
          IF( line(j:j) .EQ. '!' .AND. j .NE. 6
     &                      .AND. in_quote .EQ. 0 ) THEN
C  Inline comment found
            lnew = j
            GOTO 200
          ENDIF
C
C  Check for missing arguments
          IF( ((line(j:j) .EQ. '(' .OR. line(j:j) .EQ. ',')
     &        .AND. j .GT. 6 .AND. in_quote .EQ. 0)
     &        .OR. (cont_next .EQ. 1 .AND. j .EQ. 6) ) THEN
            IF( j .GT. 6 ) last=line(j:j)
            temp_line(k:k)=line(j:j)
            k=k+1
            j=j+1
C  Skip over spaces
            DO WHILE( line(j:j) .EQ. ' ' )
              IF( j .GT. lon ) GOTO 200
              j=j+1
            ENDDO
            IF( line(j:j) .EQ. ',' .OR.
     &          (line(j:j) .EQ. ')' .AND. last .NE. '(') ) THEN
C  A missing argument encountered.  Insert a %VAL(0) in its place
              temp_line(k:k+6)='%VAL(0)'
              k=k+7
              add_val=add_val+1
            ENDIF
            GOTO 100
          ENDIF
C
          temp_line(k:k)=line(j:j)
          k=k+1
          j=j+1
  100     CONTINUE
        ENDDO
C
  200   CONTINUE
        tlen=lenocc(temp_line(1:k))
        IF( (temp_line(tlen:tlen) .EQ. '('
     &      .OR. temp_line(tlen:tlen) .EQ. ',')
     &      .AND. in_quote .EQ. 0 ) THEN
C  Set flag to indicate that an argument is expected
C  at the start of the following continuation line
          cont_next = 1
        ELSE
          cont_next = 0
        ENDIF
C
        IF( add_val .GT. 0 ) THEN
C  Missing argument(s) were encountered and %VAL(0) argument(s) inserted
  300     CONTINUE
          IF( tlen .GT. 72 ) THEN
            DO i=72,1,-1
              IF( temp_line(i:i) .EQ. ',' ) THEN
C  Break up new line with a continuation line
                CALL writeln(wr_unit,temp_line(1:i),i)
                str_temp='     &'//temp_line(i+1:)
                temp_line=str_temp
                tlen=lenocc(temp_line)
                GOTO 300
              ENDIF
            ENDDO
          ENDIF
          IF( lnew .EQ. 0 ) THEN
            line=temp_line(1:tlen)
            lon=tlen
          ELSE
C  Inline comment was found.  Place it on a separate comment line
            CALL writeln(wr_unit,temp_line(1:tlen),tlen)
            line(1:lnew-1) = 'C<<'
          ENDIF
          IF( debug ) THEN
            WRITE(cadd_val,'(I10)') add_val
            cadd_val = spaces(cadd_val,0)
            WRITE(0,*) cadd_val(1:lenocc(cadd_val)) //
     &                ' %VAL(0) argument(s) inserted at line ' //
     &                clnum(1:lenocc(clnum))
          ENDIF
          upline = 'C*'
        ENDIF
      ENDIF
  999 RETURN
      END
C
      SUBROUTINE writeln( wr_unit, line, lon )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write the output line to the
C-                         output unit (wr_unit)
C-
C-   Inputs  : line [C*]    : the output line
C-             lon  [I]     : the line length
C-             wr_unit [I]  : the output unit
C-
C-   Created   24-JUN-1992  Richard Mueller
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) line
      INTEGER lon,wr_unit
C----------------------------------------------------------------------
C
      IF( wr_unit .NE. 6 ) THEN
        WRITE( wr_unit, 500 ) line(1:lon)
      ELSE
C  Write to standard output
        WRITE( wr_unit, 550 ) line(1:lon)
      ENDIF
  500 FORMAT(a)
C&IF VAXVMS,IBMAIX
  550 FORMAT(a)
C&ELSE
C&  550 FORMAT(' ',a)
C&ENDIF
      RETURN
      END
