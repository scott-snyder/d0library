$!========================================================================
$!
$! Name      : SQL_CODE_MAKER
$!
$! Purpose   : Create standard Database code from CDD definitions.
$!
$! Arguments : P1       Table-list
$!             P2       Prefix to routines
$!
$! Created   5-OCT-1990   Harrison B. Prosper
$! Modified 27-OCT-1990   Harrison B. Prosper 
$!      Added processing of indicators
$! Modified 29-NOV-1990   Harrison B. Prosper 
$!      Remove C_ZERO
$! Modified 21-FEB-1991   Harrison B. Prosper 
$!      Add HDB_CVERT_xxxx
$! Modified 20-APR-1991   Harrison B. Prosper 
$!      Fix type bug
$! Modified  8-JUL-1992   Harrison B. Prosper 
$!      Add prefix to tables
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   version        = "V1.13 8-Jul-1992"
$   sql_code_maker = "SQL_CODE_MAKER"
$
$   wr  :== WRITE SYS$OUTPUT
$   wf  :== WRITE file
$   wa  :== WRITE afile
$   wd  :== WRITE dfile
$   wb1 :== WRITE bfile1
$   wb02:== WRITE bfile02
$   wb2 :== WRITE bfile2
$   wb3 :== WRITE bfile3
$
$   table_lis   = ""
$   table_for   = ""
$   table_txt   = ""
$
$!================================================
$!   Check arguments
$!================================================
$   Table_list  = P1
$   IF Table_list .EQS. ""
$   THEN
$       INQUIRE table_list "Database Table "
$   ENDIF
$   IF table_list .EQS. "" THEN GOTO EXIT
$   
$   Pre    = P2
$   prefix = ""
$   IF Pre .EQS. ""
$   THEN
$       INQUIRE pre     "Routine Prefix "
$   ENDIF
$   IF pre .NES. "" 
$   THEN 
$       Prefix = Pre + "_"
$   ENDIF
$   
$
$!================================================
$!   Loop over tables
$!================================================
$   next        =-1
$   
$Get_next_table:
$
$   next        = next + 1
$   Table       = F$ELEMENT(next,",",table_list)
$   IF Table .EQS. "," THEN GOTO EXIT
$
$   table_for   = table + ".FOR"
$   table_lis   = table + ".LIS"
$   table_txt   = table + ".TXT"
$   
$   table_def   = Prefix + table + ".DEF"
$   table_cvt   = Prefix + "CVERT_" + table + ".FOR"
$   table_typ   = Prefix + "TYPE_"  + table + ".FOR"
$   table_acc   = Prefix + "ACCESS_"+ table + ".FOR"
$   
$   table_acc1  = table_acc + "_01"
$   table_acc02 = table_acc + "_02"
$   table_acc2  = table_acc + "_20"
$   table_acc3  = table_acc + "_30"
$
$!================================================
$!
$!   Compile source and create a .LIS file
$!
$!================================================
$   IF F$SEARCH(table_lis) .EQS. "" 
$   THEN 
$       WR " Extracting structure for table ''table'"
$       OPEN/WRITE file 'table_for'
$       WF "      IMPLICIT NONE"
$       WF "      DICTIONARY '" + table + "/LIST'"
$       WF "      RECORD /''table'/ ''table'"
$       WF "      END"
$       CLOSE file
$       FORTRAN/LIST/NOOBJECT/SHOW=(DICTIONARY) 'table_for'
$       SEARCH/OUTPUT='table_txt'   'table_lis' "  1     "
$       RENAME/NOLOG/NOCONFIRM  'table_txt' 'table_lis'
$       PURGE/NOLOG/NOCONFIRM   'table_lis'
$   ENDIF
$       
$!================================================
$!   Filter .LIS file
$!================================================
$       
$   COPY/NOLOG/NOCONFIRM NL: 'table_txt'
$   OPEN/READ   infile  'table_lis'
$   OPEN/APPEND outfile 'table_txt'
$   
$   READ/End=Close_Lis_File infile last_record
$   
$Get_Next_line:
$   
$   READ/End=Close_Lis_File infile record
$   length = F$LENGTH(record)
$   IF F$LOCATE("%FILL",record) .LT. Length
$   THEN
$       READ/End=Close_Lis_File infile skip_record
$       name    = F$ELEMENT(3," ",F$EDIT(last_record,"TRIM,COMPRESS"))
$       size    = F$ELEMENT(1,"(",record)
$       size    = F$ELEMENT(0,")",size)
$       I       = F$LOCATE("STRUCTURE",last_record)
$       record  = F$EXTRACT(0,I,last_record)+"CHARACTER*"+size+"  " + name
$   ELSE
$       WRITE outfile last_record
$   ENDIF
$   last_record = record
$   GOTO Get_Next_Line
$   
$Close_Lis_File:
$   WRITE   outfile last_record
$   CLOSE   infile
$   CLOSE   outfile
$!================================================
$!
$!   Create .DEF file
$!
$!================================================
$       
$START:
$       
$   WR "        Creating  ''table_def'"
$   WR "                  ''table_typ'"
$   WR "                  ''table_cvt'"
$   WR "                  ''table_acc'"
$   
$   COPY/NOLOG/NOCONFIRM NL: 'table_def'
$   COPY/NOLOG/NOCONFIRM NL: 'table_typ'
$   COPY/NOLOG/NOCONFIRM NL: 'table_cvt'
$   COPY/NOLOG/NOCONFIRM NL: 'table_acc1'
$   COPY/NOLOG/NOCONFIRM NL: 'table_acc02'
$   COPY/NOLOG/NOCONFIRM NL: 'table_acc2'
$   COPY/NOLOG/NOCONFIRM NL: 'table_acc3'
$   
$   OPEN/READ infile 'table_txt'
$   OPEN/APPEND file 'table_def'
$   OPEN/APPEND afile 'table_typ'
$   OPEN/APPEND dfile 'table_cvt'
$   OPEN/APPEND bfile1 'table_acc1'
$   OPEN/APPEND bfile02 'table_acc02'
$   OPEN/APPEND bfile2 'table_acc2'
$   OPEN/APPEND bfile3 'table_acc3'
$
$   wf "C-----------------------------------------------------------------"
$   wf "C-   Name:         ''table_def'"
$   wf "C-   Purpose:      ''Pre' declaration for table ''table' "
$   wf "C-   Created   ''F$TIME()'  by ''sql_code_maker' ''version'"
$   wf "C-----------------------------------------------------------------"
$
$   Wa "      SUBROUTINE ''Prefix'TYPE_''table'(TAB,IND,A)"
$   wa "C-----------------------------------------------------------------"
$   wa "C-"
$   wa "C-   Purpose and Methods : WRITE out fields for table ''table'"
$   wa "C-   to list specified in /''Pre'TYPE/ LISTID, ITYPE. "
$   wa "C-"
$   wa "C-   Inputs  : TAB      [I]    Tab value"
$   wa "C-             IND(*)   [I*2]  Indicators"
$   wa "C-             A        [S]    Table structure"
$   wa "C-"
$   wa "C-   Created   ''F$TIME()'  by ''sql_code_maker'" 
$   wa "C-                             ''version'"
$   wa "C-"
$   wa "C-----------------------------------------------------------------"
$   wa "      IMPLICIT NONE"
$   wa "      INTEGER TAB"
$   wa "      INTEGER*2 IND(*)"
$   wa "C-----------------------------------------------------------------"
$   wa "      INCLUDE 'D0$PARAMS:''Prefix'''TABLE'.DEF'"
$   wa "      RECORD /''TABLE'/ A"
$   wa "C-----------------------------------------------------------------"
$   wa "      INTEGER LISTID, ITYPE"
$   wa "      COMMON /''Pre'TYPE/ LISTID, ITYPE"
$   wa "C-----------------------------------------------------------------"
$   wa "      INTEGER I,J,K,LENGTH,LUN"
$   wa "      CHARACTER*80 STRING,PAD,RECORD"
$   wa "      DATA PAD/' '/"
$   wa "C-----------------------------------------------------------------"
$
$   Wb1 "      SUBROUTINE ''Prefix'ACCESS_''table'"
$   Wb1 "     &  (COMMAND,ACCESS,CVAL,IVAL,RVAL,BUFFER,LENGTH,STATUS)"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "C-"
$   wb1 "C-   Purpose and Methods : Access routine for table ''table'"
$   wb1 "C-"
$   wb1 "C-   Inputs  : COMMAND   [I]     Command Code"
$   wb1 "C-             ACCESS    [I]     Access Code"
$   wb1 "C-             CVAL      [C*]    CHARACTER datum"
$   wb1 "C-             IVAL      [I]     INTEGER   datum"
$   wb1 "C-             RVAL      [R]     REAL      datum"
$   wb1 "C-             BUFFER    [C*]    Unstructured buffer"
$   wb1 "C-             LENGTH    [I]     Length of unstructured buffer"
$   wb1 "C-"
$   wb1 "C-   Outputs : CVAL      [C*]    CHARACTER datum"
$   wb1 "C-             IVAL      [I]     INTEGER   datum"
$   wb1 "C-             RVAL      [R]     REAL      datum"
$   wb1 "C-             BUFFER    [C*]    Unstructured buffer"
$   wb1 "C-             LENGTH    [I]     Length of unstructured buffer"
$   wb1 "C-             STATUS    [I]     Status code"
$   wb1 "C-"
$   wb1 "C-  Created   ''F$TIME()'  by ''sql_code_maker'"
$   wb1 "C-                            ''version'"
$   wb1 "C-"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "      IMPLICIT NONE"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "      INTEGER       COMMAND"
$   wb1 "      INTEGER       ACCESS"
$   wb1 "      CHARACTER*(*) CVAL"
$   wb1 "      INTEGER       IVAL"
$   wb1 "      REAL          RVAL"
$   wb1 "      CHARACTER*(*) BUFFER"
$   wb1 "      INTEGER       LENGTH"
$   wb1 "      INTEGER       STATUS"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "      INCLUDE 'D0$INC:''Pre'COMMON.INC'"
$   wb1 "      INCLUDE 'D0$PARAMS:''Pre'CODES.DEF'"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "      INCLUDE 'D0$PARAMS:''Prefix'''table'.DEF'"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "      RECORD /''TABLE'/ A,B"
$   wb1 "      STRUCTURE /IND/"
$   wb1 "        UNION"
$   wb1 "          MAP"
$   wb1 "          INTEGER*2 IND(''TABLE'_MAX)"
$   wb1 "          END MAP"
$   wb1 "          MAP"
$   wb1 "          CHARACTER*(2*''table'_MAX) BUFFER"
$   wb1 "          END MAP"
$   wb1 "        END UNION"
$   wb1 "      END STRUCTURE"
$   wb1 "      RECORD /IND/ I"
$   wb1 "C-----------------------------------------------------------------"
$   wb1 "  "
$   wb1 "      IF ( ''Prefix'DEBUG ) THEN"
$   wb1 "        CALL ''Prefix'DEBUG_DUMP (0,T_''table',"
$   wb1 "     &       COMMAND,ACCESS,CVAL,IVAL,RVAL,STATUS)"
$   wb1 "        IF ( ''Prefix'NOACTION ) GOTO 999"
$   wb1 "      ENDIF"
$   wb1 " "
$   wb1 "      IF     ( COMMAND .EQ. C_INSERT    .OR."
$   wb1 "     &         COMMAND .EQ. C_UPDATE    .OR. "
$   wb1 "     &         COMMAND .EQ. C_DELETE    .OR. "
$   wb1 "     &         COMMAND .EQ. C_FETCH   ) THEN"
$   wb1 " "
$   wb1 "        CALL ''Pre'A_''table'"
$   wb1 "     &      (COMMAND,ACCESS,CVAL,IVAL,I.IND,A,STATUS)"
$   wb1 " "
$   wb1 "      ELSEIF ( COMMAND .EQ. C_TYPE    ) THEN"
$   wb1 " "
$   wb1 "        CALL ''Prefix'TYPE_''table' (ACCESS,I.IND,A)"
$   wb1 " "
$   wb1 "      ELSEIF ( COMMAND .EQ. C_GET     ) THEN"
$   wb1 " "
$   wb1 "        IF     ( ACCESS .EQ. A_NEXT_ID   ) THEN"
$   wb1 " "
$   wb1 "          CALL ''Prefix'GET_NEXT_ID (T_''table',IVAL)"
$   wb1 " "
$   wb1 "        ELSEIF ( ACCESS .EQ. C_RECORD    ) THEN"
$   wb1 "          LENGTH = LEN(A.BUFFER)"
$   wb1 "          BUFFER = A.BUFFER"
$   wb1 " "
$   wb1 "        ELSEIF ( ACCESS .EQ. A_INDICATORS) THEN"
$   wb1 "          LENGTH = LEN(I.BUFFER)"
$   wb1 "          BUFFER = I.BUFFER"
$   wb1 " "
$
$   Wd "      SUBROUTINE ''Prefix'CVERT_''table'"
$   wd "     & (SWIT,INDBUF,LIND,BUFFER,LENGTH,CFL,VAL,REC,LREC,NREC)"
$   wd "C-----------------------------------------------------------------"
$   wd "C-"
$   wd "C-   Purpose and Methods : Convert from structure to strings"
$   wd "C-   and vice versa."
$   wd "C-"
$   wd "C-   Inputs  : SWIT     [I]    0 Structure to RCP"
$   wd "C-                             1 RCP to structure"
$   wd "C-             INDBUF   [C*]   Indicators      (-1 or 0)"
$   wd "C-             LIND     [I]    Length of INDBUF"
$   wd "C-             BUFFER   [C*]   Table structure"
$   wd "C-             LENGTH   [I]    Length of BUFFER"
$   wd "C-   Outputs : CFL(*)   [C*]   Column Indicator (0,1,3)"
$   wd "C-             VAL(*)   [C*]   Column Value"
$   wd "C-             REC(*)   [C*]   COL//TYP//VAL"
$   wd "C-             LREC(*)  [I]    Length of record"
$   wd "C-             NREC     [I]    Number of records"
$   wd "C-          
$   wd "C-"
$   wd "C-   Created   ''F$TIME()'  by ''sql_code_maker'" 
$   wd "C-                             ''version'"
$   wd "C-"
$   wd "C-----------------------------------------------------------------"
$   wd "      IMPLICIT NONE"
$   wd "      INTEGER SWIT"
$   wd "      CHARACTER*(*) INDBUF"
$   wd "      INTEGER LIND"
$   wd "      CHARACTER*(*) BUFFER"
$   wd "      INTEGER LENGTH"
$   wd "      CHARACTER*(*) CFL(*)"
$   wd "      CHARACTER*(*) VAL(*)"
$   wd "      CHARACTER*(*) REC(*)"
$   wd "      INTEGER LREC(*),NREC"
$   wd "C-----------------------------------------------------------------"
$   wd "      INCLUDE 'D0$PARAMS:''Prefix'''TABLE'.DEF'"
$   wb1 "C-----------------------------------------------------------------"
$   wd "      RECORD /''TABLE'/ A"
$   wd "      STRUCTURE /IND/"
$   wd "        UNION"
$   wd "          MAP"
$   wd "          INTEGER*2 IND(''TABLE'_MAX)"
$   wd "          END MAP"
$   wd "          MAP"
$   wd "          CHARACTER*(2*''table'_MAX) BUFFER"
$   wd "          END MAP"
$   wd "        END UNION"
$   wd "      END STRUCTURE"
$   wd "      RECORD /IND/ IND"
$   wb1 "C-----------------------------------------------------------------"
$   wd "      INTEGER I,J,K,L,N"
$   wd "      CHARACTER*80 STRING,PAD"
$   wd "      DATA PAD/'                                           '/"
$   wd "C-----------------------------------------------------------------"
$   wd "      NREC = ''table'_MAX"
$   wd "      IND.BUFFER = INDBUF(1:LEN(INDBUF))   ! Initialize Indicators"
$   wd "      IF ( SWIT .EQ. 0 ) THEN"
$   wd "        A.BUFFER   = BUFFER(1:LEN(BUFFER)) ! Initialize Structure"
$   wd "      ENDIF"
$   wd "  "
$!================================================
$!
$!   Start part 2 of access routine
$!
$!================================================
$   wb02 " "
$   wb02 "      ELSEIF ( COMMAND .EQ. C_SET     ) THEN"
$   wb02 " "
$   wb02 "        IF     ( ACCESS .EQ. C_RECORD    ) THEN"
$   wb02 "          B.BUFFER = BUFFER(1:LENGTH)"
$   
$   wb2 "
$   wb2 "        ELSEIF ( ACCESS .EQ. A_INDICATORS) THEN"
$   wb2 "          I.BUFFER = BUFFER(1:LENGTH)"
$   wb2 " "
$!================================================
$!
$!   Write out type statements
$!
$!================================================
$   ltab = F$STRING(F$LENGTH(table))
$   wa "  "
$   wa "      LUN = IABS(LISTID)"
$   wa "      RECORD = ' '"
$   wa "      IF (ITYPE .EQ. 0) THEN"
$   wa "        WRITE(RECORD,'(A)')"
$   WA "     &  PAD(1:TAB)//'RECORD " + table + "'"
$   wa "        LENGTH = TAB+7+''ltab'"
$   wa "      ELSE"
$   wa "        WRITE(RECORD,'(A)')"
$   WA "     &  '\ARRAY " + table + "'"
$   wa "        LENGTH = 7+''ltab'"
$   wa "      ENDIF"
$   wa "      IF (LISTID .GT. 0) THEN"
$   wa "        CALL LWRITE(LISTID,RECORD(1:LENGTH))"
$   wa "      ELSE"
$   wa "        IF ( LUN .EQ. 6 ) THEN"
$   wa "          CALL INTMSG(' '//RECORD(1:LENGTH))"
$   wa "        ELSE"
$   wa "          WRITE(LUN,'(1X,A)') RECORD(1:LENGTH)"
$   wa "        ENDIF"
$   wa "      ENDIF"
$
$   field_count = 0          ! field count
$   byte_count  = 0
$   do_this     = 1
$   start_union = 1
$   end_union   = 1
$   do_count    = 0
$
$Get_next_record:
$
$   READ/END_OF_FILE=CLose_files infile record
$   field_count  = field_count + 1
$   write_out = 1
$
$   L      = F$LENGTH(record)
$   N      = F$LOCATE(" 1 ",record)
$   record = F$EXTRACT(N+3,L-N-2,record)
$!================================================
$!
$!   Process comments
$!
$!================================================
$   N   = F$LOCATE("!",record)
$   L   = F$LENGTH(record)
$   IF N .LT. L
$   THEN
$       record = F$EDIT(record,"TRIM")
$       L   = F$LENGTH(record)
$       field_count  = field_count - 1      ! comment
$       IF do_this
$       THEN
$           IF F$LOCATE(" CDD ",record) .LT. L
$           THEN
$               Write_out = 0
$               do_this   = 0
$           ENDIF
$       ENDIF
$   ENDIF
$!================================================
$!
$!   Count total byte length
$!
$!================================================
$
$   IF Do_count
$   THEN
$       N   = F$LOCATE("*",record)
$       L   = F$LENGTH(record)
$       IF N .LT. L
$       THEN
$           vartype= F$EXTRACT(0,N,record)
$           vartype= F$EDIT(vartype,"TRIM")
$           vartype= F$EXTRACT(0,1,vartype)
$           string = F$EXTRACT(N+1,L-N,record)
$           M      = F$LOCATE(" ",string)
$           varname= F$EXTRACT(M,L,string)
$           varname= F$EDIT(varname,"TRIM")
$           number = F$EXTRACT(0,M,string)
$           byte_count = byte_count + F$INTEGER(number)
$           format_type = vartype + "*" + number
$           lvarname    = F$LENGTH(varname)
$!================================================
$!
$!   Define output format
$!
$!================================================
$           
$           IF vartype .eqs. "I"
$           THEN
$               format = "I16"
$               apo    = ""
$               IF number .eqs. "2"
$               THEN
$                   lentype = 6
$               ELSE
$                   lentype =12
$               ENDIF
$           ELSE
$           IF vartype .eqs. "C"
$           THEN
$               format = "A"
$               apo    = "'"
$               lentype= F$INTEGER(number)
$           ELSE
$           IF vartype .eqs. "R"
$           THEN
$               format = "1PE12.5"
$               apo    = ""
$               lentype= 12
$           ENDIF
$           ENDIF
$           ENDIF
$           IF lentype .GT. 32
$           THEN
$               lentype = 32
$           ENDIF
$
$           l = F$STRING(24 - F$INTEGER(lvarname))
$           ifield = F$STRING(field_count-1)
$           lt= F$STRING(lentype)
$           formt = format_type + "           ."
$   wa " "
$   wa "C------------------ ''table'.''varname'        
$   wa "      RECORD = ' '"
$   wa "      IF ( IND(''ifield') .EQ. 0 ) THEN"
$   wa "        STRING = ' '"
$   wa "        WRITE(STRING,FMT='(''format')') A.''varname'"
$   wa "        CALL SWORDS(STRING,I,J,K)"
$   wa "        IF (ITYPE .EQ. 0) THEN"
$   wa "          WRITE(RECORD,'(A)') PAD(1:TAB+2)//'"+ varname + "'//"
$   wa "     &    PAD(1:''L')//' '"  -
        +apo+apo + "//STRING(I:J)//"+apo+apo+"' '"
$   wa "          LENGTH = TAB+2+"+F$STRING(lvarname)+"+''l'+1+K+4"
$   wa "        ELSE"
$   wa "          WRITE(RECORD,'(A)') '  " +"'"+"'"+ varname +"'"+"'"+ "'//"
$   wa "     &    PAD(1:''L'+2)//'" -
        +"'"+"'"+ F$EXTRACT(0,4,formt) +"'"+"'"+ "'//"
$   wa "     &    ' " +"'"+"'"+"'"+ "//STRING(I:J)//PAD(1:''LT'-K)//"  -
        +"'"+"'"+"'"+"'"
$   wa "          LENGTH = 4+"+F$STRING(lvarname)+"+2+''l'+2+8+''lt'+8"
$   wa "        ENDIF"
$   wa "        IF (LISTID .GT. 0) THEN"
$   wa "          CALL LWRITE(LISTID,RECORD(1:LENGTH))"
$   wa "        ELSE"
$   wa "          IF ( LUN .EQ. 6 ) THEN"
$   wa "            CALL INTMSG(' '//RECORD(1:LENGTH))"
$   wa "          ELSE"
$   wa "            WRITE(LUN,'(1X,A)') RECORD(1:LENGTH)"
$   wa "          ENDIF"
$   wa "        ENDIF"
$   wa "      ENDIF"
$   
$   wd " "
$   wd "C------------------ ''table'.''varname'        
$   wd "      IF (SWIT .EQ. 0) THEN     ! Structure --> RCP"
$   wd "        IF ( IND.IND(''ifield') .EQ. 0 ) THEN"
$   wd "          STRING = ' '"
$   wd "          WRITE(STRING,'(''format')') A.''varname'"
$   wd "          CALL SWORDS(STRING,I,J,K)"
$   wd "          CFL(''ifield') = '0'"
$   wd "          VAL(''ifield') = STRING(I:J)"
$   wd "          REC(''ifield') = '  " +"'"+"'"+ varname +"'"+"'"+ "'//"
$   wd "     &    PAD(1:''L'+2)//'" -
        +"'"+"'"+ F$EXTRACT(0,4,formt) +"'"+"'"+ "'//"
$   wd "     &    ' " +"'"+"'"+"'"+ "//STRING(I:J)//PAD(1:''LT'-K)//"  -
        +"'"+"'"+"'"+"'"
$   wd "          LREC(''ifield')= 4+"+F$STRING(lvarname)+"+2+''l'+2+8+''lt'+8"
$   wd "        ENDIF"
$   wd "      ELSE                      ! RCP --> Structure"
$   wd "        IF ( (CFL(''ifield')(1:1) .EQ. '1') .OR."
$   wd "     &       (CFL(''ifield')(1:1) .EQ. '3') ) THEN"
$   wd "          STRING = VAL(''ifield')"
$   IF vartype .EQS. "I" .OR. vartype .EQS. "R"
$   THEN
$   wd "          READ(STRING,*) A.''varname'"
$   ELSE
$   wd "          READ(STRING,'(''format')') A.''varname'"
$   ENDIF
$   wd "          IND.IND(''ifield') = 0"
$   wd "        ENDIF"
$   wd "      ENDIF"
$!================================================
$!
$!   Write out GET statements
$!
$!================================================
$           wb1 "        ELSEIF ( ACCESS .EQ. F_''varname' ) THEN"
$           wb1 "          ''vartype'VAL = A.''varname'"
$!================================================
$!
$!   Write out SET statements
$!
$!================================================
$           wb02 "          IF ( I.IND(''ifield') .EQ. 0 ) THEN"
$           wb02 "            A.''varname' = B.''varname'"
$           wb02 "          ENDIF"
$           
$           wb3 "        ELSEIF ( ACCESS .EQ. F_''varname' ) THEN"
$           wb3 "          A.''varname' = ''vartype'VAL"
$           wb3 "          I.IND(''ifield') = 0"
$       ENDIF
$   ENDIF
$!================================================
$!
$!   END UNION
$!
$!================================================
$   IF End_union
$   THEN
$       N   = F$LOCATE("END STRUCTURE",record)
$       L   = F$LENGTH(record)
$       IF N .LT. L
$       THEN
$           End_union = 0
$           Do_count  = 0
$           string    = F$STRING(byte_count)
$           wf "          END MAP"
$           wf "!------------------------------<Unstructured buffer>"
$           wf "          MAP"
$           wf "          CHARACTER*''string' BUFFER"
$           wf "          END MAP"
$           wf "        END UNION"
$       ENDIF
$   ENDIF
$
$   IF write_out THEN wf record
$!================================================
$!
$!   START UNION
$!
$!================================================
$   IF Start_union
$   THEN
$       N   = F$LOCATE("    STRUCTURE",record)
$       L   = F$LENGTH(record)
$       IF N .LT. L
$       THEN
$           do_count = 1
$           Start_union = 0
$           wf "        UNION"
$           wf "!------------------------------<Structured buffer>"
$           wf "          MAP"
$       ENDIF
$   ELSE
$!================================================
$!   Look for %FILL (8)
$!================================================
$       N   = F$LOCATE("    STRUCTURE",record)
$       L   = F$LENGTH(record)
$       IF N .LT. L
$       THEN
$           do_count = 1
$           Start_union = 0
$           wf "        UNION"
$           wf "!------------------------------<Structured buffer>"
$           wf "          MAP"
$       ENDIF
$   ENDIF
$
$   GOTO Get_next_record
$
$Close_files:
$   field_count  = field_count - 2          ! STRUCTURE ... END STRUCTURE
$   string       = F$STRING(field_count)
$   WF "!     Total number of fields"
$   WF "      INTEGER    ''table'_MAX"
$   wf "      PARAMETER( ''table'_MAX  = ''string' )"
$   WF "  "
$!================================================
$!
$!   End for TYPE program
$!
$!================================================
$   wa " "
$   wa "      RECORD = ' '"
$   wa "      IF (ITYPE .EQ. 0) THEN"
$   wa "        WRITE(RECORD,'(A)') PAD(1:TAB)//'ENDRECORD'"
$   wa "        LENGTH = TAB + 9"
$   wa "      ELSE"
$   wa "        WRITE(RECORD,'(A)') '\END'"
$   wa "        LENGTH = 5"
$   wa "      ENDIF"
$   wa "      IF (LISTID .GT. 0) THEN"
$   wa "        CALL LWRITE(LISTID,RECORD(1:LENGTH))"
$   wa "      ELSE"
$   wa "        IF ( LUN .EQ. 6 ) THEN"
$   wa "          CALL INTMSG(' '//RECORD(1:LENGTH))"
$   wa "        ELSE"
$   wa "          WRITE(LUN,'(1X,A)') RECORD(1:LENGTH)"
$   wa "        ENDIF"
$   wa "      ENDIF"
$   wa "  999 RETURN"
$   wa "      END"
$   
$   wd " "
$   wd "      IF ( SWIT .EQ. 1 ) THEN"
$   wd "        INDBUF = IND.BUFFER"
$   wd "        LIND   = LEN(IND.BUFFER)"
$   wd "        BUFFER = A.BUFFER"
$   wd "        LENGTH = LEN(A.BUFFER)"
$   wd "      ENDIF"
$   wd "      RETURN"
$   wd "      END"
$!================================================
$!
$!   Write end of parts 1,3 access code segment
$!
$!================================================
$   wb1 "        ENDIF"         ! Get
$   wb3 "        ENDIF"         ! Set (end of access)
$   wb3 "      ENDIF"
$   wb3 " "
$   wb3 "      IF ( ''Prefix'DEBUG ) THEN"
$   wb3 "        CALL ''Prefix'DEBUG_DUMP (1,T_''table',"
$   wb3 "     &       COMMAND,ACCESS,CVAL,IVAL,RVAL,STATUS)"
$   wb3 "      ENDIF"
$   wb3 "  999 RETURN"
$   wb3 "      END"
$
$   CLOSE file
$   CLOSE afile
$   CLOSE bfile1
$   CLOSE bfile02
$   CLOSE bfile2
$   CLOSE bfile3
$   CLOSE dfile
$   CLOSE infile
$!================================================
$!
$!   Concatenate access routine segments
$!
$!================================================
$   COPY/CONC/NOCONFIRM/NOLOG 'table_acc'_* 'table_acc'
$   IF F$SEARCH("''table_acc'_*") .NES. "" THEN  -
        DELETE/NOCONFIRM/NOLOG 'table_acc'_*;*
$   IF F$SEARCH(table_for) .NES. "" THEN DELETE/NOCONFIRM/NOLOG 'table_for';*
$   IF F$SEARCH(table_txt) .NES. "" THEN DELETE/NOCONFIRM/NOLOG 'table_txt';*
$
$   GOTO Get_next_table
$
$EXIT:
$   EXIT
