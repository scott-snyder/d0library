$!========================================================================
$!
$! Name      : SAME
$!
$! Purpose   : find out if the two files are the same
$!              possible outcomes (on screen) are:
$!              file 1 same age as file 2
$!              file 1 same contents as file 2
$!              file 2 not found
$!              file 1 older than file 2
$!              file 1 newer than file 2
$!
$!      In addition, a global symbol FILE12_SAME is created.  It is TRUE
$!          if both files were found, and matched age and/or contents.
$!      This symbol can be used in DCL via:
$!      $       IF FILE12_SAME THEN  take action in DCL
$!      This symbol is CUMULATIVE across successive calls; once it's false it
$!      stays false.
$!
$!      Finally, names of files are written into three files:
$!      OLDER.LIS and NEWER.LIS and SAME.LIS.
$!      The age comparison is done on the basis of CREATION time, which is
$!      appropriate for text files (but not necessarily for .OLB's)
$!          Files are entered into SAME.LIS Only!! if they are not the self-same
$!          file, ie you weren't compareing the file with itself.
$!  
$!      the global symbol SAME_WRITE_WILD, if defined, says to put the files in
$!          the .LIS files with ;* instead of the actual versions.  This
$!          facilitates using them via, for example,
$!              PIPER DELETE/CONFIRM same.lis
$!
$! Arguments : P1       the first file
$!             P2       the second file (as for DIFF, supply name and/or type
$!                      from first file if missing)
$!
$! Created  17-SEP-1991   James T. Linnemann
$! Modified 24-FEB-1994   James T. Linnemann add SAME.LIS
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$  age_a = "newer"
$  file = "NEWER.LIS"
$  a = P1
$  b = F$PARSE("''P2'","''P1'")
$  bsearch = b
$  IF (F$LOCATE(";","''P2'") .EQ. F$LENGTH("''P2'") )! no version in P2?
$  THEN  !remove undesired version number and revert to top version
$       bsearch = b - F$PARSE("''b'",,,"VERSION")
$  ENDIF
$  b = F$SEARCH("''bsearch'") !get instance (and thus allow wildcard search)
$  IF ("''b'" .EQS."") 
$  THEN 
$       age_a = "not found under ''bsearch'"
$       GOTO WRITE
$  ENDIF
$  ta = F$CVTIME(F$FILE_ATTRIBUTES("''a'","CDT")) !revsion date
$  tb = F$CVTIME(F$FILE_ATTRIBUTES("''b'","CDT")) !revsion date
$  IF (ta.EQS.tb) 
$  THEN 
$       age_a = "same age"
$       file = "SAME.LIS"
$  ELSE ! see if really different
$       IF (ta.LTS.tb) THEN age_a = "older"
$       IF (ta.LTS.tb) THEN file = "OLDER.LIS"
$       ftyp = F$PARSE("''a'",,,"TYPE")
$       IF (ftyp.NES.".OLB").AND.(ftyp.NES.".DIR").AND.(ftyp.NES.".STP").AND.-
          (ftyp.NES.".OBJ").AND.(ftyp.NES.".DUMP").AND.(ftyp.NES.".EXE")
$       THEN        !cross check contents if possible
$           DIFF/max=5/ignore=(spacing,trailing) 'a 'b
$           IF ($severity.EQ.1) 
$           THEN 
$               age_a = "same contents"
$               file = "SAME.LIS"
$           ENDIF
$       ENDIF
$  ENDIF
$WRITE:
$  text = a + " is " + age_a
$  WRITE SYS$OUTPUT text
$  IF ( "''FILE12_SAME'" .EQS. "" ) THEN FILE12_SAME == "TRUE"       !default
$  IF ( F$LOCATE("same",age_a) .NE. 0 ) 
$  THEN !age is NOT same
$       FILE12_SAME == "FALSE"      
$  ELSE
$! but be sure not mistakenly running against itself!!
$       a = F$SEARCH("''a'") !get instance (and thus allow wildcard search)
$       IF (a.eqs.b)
$       THEN
$           WRITE SYS$OUTPUT "''a' not added to SAME.LIS: compared with itself"
$           GOTO EXIT
$       ENDIF
$  ENDIF
$  OPEN/APPEND/ERR=EXIT OUT 'file'
$  IF ("''SAME_WRITE_WILD'" .NES. "") THEN a = a - F$PARSE(a,,,"VERSION") + ";*"
$  WRITE OUT a
$  CLOSE OUT
$  WRITE SYS$OUTPUT " >>>>>>>>> Wrote ''a' to ''file'"
$EXIT:
$  EXIT
