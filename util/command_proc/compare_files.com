$!========================================================================
$!
$! Name      : COMPARE_FILES
$!
$! Purpose   : compare two sets of files and see if everything in the first 
$! the same as the second.  If not, complain.  Any files which differ will be
$! put on lists OLDER.LIS and NEWER.LIS; those same go to SAME.LIS
$!
$!      The age comparison is done on the basis of CREATION time, which is
$!      appropriate for text files (but not necessarily for .OLB's)
$!
$! Arguments : P1 a filespec for first set of files
$!             P2 dirspec or filespec for comparison files
$! Examples:
$! @compare_release older_file.com  [.test]
$!
$! Created  18-SEP-1991   James T. Linnemann
$! Modified 24-FEB-1994   James T. Linnemann Add SAME.LIS
$! Modified  3-APR-1994   James T. Linnemann and avoided deletion if all same
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ IF ("''P2'" .EQS. "" )
$ THEN 
$    WRITE SYS$OUTPUT "No second filespec given"
$    GOTO EXIT
$ ENDIF
$ FILE12_SAME == "TRUE"
$ OPEN/WRITE OUT OLDER.LIS
$ CLOSE OUT
$ OPEN/WRITE OUT SAME.LIS
$ CLOSE OUT
$ OPEN/WRITE OUT NEWER.LIS
$ CLOSE OUT
$ PIPE SAME 'P1' 'P2
$ WRITE SYS$OUTPUT "First filespec not all same "
$ WRITE SYS$OUTPUT "These files were older: "
$ TYPE/NOPAGE OLDER.LIS
$ WRITE SYS$OUTPUT "These files were newer or not found: "
$ TYPE/NOPAGE NEWER.LIS
$ WRITE SYS$OUTPUT "These files were the same: "
$ TYPE/NOPAGE SAME.LIS
$ DELETE/SYMBOL/GLOBAL FILE12_SAME
$EXIT:
$   EXIT
