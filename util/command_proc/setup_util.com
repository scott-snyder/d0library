$!========================================================================
$!
$! Name      : SETUP_UTIL
$!
$! Purpose   : set up miscellaneous utilities in d0$util
$!
$! Arguments : none
$!
$! Created  11-MAY-1992   James T. Linnemann
$! Modified  9-DEC-1992   Harrison B. Prosper 
$!  Added RUNSUM, SHIFTS, PICKEVENTS
$! Modified  9-DEC-1992   James T. Linnemann  add look, pbd_test, FZBAscii
$! Modified  1-Feb-1993   Harrison B. Prosper
$!  Added FZMODE
$! Modified  3-May-1993   Harrison B. Prosper
$!  Added FM_LIST and FM_LIST_BATCH
$! Modified  2-July-1993   James T. Linnemann add d0lgo, Define_here
$! Modified  18-Oct-1993   James T. Linnemann add DOMAP,NOMAP,MAKE_TREE
$! 
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT 
$   
$   
$        !cms comparison.  usage: CMSD elem.ext/GEN
$   CMSD :== CMS DIFF/OUT=SYS$OUTPUT -
                     /IGNORE=(HISTORY,CASE,SPACING,TRAILING_BLANKS)
$   D0LGO :== @D0$UTIL:LGO.COM  !link and run a file, optionally debug
$        ! very fast readonly file scanner
$       DEFINE LOOK$HELP "LOOK D0$UTIL:LOOK.DOC"
$       DEFINE LOOK$EDIT "EDIT/TPU %s" 
$ ! usage: DEFINE_HERE MY_DIR   defines logical MY_DIR to default dir
$   DEFINE_HERE :== @D0$UTIL:DEFINE_HERE.COM   
$   D0LOOK :== $D0$UTIL:LOOK
$   DELDIR :== @D0$UTIL:DELDIR.COM
$   DOMAP :== $d0$beta_util:swap *.lnk nomap """map/cross"""
$   NOMAP :== $d0$beta_util:swap *.lnk """map/cross""" nomap
$   FZBA*SCII :== $d0$util:MAP.EXE
$   FZB*ROWSE :== $d0$xframe:D0X.EXE
$        !put an OBJ into any .OLB's found in this subdir
$   INSERT :== @D0$UTIL:INSERT.COM
$        !make 2 levels of subdirectory
$   MAKE_TREE :== @d0$util:make_tree.com    
$   MAKE_SDIR :== @D0$UTIL:MAKE_DIR.COM
$       !copy a whole subdirectory tree
$   MOVE :== @D0$UTIL:MASS_MOVE.COM
$       !yank a module out of any .OLB's found in this subdir
$   REMOVE :== @D0$UTIL:REMOVE.COM
$   PBD_TEST :== @D0$UTIL:PBD_TEST.COM          ! test a PBD file
$   PROT*ECT :== SET PROTECTION =(S:RE,O:RWED,GROUP,WORLD)   !Hide a file
$   SAME :== @D0$UTIL:SAME.COM      !is file a same as b
$   TYPE_S*PACE :== @d0$util:type_space.com
$   UNPR*OTECT :== @D0$UTIL:UNPROTECT.COM
$   ! compare area with subdir of release; report NEWER.LIS, OLDER.LIS, SAME.LIS
$!================================================
$!   
$ COMPARE_RELEASED :== @D0$UTIL:COMPARE_FILES.COM
$   ! compare area with d0$cms contents; report NEWER.LIS, OLDER.LIS, SAME.LIS
$ COMPARE_CMS :== @D0$UTIL:COMPARE_CMS.COM
$   ! compare beta area with release contents for ZEB area (many subdirs...)
$ CHECK_AREA  :== @D0$UTIL:CHECK_AREA
$   ! check one library (used by above)
$ CHECK_LOCAL_LIB  :== @D0$UTIL:CHECK_LOCAL_LIB
$!set defaults for above:
$   COMPARE_MODE :== ""
$   PROCESS_SDIR :== @D0$UTIL:COMPARE_SDIR
$!================================================
$   
$   RUNSUM          :== @D0$UTIL:COPY_RUN_SUMMARY.COM
$   SHIFT*S         :== @D0$UTIL:COPY_COM_SHIFTS.COM
$   PICK*EVENTS     :== @d0fs_usr$disk21:[pick_events.NEW]PICK_EVENTS.COM
$   PSTA*TUS        :== @D0$UTIL:PICK_EVENTS_STATUS.COM
$   FZMODE          :== $D0$UTIL:FZMODE.EXE
$   FM_LIST         :== @D0$UTIL:FM_LIST.COM
$   FM_LIST_B*ATCH  :== @D0$UTIL:FM_LIST_BATCH.COM
$   UPDATE_LST      :== @D0$UTIL:UPDATE_LST.COM
$   SORT_FILE_L*IST :== RUN D0$UTIL:SORT_FILE_LIST.EXE
$!================================================
$
$   SETUP_DBLINK*ER :== @D0$UTIL:SETUP_DBLINKER
$   
$EXIT:
$   EXIT
