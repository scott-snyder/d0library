      SUBROUTINE ZBINPF(INUNIT,NOIN)
C---------------------------------------------------------------------
C-                                                                   -
C-    Dialog to select Zebra input file                              -
C-                                                                   -
C-    OUTPUT:                                                        -
C-    INUNIT= input file unit                                        -
C-    NOIN  = .FALSE. if an input file was opened                    -
C-                                                                   -
C-    ENTRY INPUT_F_INFO(NAME,XMOD)
C-    OUTPUT:                                                        -
C-    NAME=  name of input file                                      -
C-    XMOD=  'X' or 'G' for exchange mode
C-                                                                   -
C-                       SDP Oct,1986                                -
C-   Updated   8-MAR-1990   Serban D. Protopopescu
C-   Updated   1-FEB-1992   Krzysztof L. Genser   
C-    Interface to FATMEN + CALL CLTOU(NAMIN), NAMIN now 255 char long
C-   Updated  18-MAR-1992   Serban D. Protopopescu   
C-    Added use of FILE_NAMES
C-
C---------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FATPARA.DEF/LIST'

      INTEGER LENOCC
      CHARACTER*4 CFLF
C
C        Communications with COMPACK
      INTEGER PFNUM
C
      INTEGER INUNIT
      LOGICAL NOIN,OK,FLGVAL,TAPE,FIRST
      INTEGER ERR,IRC
      CHARACTER*4 XCHOPT
      CHARACTER*1 XOPT,XMOD
      CHARACTER*76 MESG
      CHARACTER*255 NAMIN,FNAME,TMPFNAME
      CHARACTER*(*) NAME
      SAVE FNAME
C------------------------------------------------------------------
      DATA NAMIN/'NONE'/
      DATA FIRST/.TRUE./
C
      IF(FLGVAL('NO_INPUT_FILE')) THEN
        CALL INTMSG(' NO INPUT FILE ALLOWED: package generates data.')
        RETURN   ! no input files allowed
      ENDIF
    1 CONTINUE
      IF(FIRST) THEN
        TAPE=.FALSE.
        CALL GETPAR(1,' Will you be reading files from tape? [N]>',
     &    'L',TAPE)
        FIRST=.FALSE.
      ENDIF
      IF(.NOT.TAPE) THEN
        MESG=
     &' If you give wild card all files with given string are processed'
        CALL OUTMSG(MESG)
        MESG=
     &' but option Manual Process will stop after N events requested.'
        CALL OUTMSG(MESG)
        MESG=
     &' If you give generic name no wild card will be allowed'
        CALL OUTMSG(MESG)
        MESG=
     &' If generic name points to a tape stage will be invoked' 
        CALL OUTMSG(MESG)
      ELSE
        MESG=' Wild cards not allowed for files from tape.'
        CALL OUTMSG(MESG)
      ENDIF
      MESG=' If name of input file is NONE no file will be open.'
      CALL OUTMSG(MESG)
      MESG=
     &  ' If name of input file is FILE_NAMES expect logical FILE_NAMES'
      CALL OUTMSG(MESG)
      MESG=' to be assigned to an ASCII file with lists of files.'
      CALL OUTMSG(MESG)
      CALL GETPAR(1,'Input File Name>','C',NAMIN)
C
      IF(PFNUM().EQ.4) RETURN
      TMPFNAME = NAMIN
      CALL CLTOU(TMPFNAME)
C
      IF(TMPFNAME(1:4).NE.'NONE') THEN
C
C          release input unit
C
        IF(INUNIT.NE.0) THEN
          CALL FZENDI(INUNIT,'T')
          CLOSE(INUNIT)
          CALL RLUNIT(87,INUNIT,ERR)
        ENDIF
C
C ****  pretend FATMEN files are tape files to prevent VMS parsing
C
        IF(TMPFNAME(1:9).EQ.D0TRNK) TAPE=.TRUE.    
C
        CALL PARSE_FILES(NAMIN,ERR,TAPE)
        CALL SEARCH_FILES(FNAME,ERR)
        TMPFNAME = FNAME
        CALL CLTOU(TMPFNAME)
C
        IF ( TMPFNAME(1:9) .NE. D0TRNK  ) THEN
          ERR=0
          NOIN=.FALSE.
          XOPT=' '
          MESG=' Input file modes are X (exchange), G (special X)'//
     &      ', or N (native)'
          CALL OUTMSG(MESG)
          CALL GETPAR(1,' File mode X,G or N ? [N]:>' ,'C',XOPT)
          CALL UPCASE(XOPT,XOPT)
          IF(XOPT.NE.'X'.AND.XOPT.NE.'G') XOPT=' '
          IF(ERR.NE.0) GOTO 100
          XCHOPT=XOPT
          IF(TAPE) XCHOPT=XOPT//'T'
          CALL EVOPIN(FNAME,XCHOPT,INUNIT,OK)  ! open input file
          IF(.NOT.OK) GOTO 100
        ELSE
C
C ****  generic name
C
C
C ****  use D0OPEN to open the file via FATMEN 
C
          CALL GTUNIT(87,INUNIT,ERR)
          IF(ERR.NE.0) GOTO 100
C
C ****  options for fmopen after / Read,  
C ****  V update file size after staging to disk, F call FZFILE
C
          CALL D0OPEN(INUNIT,FNAME,'/RVF',OK)
          IF(.NOT.OK) GOTO 100
C
C ****  declare the file to be opened
C
          NOIN=.FALSE.
C
C ****  get the file name corresponding to the generic name
C
          CALL FMGETC (LFMINF,FNAME,MFQNFA,255,IRC)
          IF(IRC.NE.0) GOTO 100
C
C ****  get the file format
C
          CALL FMGETC (LFMINF,CFLF,MFLFFA,NFLFFA,IRC)
          IF(IRC.NE.0) GOTO 100
          XOPT=' '
          IF ( CFLF(1:LENOCC(CFLF)) .EQ. 'FX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FFX' .OR.
     &         CFLF(1:LENOCC(CFLF)) .EQ. 'FXN' ) XOPT='X'

        ENDIF

      ENDIF
C
      RETURN
  100 CONTINUE
      MESG=' Unable to open or close file:'
      CALL OUTMSG(MESG)
      MESG=' '//FNAME
      CALL OUTMSG(MESG)
      GOTO 1
C
      ENTRY INPUT_F_INFO(NAME,XMOD)
      NAME=FNAME
      XMOD=XOPT
      RETURN
      END
