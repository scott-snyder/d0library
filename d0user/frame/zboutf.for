      SUBROUTINE ZBOUTF
C---------------------------------------------------------------------
C-                                                                   -
C-    Select Zebra output files                                      -
C-    Use EVOPWO and EVCLWO
C-                                                                   -
C-                       SDP Nov,1986                                -
C-             Modified May, 1989
C-                                                                   -
C---------------------------------------------------------------------
      IMPLICIT NONE             
C
C        Communications with COMPACK
      INTEGER PFNUM
C
      CHARACTER*72 MESG,NAMOUT
      CHARACTER*40 PROMPT(2)
      CHARACTER*4 NAMCHK,NAMDRP(100)
      CHARACTER*1 XOPT
      CHARACTER*3 OSTRM
      CHARACTER*15 ONLY_E
      INTEGER NUM,NLOW,NHIGH,I,NUSER
      LOGICAL OK,YES,FIRST,FLGCHK
      SAVE NUSER,PROMPT,FIRST
C
      DATA PROMPT/' Output stream, 3 CHARACTERS,<cr> exit >',
     $            ' Name for output file >'/
      DATA FIRST/.TRUE./
C---------------------------------------------------------------------
C
C       Select Zebra output files
C
      IF(FIRST) THEN
C          drop raw banks, GEAN and HITS from DST
        CALL EVDROP('DST','TRGR')
        CALL EVDROP('DST','MUD1')
        CALL EVDROP('DST','CDD1')
        CALL EVDROP('DST','CDD2')
        CALL EVDROP('DST','CDD3')
        CALL EVDROP('DST','CDD4')
        CALL EVDROP('DST','CAD1')
        CALL EVDROP('DST','CAD2')
        CALL EVDROP('DST','GEAN')
        CALL EVDROP('DST','HITS')
        NUSER=0
        FIRST=.FALSE.
      ENDIF
C
      MESG='  DST default: drop GEAN, HITS and raw data'
      CALL INTMSG(MESG)
      MESG=' Give STA or DST to select Standard or DST output stream.'
      CALL OUTMSG(MESG)
      MESG=' Give EDS for Write Event option to write to EDS'
     &  //' output stream.'
      CALL OUTMSG(MESG)
      MESG=' If ALL then all output streams will be dropped.'
      CALL OUTMSG(MESG)
      MESG=' Any other (CHARACTER*3) for your own.'
      CALL OUTMSG(MESG)
C
      OSTRM='   '
      CALL GETPAR(1,PROMPT(1),'C',OSTRM)
      IF(PFNUM().EQ.4.OR.OSTRM.EQ.'   ') GOTO 999          ! RETURN
C
      IF(OSTRM.EQ.'ALL') THEN         ! drop all output units
        NUSER=0
        CALL EVCLWO('ALL')
        GOTO 999                   ! RETURN
      ENDIF
C
C              dialog to open file
C
      MESG=' If name of an output file is NONE no file will be open'
      CALL OUTMSG(MESG)
      MESG=' and output stream will be dropped.'
      CALL OUTMSG(MESG)
C
      CALL GETPAR(-1,PROMPT(2),'C',NAMOUT)
      IF(PFNUM().EQ.4) GOTO 999                  ! RETURN
C
   10 NAMCHK=NAMOUT(1:4)
      IF(NAMCHK.EQ.'NONE') THEN
        CALL EVCLWO(OSTRM)
        NUSER=NUSER-1
        GOTO 999                    ! RETURN
      ENDIF
C
      XOPT=' '
      MESG=' Output file modes are X (exchange), G (special X)'//
     &      ', or N (native)'
      CALL OUTMSG(MESG)
      CALL GETPAR(1,' File mode X,G or N ? [N]:>' ,'C',XOPT)
      CALL UPCASE(XOPT,XOPT)
      IF(XOPT.NE.'X'.AND.XOPT.NE.'G') XOPT=' '
      CALL EVOPWO(OSTRM,NAMOUT,XOPT,OK)
C
      IF(.NOT.OK) THEN
        CALL OUTMSG(' Unable to open or close file, give another name'
     &    //' or NONE.')
        CALL GETPAR(1,PROMPT(2),'C',NAMOUT)
        GOTO 10
      ENDIF
C
C        set flags if only event records are to be written out
      YES=.FALSE.
      CALL GETPAR(1,' Write only event records(no b-o-r or e-o-r)? [N]>'
     &  ,'L', YES)
      IF(YES) THEN
        ONLY_E='ONLY_EVENTS_'//OSTRM
        IF(.NOT.FLGCHK(ONLY_E)) CALL FLGBK(ONLY_E,1)
        CALL FLGSET(ONLY_E,.TRUE.)
      ENDIF
C                  
C        dialog to handle drops
C
      NLOW=1
      NHIGH=10
      MESG=' Banks dropped from '//OSTRM//' stream:'
      CALL INTMSG(MESG)
      CALL EVDRLS(OSTRM,NUM,NAMDRP)
   4  IF(NUM.LT.NHIGH) NHIGH=NUM
      WRITE(MESG,101) (NAMDRP(I),I=NLOW,NHIGH)
      CALL INTMSG(MESG)
      NLOW=NHIGH+1
      NHIGH=NLOW+9
      IF(NLOW.LT.NUM) GOTO 4
      YES=.FALSE.
      CALL GETPAR(1,' Want to add or remove from list? [N]>','L',
     &    YES)
      IF(YES) THEN
C          add to list
   5    NAMCHK='    '
        CALL GETPAR(1,' Add to drop list, end with <cr> >','C',
     &      NAMCHK)
        CALL EVDROP(OSTRM,NAMCHK)
        IF(NAMCHK.NE.'    ') GOTO 5
C           remove from list
   6    NAMCHK='    '
        CALL GETPAR(1,' Remove from drop list, end with <cr> >','C',
     &      NAMCHK)
        CALL EVBACK(OSTRM,NAMCHK)
        IF(NAMCHK.NE.'    ') GOTO 6
C           print list again
        NLOW=1
        NHIGH=10
        MESG=' Banks dropped from '//OSTRM//' stream:'
        CALL INTMSG(MESG)
        CALL EVDRLS(OSTRM,NUM,NAMDRP)
  14    IF(NUM.LT.NHIGH) NHIGH=NUM
        WRITE(MESG,101) (NAMDRP(I),I=NLOW,NHIGH)
        CALL INTMSG(MESG)
        NLOW=NHIGH+1
        NHIGH=NLOW+9
        IF(NLOW.LT.NUM) GOTO 14
      ENDIF
C
  999 RETURN
  101 FORMAT(X,10('...',A4))
      END
