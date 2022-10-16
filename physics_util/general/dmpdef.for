      SUBROUTINE DMPDEF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-    Define defaults for event dump
C-
C-    ENTRY DMPINI
C-    Book required flags
C-
C-   Created  27-JUL-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      CHARACTER*4 OPTION(10,2)
      INTEGER OUTNUM(2),OPTNUM(2),I
      INTEGER EVENT_TO_DUMP(3),IER
      CHARACTER*80 MSG
      CHARACTER*20 LABELS(2)
      LOGICAL YES,FLGVAL
C----------------------------------------------------------------------
      DATA LABELS/
     &  'Select normal dump: ',
     &  'Select raw dump   : '/
C
      DATA OPTION/
     &  'NONE','PROC','USER','ALL',6*'    ',
     &  'NONE','ALL ','TRGR','MUD1','CDD1','CDD2','CDD3','CDD4','CAD1',
     &  'CAD2'/
C
      DATA OPTNUM/4,10/
      DATA OUTNUM/2,1/
C
C        set flags for type of dump chosen
      DO 1 I=1,NDFL
    1   CALL DMPFLG(I,.FALSE.)
      CALL GETOPT(2,LABELS,OPTNUM,OPTION,OUTNUM)
      I=OUTNUM(1)
      CALL DMPFLG(I,.TRUE.)
      IF(FLGVAL('DUMP_ALL_F')) THEN
        DO 3 I=2,OPTNUM(1)
    3   CALL DMPFLG(I,.TRUE.)
      ENDIF
      I=OUTNUM(2)+OPTNUM(1)
      CALL DMPFLG(I,.TRUE.)
      IF(FLGVAL('DUMP_ALL_H')) THEN
        DO 4 I=2,OPTNUM(2)
    4   CALL DMPFLG(I,.TRUE.)
      ENDIF
      CALL DMPUDF
C
C        reset all counters
      NDUMP=0
      NSKIP=0
      NFDONE=0
      NHDONE=0
      MSG=' All event dump counters have been reset to 0.'
      CALL INTMSG(MSG)
      MSG=' If you want events dumped automatically follow dialog.'
      CALL INTMSG(MSG)
C
      YES=.FALSE.
      CALL GETPAR1l('Dump event to screen? [N]:>','L',YES)
      CALL FLGSET('DUMP_SCREEN',YES)
      YES=.FALSE.
      CALL GETPAR1l('Dump event to printer? [N]:>','L',YES)
      CALL FLGSET('DUMP_QPRINT',YES)
C
C          dialog to specify number of events to dump
      MSG=
     &  ' NDUMP events will be dumped, every NSKIP events.'
      CALL OUTMSG(MSG)
      MSG=
     &  ' If NDUMP=0 events dumped only on request.'
      CALL OUTMSG(MSG)
      CALL GETPAR(1,'NDUMP>','I',NDUMP)
C
      IF(NDUMP.GT.0) THEN
        MSG=
     &    ' If NSKIP<0 you will be asked to give event numbers.'
        CALL OUTMSG(MSG)
        CALL GETPAR(-1,'NSKIP>','I',NSKIP)
      ENDIF
C
      IF(NSKIP.LT.0) THEN
        DO 10 I=1,NDUMP
        CALL GETPAR(1,'Run no. >','I',EVENT_TO_DUMP(1))
        CALL GETPAR(-1,'Event no. >','I',EVENT_TO_DUMP(2))
        CALL GETPAR(-1,'How many consecutive events? >','I',
     &    EVENT_TO_DUMP(3))
        CALL DMPLST(EVENT_TO_DUMP,IER)
        IF(IER.NE.0) THEN
          WRITE(MSG,1001) IER
          CALL OUTMSG(MSG)
          NDUMP=IER
          GOTO 999
        ENDIF
   10   CONTINUE
      ENDIF
C
  999 RETURN
C
 1001 FORMAT(' Maximum number of events on a list is',I3,
     &  ', NDUMP has been reset')
      END
