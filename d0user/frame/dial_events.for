      SUBROUTINE DIAL_EVENTS(NSKIP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Dialog to set number of events to process
C-
C-   Outputs:
C-     NEVRUN = cumulative number of events to process
C-     NSKIP  = number of events to skip
C-
C-   Created  10-JUL-1990   Serban D. Protopopescu
C-   Updated  23-AUG-1991   Susan K. Blessing  Changed dialog slightly. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NSKIP,NEVRUN,USNVRN
      LOGICAL FLGVAL,YES
C----------------------------------------------------------------------
C
      NEVRUN=USNVRN()
C
      IF(FLGVAL('MORE_EVENTS')) THEN
        NSKIP=0
        YES=.FALSE.
        CALL GETPAR(1,' Process all events for remaining runs? Y/N >'
     &      ,'L',YES)
C
        IF ( YES ) THEN
          NEVRUN=-2
          CALL STNVRN(NEVRUN)
          CALL FLGSET('MORE_EVENTS',.FALSE.)
C
        ELSE
          IF(NEVRUN.EQ.0) NEVRUN=-1
          CALL STNVRN(NEVRUN)
          CALL OUTMSG(' Give cumulative number of events to '//
     &        'be processed')
          CALL OUTMSG(
     &        ' including skips and already processed events.')
          CALL OUTMSG(' -1 will process all events in file.')
          CALL GETPAR(1,' Number of events >','I',NEVRUN)
          IF(NEVRUN.GT.0) THEN
            CALL GETPAR(-1,
     &        ' Number of events to skip from beginning of file. '//
     &        'Default is 0. >','I',NSKIP)
            CALL STNVRN(NEVRUN)
            CALL FLGSET('MORE_EVENTS',.FALSE.)
          ENDIF
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
