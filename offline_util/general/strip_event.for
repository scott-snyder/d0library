      FUNCTION STRIP_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Strips events from files. 
C-                         
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: RCP controlled.
C-             Can strip by:
C-          
C-             a.) RUN and EVENT number
C-             b.) Trigger bit number (L1 or L2)
C-             c.) Coincidence of L1 AND L2 bits
C-
C-   Created  25-NOV-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL STRIP_EVENT
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER I,IER,NEVENTS,RUNNUM,EVENTNUM,PTR
      INTEGER EVENTS(1000)
      INTEGER NL1_TRGR,L1_TRGR(32)
      INTEGER NL2_TRGR,L2_TRGR(128)
      INTEGER NL1L2_TRGR,L1L2_TRGR(40)
      LOGICAL STRIP_EVNT,STRIP_TRGR,STRIP_L1L2TRGR,PASSED
      LOGICAL L1BIT_PASSED,L2BIT_PASSED,PASSED1,PASSED2
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('STRIP_RCP')
C     Strip type...
        CALL EZGET_l('STRIP_BY_EVENTS',STRIP_EVNT,IER)
        CALL EZGET_l('STRIP_BY_TRIGGER',STRIP_TRGR,IER)
        CALL EZGET_l('STRIP_BY_L1L2TRIGGER',STRIP_L1L2TRGR,IER)
C     Events...
        CALL EZGETA('STRIP_EVENTS',0,0,0,NEVENTS,IER)
        CALL EZGET_i('STRIP_EVENTS',EVENTS,IER)
        CALL EZGETA('L1_TRIGGER_BITS',0,0,0,NL1_TRGR,IER)
        CALL EZGET('L1_TRIGGER_BITS',L1_TRGR,IER)
        CALL EZGETA('L2_TRIGGER_BITS',0,0,0,NL2_TRGR,IER)
        CALL EZGET('L2_TRIGGER_BITS',L2_TRGR,IER)
        CALL EZGETA('L1L2_TRIGGER_BITS',0,0,0,NL1L2_TRGR,IER)
        CALL EZGET('L1L2_TRIGGER_BITS',L1L2_TRGR,IER)
        CALL EZRSET
C     Reformatting...
        NEVENTS = NEVENTS/2           
        NL1L2_TRGR = NL1L2_TRGR/2
      ENDIF
C
      STRIP_EVENT = .TRUE.
      CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)
C
C ****  STRIP BY RUN AND EVENT NUMBER...
C
      IF(STRIP_EVNT) THEN
        RUNNUM = IQ(LHEAD+6)
        EVENTNUM = IQ(LHEAD+9)
        DO I = 1,NEVENTS
          PTR = 2*I-1
          IF(RUNNUM.EQ.EVENTS(PTR) .AND. EVENTNUM.EQ.EVENTS(PTR+1)) THEN
            CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
          ENDIF
        ENDDO
      ENDIF
C
C ****  STRIP BY TRIGGER IN EITHER L1 OR L2...
C
      IF(STRIP_TRGR) THEN
        DO I = 1,NL1_TRGR
          PASSED = L1BIT_PASSED(L1_TRGR(I))
          IF(PASSED) CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDDO
        DO I = 1,NL2_TRGR
          PASSED = L2BIT_PASSED(L2_TRGR(I))
          IF(PASSED) CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDDO
      ENDIF
C
C ****  STRIP BY REQUIRING L1 AND L2 BITS TO HAVE PASSED...
C
      IF(STRIP_L1L2TRGR) THEN
        DO I = 1,NL1L2_TRGR
          PTR = 2*I-1
          PASSED1 = L1BIT_PASSED(L1L2_TRGR(PTR))
          PASSED2 = L2BIT_PASSED(L1L2_TRGR(PTR+1))
          IF(PASSED1 .AND. PASSED2)
     &         CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDDO
      ENDIF
C
  999 RETURN
      END
