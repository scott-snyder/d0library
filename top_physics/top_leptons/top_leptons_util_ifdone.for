      SUBROUTINE TOP_LEPTONS_UTIL_IFDONE(DO_EVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Test for duplicate event processing
C-
C-   Inputs  :  None
C-
C-   Outputs :  DO_EVENT = .TRUE./.FALSE. for unique/duplicate event
C-
C-   Controls:  DO_CHECK from RCP file
C-
C-   Created    8-Jun-1993   Tom Diehl
C-   Tidied up  3-Apr-1994   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      EXTERNAL TOP_LEPTONS_UTIL_MONTECARLO
C 
      LOGICAL DO_EVENT,FIRST,DO_CHECK,TOP_LEPTONS_UTIL_MONTECARLO
C
      INTEGER NEVENTS,I,IER
      INTEGER EVENT(3,5000)
C
      CHARACTER*80 MSG1
C
      DATA NEVENTS/0/
      DATA FIRST/.TRUE./
      DATA MSG1 / ' Checking for Duplicate events in Beam Data '/ 
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        IER=0
        CALL EZPICK('TOP_LEPTONS_RCP')
        CALL EZGET('DO_DUPLICATE_CUT',DO_CHECK,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error reading test logical',
     &    'TOP_LEPTONS_UTIL_IFDONE',' ','F')
        IF(DO_CHECK) THEN
          CALL INTMSG(MSG1)
        ENDIF
      ENDIF
C
C *** Skip out if Montecarlo or no check wanted
C
      IF(.NOT.DO_CHECK.OR.TOP_LEPTONS_UTIL_MONTECARLO()) THEN
        DO_EVENT=.TRUE.
        RETURN
      ENDIF
C
      DO I=1,NEVENTS
        IF(IQ(LHEAD+12).EQ.EVENT(1,I).AND.
     1    IQ(LHEAD+7).EQ.EVENT(2,I).AND.IQ(LHEAD+8).EQ.EVENT(3,I)) THEN
            DO_EVENT=.FALSE.
            GO TO 999
        ENDIF
      ENDDO
C
C *** Unique Data Event
C
      DO_EVENT=.TRUE.
      NEVENTS=NEVENTS+1
      EVENT(1,NEVENTS)=IQ(LHEAD+12)
      EVENT(2,NEVENTS)=IQ(LHEAD+7)
      EVENT(3,NEVENTS)=IQ(LHEAD+8)
C
C----------------------------------------------------------------------
  999 RETURN
      END
