      FUNCTION L2_SUMM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process one event through filter
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created 4-JUN-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2_SUMM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FILTER_TIMING.INC'
      INCLUDE 'D0$INC:FSUM.INC'
      LOGICAL L,L2_SUMM_FILT
      REAL    ISA_WEIGHT,NEW_WEIGHT
      INTEGER LOC_MASK,I,J,HARDWARE_TRIGGER,GZFILT,LFILT,NUMBIT,NBITS
      INTEGER GZISAE,LISAE
      INTEGER HDSIZE
      PARAMETER( HDSIZE = 30  )
      DATA EVENT_WEIGHT /1.0/
C----------------------------------------------------------------------
      CALL HCDIR('//PAWC/L2FILT',' ')  ! go to L2FILT histogram directory
      LOC_MASK=IQ(LHEAD+11)       ! hardware_trigger
C
C...get event weight for histogramming.  Protect against missing MC info
      LISAE = GZISAE()
      IF (LISAE.GT.0) THEN
        CROSS_SECT = Q(LISAE+11)
        EVENT_WEIGHT = Q(LISAE+12)  ! else use previous event or 1.0
      ENDIF
      NBITS = NUMBIT(LOC_MASK)      ! level 1 bits count
      CALL HFILL(10001,FLOAT(NBITS),0.0,EVENT_WEIGHT)
      IF(NBITS.NE.0) THEN
        TOT_EVENTS(1)=TOT_EVENTS(1)+1
        L = L2_SUMM_FILT()            ! here call INDEPENDENTLY new script info
        IF(L) THEN    ! something wants to write out
          CALL HFILL(1000,FLOAT(IQ(LHEAD+9)),0.0,EVENT_WEIGHT) !vs evno
          LFILT = GZFILT()
          NBITS = 0
          DO I = 2,5    !loop over 4 words of filter bits
            NBITS = NBITS + NUMBIT(IQ(LFILT+I+8))  !actually passed
          ENDDO
          CALL HFILL(10002,FLOAT(NBITS),0.0,EVENT_WEIGHT)
          L2_SUMM=.TRUE.
          IF( NBITS.NE.0) TOT_EVENTS(2)=TOT_EVENTS(2)+1
        ELSE
          L2_SUMM=.FALSE.
        ENDIF
      ELSE
        TOT_EVENTS(3)=TOT_EVENTS(3)+1 !Error  (no L1 bits set)
      ENDIF
      CALL HCDIR('//PAWC',' ')  ! Leave in TOP HBOOK DIRECTORY
  999 RETURN
      END
