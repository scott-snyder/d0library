      SUBROUTINE OUTUTAG(STREAM_ID,EVENT_ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called every time an event is written out. Drop UTAG
C-   bank from event unless it is the first event written to this stream or the
C-   version of the UTAG bank is different from the version of the last UTAG
C-   bank that was written to this stream.
C-
C-   Inputs  : STREAM_ID - stream identifier
C-             EVENT_ID  - event number for this stream
C-
C-   Created  29-JAN-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER STREAM_ID,EVENT_ID,LUTAG,GZUTAG,IVERSION,STREAM_ID_MAX
      PARAMETER( STREAM_ID_MAX = 20 )
      INTEGER UTAG_VERSION(STREAM_ID_MAX)
      CHARACTER*3 MSG
C----------------------------------------------------------------------
      LUTAG=GZUTAG()
      IF(LUTAG.GT.0)THEN              ! quit if there is no UTAG bank
        IF(STREAM_ID.LE.STREAM_ID_MAX)THEN 
          IVERSION=IQ(LUTAG+1) 
          IF(EVENT_ID.GT.1)THEN       ! if this is not the first event
            IF(IVERSION.EQ.UTAG_VERSION(STREAM_ID))THEN ! if no new version 
              CALL MZDROP(IXCOM,LUTAG,' ')              ! drop UTAG bank
            ELSE
              UTAG_VERSION(STREAM_ID)=IVERSION          ! save version
            ENDIF
          ELSE  
            UTAG_VERSION(STREAM_ID)=IVERSION            ! save version
          ENDIF
        ELSE
          WRITE (MSG,1) STREAM_ID   ! if STREAM_ID out of bounds issue an 
    1     FORMAT(I3)                ! error and don't drop UTAG bank
          CALL ERRMSG('STREAM_ID','OUTUTAG',
     &      'stream ID '//MSG//' invalid - UTAG bank not dropped','W')
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
