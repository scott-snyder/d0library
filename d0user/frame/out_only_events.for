      SUBROUTINE OUT_ONLY_EVENTS(CHECK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Set output stream flags to false
C-      if the flags for only events are true
C-      or reset to original values
C-
C-   Controls: 
C-   CHECK = true set flags, false reset flags
C-
C-   Created   7-DEC-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CHECK
      LOGICAL SAVE_STRM(10),FLGVAL
      INTEGER ISTRM
      CHARACTER*3 OSTRM_IN
      CHARACTER*16 STREAM
      CHARACTER*15 ONLY_E
C----------------------------------------------------------------------
C
      ISTRM=1
      CALL EVSTRM_ITOC(ISTRM,OSTRM_IN) ! get stream name from index
C
      DO  WHILE (OSTRM_IN.NE.'   ')
        STREAM='WRITE_STREAM_'//OSTRM_IN
        IF(CHECK) THEN
          ONLY_E='ONLY_EVENTS_'//OSTRM_IN
          SAVE_STRM(ISTRM)=FLGVAL(STREAM)
          IF(FLGVAL(ONLY_E)) CALL FLGSET(STREAM,.FALSE.)
        ELSE
          CALL FLGSET(STREAM,SAVE_STRM(ISTRM))
        ENDIF
        ISTRM=ISTRM+1
        CALL EVSTRM_ITOC(ISTRM,OSTRM_IN) ! get stream name from index
      ENDDO
C
  999 RETURN
      END
