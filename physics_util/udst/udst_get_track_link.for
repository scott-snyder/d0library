      SUBROUTINE UDST_GET_TRACK_LINK(LZTRK,UDST_TRACK_LINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the index of the saved track on the
C-                         UDST given the zebra link
C-
C-   Inputs  : LZTRK - link to ZTRK bank (on the DST)
C-   Outputs : UDST_TRACK_LINK - sequential index of UDST track
C-   Controls:
C-
C-   Created  13-DEC-1993   Ian Adam
C-   Updated  14-JAN-1994   Ian Adam  Add extra error checks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INCLUDE   'D0$INC:UDST_LINKS.INC'
      INCLUDE   'D0$LINKS:IZZTRK.LINK'
      LOGICAL FOUND
      INTEGER UDST_TRACK_LINK,TRACK_ID,LZTRK,I
      INTEGER LZTRH,GZZTRH,LZTRK_FIRST,N_TRAKS,NZBANK
C----------------------------------------------------------------------
      IF (LZTRK.LE.0) THEN
        CALL ERRMSG('ZTRK','UDST_GET_TRACK_LINK','BAD ZTRK LINK','W')
        UDST_TRACK_LINK=0
        GOTO 999
      ENDIF

      TRACK_ID=IQ(LZTRK-5)
      IF (TRACK_ID.LE.0) THEN
        CALL ERRMSG('INVALID TRACK ID','UDST_GET_TRACK_LINK',
     &    'ZTRK LINK CHECK FAILURE','W')
        GOTO 999
      ENDIF

      FOUND=.FALSE.
      UDST_TRACK_LINK=0

      LZTRH=GZZTRH()
      IF (LZTRH.LE.0) THEN
        CALL ERRMSG('NO ZTRH','UDST_GET_TRACK_LINK',
     &    'ZTRK LINK CHECK FAILURE','W')
        GOTO 999
      ENDIF

      LZTRK_FIRST=LQ(LZTRH-IZZTRK)

      IF (LZTRK_FIRST.GT.0) THEN
        N_TRAKS=NZBANK(IXCOM,LZTRK_FIRST)
      ELSE
        CALL ERRMSG('NO ZTRK','UDST_GET_TRACK_LINK',
     &    'ZTRK LINK CHECK FAILURE','W')
        GOTO 999
      ENDIF

      IF (N_TRAKS.GT.N_TRAK_MAX) THEN
        CALL ERRMSG('TOO MANY TRACKS','UDST_GET_TRACK_LINK',
     &    'TRACK LINK FINDING MAY FAIL','W')
      ENDIF

      DO I=1,MIN(N_TRAKS,N_TRAK_MAX)
        IF (TRACK_ID.EQ.TRACK_NUMBER(I)) THEN
          IF (.NOT.FOUND) THEN
            FOUND=.TRUE.
            UDST_TRACK_LINK=I
          ELSE
            CALL ERRMSG('DISASTROUS ERROR','UDST_GET_TRACK_LINK',
     &        'TRACK ID NOT UNIQUE','W')
          ENDIF
        ENDIF
      ENDDO

      IF (.NOT.FOUND) THEN
        CALL ERRMSG('TRACK NOT FOUND','UDST_GET_TRACK_LINK',' ','W')
      ENDIF

  999 RETURN
      END
