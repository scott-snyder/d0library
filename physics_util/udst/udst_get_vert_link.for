      SUBROUTINE UDST_GET_VERT_LINK(LZTRK,UDST_VERT_LINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : LZTRK
C-   Outputs : UDST_VERT_LINK
C-   Controls:
C-
C-   Created  17-DEC-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:UDST_LINKS.INC'
      LOGICAL FOUND

      INTEGER LZTRK,LVERT,VERT_ID,UDST_VERT_LINK,I
C----------------------------------------------------------------------
      IF (LZTRK.LE.0) THEN
        CALL ERRMSG(' ','UDST_GET_VERT_LINK','NO ZTRK','W')
        UDST_VERT_LINK=0
        GOTO 999
      ENDIF

      LVERT=LQ(LZTRK-2)
      IF (LVERT.LE.0) THEN
        UDST_VERT_LINK=0
        GOTO 999
      ENDIF

      FOUND=.FALSE.
      VERT_ID=IQ(LVERT-5)

      DO I=1,N_VERT_MAX
        IF (VERT_ID.EQ.VERT_NUMBER(I)) THEN
          IF (.NOT.FOUND) THEN
            FOUND=.TRUE.
            UDST_VERT_LINK=I
          ELSE
            CALL ERRMSG('DISASTROUS ERROR','UDST_GET_VERT_LINK',
     &        'VERTEX ID NOT UNIQUE','W')
          ENDIF
        ENDIF
      ENDDO

      IF (.NOT.FOUND) THEN
        CALL ERRMSG(' ','UDST_GET_VERT_LINK','VERTEX NOT FOUND','W')
      ENDIF

  999 RETURN
      END
