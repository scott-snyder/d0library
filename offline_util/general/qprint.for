C&IF VAXVMS
      OPTIONS /EXTEND_SOURCE
C&ENDIF
      INTEGER FUNCTION QPRINT (FSPEC,QUEUE,FNAME,DELFIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     This subroutine only works on VAX VMS to handle laser
C-     printer plots. It becomes a dummy anywhere else.
C-
C-   Created   in the distant past by Anonymous
C-   changed name from QUEUE_PRINT to QPRINT  10/5/88 SDP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*)   QUEUE,
     &                  FSPEC,
     &                  FNAME
      LOGICAL       DELFIL
C----------------------------------------------------------------------
C&IF VAXVMS
      INTEGER*4       ENTRY_NUMBER

      INCLUDE         '($SJCDef)'

      INTEGER*4       SYS$SNDJBCW
      EXTERNAL        SYS$SNDJBCW

      STRUCTURE       /ITMLST/
        UNION
          MAP
            INTEGER*2       BUFLEN, ITMCOD
            INTEGER*4       BUFADR, RETADR
          END MAP
          MAP
            INTEGER*4       END_LIST
          END MAP
        END UNION
      END STRUCTURE

      STRUCTURE       /IOSBLK/
        INTEGER*4    STS,ZEROED
      END STRUCTURE

      RECORD /ITMLST/ SUBMIT_LIST(6)
      RECORD /IOSBLK/ IOSB

      SUBMIT_LIST(1).BUFLEN = LEN(QUEUE)
      SUBMIT_LIST(1).ITMCOD = SJC$_QUEUE
      SUBMIT_LIST(1).BUFADR = %LOC(QUEUE)
      SUBMIT_LIST(1).RETADR = 0

      SUBMIT_LIST(2).BUFLEN = LEN(FSPEC)
      SUBMIT_LIST(2).ITMCOD = SJC$_FILE_SPECIFICATION
      SUBMIT_LIST(2).BUFADR = %LOC(FSPEC)
      SUBMIT_LIST(2).RETADR = 0

      SUBMIT_LIST(3).BUFLEN = LEN(FNAME)
      SUBMIT_LIST(3).ITMCOD = SJC$_FORM_NAME
      SUBMIT_LIST(3).BUFADR = %LOC(FNAME)
      SUBMIT_LIST(3).RETADR = 0

      SUBMIT_LIST(4).BUFLEN = 4
      SUBMIT_LIST(4).ITMCOD = SJC$_ENTRY_NUMBER_OUTPUT
      SUBMIT_LIST(4).BUFADR = %LOC(ENTRY_NUMBER)
      SUBMIT_LIST(4).RETADR = 0

      IF (DELFIL) THEN
        SUBMIT_LIST(5).BUFLEN = 0
        SUBMIT_LIST(5).ITMCOD = SJC$_DELETE_FILE
        SUBMIT_LIST(5).BUFADR = 0
        SUBMIT_LIST(5).RETADR = 0
        SUBMIT_LIST(6).END_LIST = 0
      ELSE
        SUBMIT_LIST(5).BUFLEN = 0
        SUBMIT_LIST(5).ITMCOD = SJC$_NO_DELETE_FILE
        SUBMIT_LIST(5).BUFADR = 0
        SUBMIT_LIST(5).RETADR = 0
        SUBMIT_LIST(6).END_LIST = 0
      END IF

      QPRINT = SYS$SNDJBCW (,
     &                          %VAL(SJC$_ENTER_FILE),,
     &                          SUBMIT_LIST,
     &                          IOSB,,)
      IF (QPRINT) QPRINT = IOSB.STS
C&ELSE
C&      QPRINT=0
C&ENDIF
      RETURN
      END
