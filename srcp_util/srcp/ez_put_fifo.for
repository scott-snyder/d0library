      SUBROUTINE EZ_PUT_FIFO(LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      EZ_PUT_FIFO and EZ_GET_FIFO provide a means to transfer ASCII
C-      data via internal memory transfer (as opposed to writing and
C-      reading from a disk file).  EZ_PUT_FIFO transfers one line of 
C-      character data into an internal memory buffer.  EZ_GET_FIFO 
C-      transfers one line of character data out of the same internal 
C-      memory buffer on a first-in-first-out basis.
C-
C-      The internal memory buffer is maintained as an unnamed top level
C-      Zebra bank in division 2 of the /ZEBCOM/ store.  The size of
C-      the fifo buffer is determined dynamically and is limited only by 
C-      the size of the store.
C-
C-   Inputs  : LINE        Character variable holding data to be 
C-                         transferred into the fifo buffer (EZ_PUT_FIFO).
C-
C-   Outputs : LINE        Character variable to receive data transferred
C-                         out of the fifo buffer (EZ_GET_FIFO).
C-
C-             IER         Error flag (EZ_GET_FIFO).
C-                           0=success.
C-                           1=no more data.
C-
C-   Created  4-Sep-1993    Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      CHARACTER*(*) LINE
      INTEGER I, IER, NLINK
      INTEGER LFIFO
      INTEGER ND                 ! Current size of FIFO bank
      INTEGER NR                 ! Next word read
      INTEGER NW                 ! Next work to write
      INTEGER NCH, NCHLINE       ! Size (characters) of LINE.
      INTEGER NWD                ! Size (hollerith words) of LINE.
      INTEGER NUSED              ! Used words in FIFO.
      INTEGER NAVAIL             ! Remaining words in FIFO.
      LOGICAL FIRST
      INTEGER LENOCC, LZFIDH
      DATA FIRST/.TRUE./
      SAVE ND, NR, NW, FIRST, NLINK

      integer ihspace/4H    /
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST = .FALSE.
C-
C- Make sure that the /ZEBCOM/ store is initialized.
C-
        CALL INZCOM(2)
C-
C- Reserve a structural link in the general link area /ZLINKA/.
C-
        CALL INZLNK
        CALL GSLINK('EZ_FIFO', NLINK)
      ENDIF
C-
C- Get link of fifo bank.  Book if necessary.
C-
      IF(LSLINK(NLINK).EQ.0)THEN
        ND = 20
        NR = 1
        NW = 1
        CALL MZBOOK(IXMAIN, LFIFO, LSLINK(NLINK), 1, '    ', 0, 0, ND, 
     &    0, -1)
      ENDIF
      LFIFO = LSLINK(NLINK)
C-
C- Confidence builder.
C-
      IF(IQ(LFIFO-4).NE.ihspace )THEN
        CALL ERRMSG('EZ_PUT_FIFO', 'EZ_PUT_FIFO', 'Wrong bank', 'F')
        GO TO 999
      ENDIF
C-
C- Calculate size of input string in characters and words (assume 4 ch./word).
C-
      NCH = LENOCC(LINE)
      NWD = (NCH+3)/4
C-
C- Calculate number of words used and available in FIFO.
C-
      NUSED = NW - NR
      NAVAIL = ND - NW + 1
C-
C- Test available space.  If space is insufficient, first try relocating 
C- data to the top of the bank.
C-
      IF(NAVAIL.LT.NWD+1.AND.NR.GT.1)THEN
        DO I=1,NUSED
          IQ(LFIFO+I) = IQ(LFIFO+I+NR-1)
        ENDDO
        NR = 1
        NW = NUSED + 1
        NAVAIL = ND - NW + 1
      ENDIF
C-
C- Test available space again.  Double bank size using MZPUSH until there
C- is sufficient space (or until the program crashes).
C-
      DO WHILE(NAVAIL.LT.NWD+1)
        CALL MZPUSH(IXCOM, LFIFO, 0, ND, 'I')
        ND = 2 * ND
        NAVAIL = ND - NW + 1
      ENDDO
C-
C- Now there is enough space in FIFO.  Transfer LINE and exit.
C-
      IQ(LFIFO+NW) = NCH
      CALL UCTOH(LINE, IQ(LFIFO+NW+1), 4, NCH)
      NW = NW + NWD + 1
      GO TO 999

      ENTRY EZ_GET_FIFO(LINE, IER)
C-
C- ENTRY point to pull a LINE of data from FIFO.
C-
      IER = 0
C-
C- Find bank.
C-
      LFIFO = LSLINK(NLINK)
      IF(LFIFO.EQ.0)THEN
        IER = 1
        GO TO 999
      ENDIF
C-
C- Confidence builder.
C-
      IF(IQ(LFIFO-4).NE.ihspace )THEN
        CALL ERRMSG('EZ_GET_FIFO', 'EZ_GET_FIFO', 'Wrong bank', 'F')
        GO TO 999
      ENDIF
C-
C- Is FIFO empty?
C-
      IF(NR.GE.NW)THEN
        IER = 1
        GO TO 999
      ENDIF
C-
C- FIFO is not empty.  Get next line.
C-
      NCH = IQ(LFIFO+NR)
      NWD = (NCH+3)/4
      NCHLINE = LEN(LINE)
      IF(NCH.GE.NCHLINE)THEN
        CALL UHTOC(IQ(LFIFO+NR+1), 4, LINE, NCHLINE)
      ELSE
        CALL UHTOC(IQ(LFIFO+NR+1), 4, LINE, NCH)
        LINE(NCH+1:NCHLINE) = ' '
      ENDIF
      NR = NR + NWD + 1
  999 RETURN
      END
