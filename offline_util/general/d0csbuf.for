      SUBROUTINE D0CSBUF(IBUF,IOWAY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Channel mode remote node I/O routine.
C-
C-   Inputs  : IBUF  -  Date buffer
C-             IOWAY -  0 = input
C-                      1 = output
C-             IQUEST(1) - Unit number
C-             IQUEST(2) - NBUF, number of machine words to write.
C-             IQUEST(3) - First block of event of .ne. 0.
C-   Outputs : IQUEST(1) - Error code.  (0 = OK, positive = error).
C-
C-   Created   4-MAY-1993   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER IBUF(*)
      INTEGER IOWAY
      INTEGER NREC(100)
      INTEGER IRC
      INTEGER NWANT, NGOT, LUN, IREC, ZLUN
      LOGICAL START
      LOGICAL DIRECT
      SAVE NREC
      DATA NREC/100*0/
C----------------------------------------------------------------------
      DIRECT = .FALSE.
      GO TO 1
C-
C- Direct access I/O entry point
C-
      ENTRY D0CSBUFD(IBUF,IOWAY)
      DIRECT = .TRUE.
C-
    1 CONTINUE
      LUN = IQUEST(1)
      NWANT = 4*IQUEST(2)
      START = IQUEST(3).NE.0
      IREC = LUN
      IF(IREC.LT.1.OR.IREC.GT.99)IREC = 100
   10 CONTINUE
      IF(IOWAY.EQ.0)THEN
        IF(DIRECT)THEN
          NREC(IREC) = NREC(IREC) + 1
          CALL XZREAD(LUN,IBUF,NREC(IREC),NWANT,NGOT,' ',IRC)
        ELSE
          CALL XZREAD(LUN,IBUF,0,NWANT,NGOT,' ',IRC)
        ENDIF
        IQUEST(2) = NGOT/4
        IF(IRC.EQ.0.AND.START)THEN
          IF(IBUF(1).NE.'0123CDEF'X.AND.IBUF(1).NE.'EFCD2301'X)GO TO 10
          IF(IBUF(2).NE.'80708070'X.AND.IBUF(2).NE.'70807080'X)GO TO 10
          IF(IBUF(3).NE.'4321ABCD'X.AND.IBUF(3).NE.'CDAB2143'X)GO TO 10
          IF(IBUF(4).NE.'80618061'X.AND.IBUF(4).NE.'61806180'X)GO TO 10
        ENDIF
      ELSE
        IF(DIRECT)THEN
          NREC(IREC) = NREC(IREC) + 1
          CALL XZRITE(LUN,IBUF,NREC(IREC),NWANT,' ',IRC)
        ELSE
          CALL XZRITE(LUN,IBUF,0,NWANT,' ',IRC)
        ENDIF
      ENDIF
      IQUEST(1) = IABS(IRC)
      GO TO 999
C-
C- Entry point to zero record count
C-
      ENTRY D0CSINIT(ZLUN)
      IREC = ZLUN
      IF(IREC.LT.1.OR.IREC.GT.99)IREC = 100
      NREC(IREC) = 0
  999 RETURN
      END
