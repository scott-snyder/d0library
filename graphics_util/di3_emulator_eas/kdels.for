      SUBROUTINE KDELS(NAM)
C
C   Routine to erase retained segment NAM and its associated 
C   primitives.
C
      IMPLICIT NONE
      EXTERNAL ERRHND
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      CHARACTER*4  PRIMI
      CHARACTER*30 EMSG
      CHARACTER*4  TSEG
      INTEGER NAM
      INTEGER I, LOCAT, NDSEG, J

      CALL KCLAS
      IF (NSEGS .EQ. 0) RETURN
      NDSEG = NSEGS
      LOCAT = 0
      DO 10 I=1,NSEGS
         IF (SEGINF(1,I) .EQ. NAM) LOCAT = I
   10 CONTINUE
      IF (LOCAT .EQ. 0) THEN
         WRITE (EMSG,100) NAM
  100 FORMAT('KDELS: SEGMENT NOT FOUND ',I5)
         CALL ERROR(EMSG)
      ENDIF
      CALL KBLDN(SEGINF(6,LOCAT), TSEG)
      TSEG = 'R'//TSEG(1:3)
C
C   Remove the segment from the display list.
C
      CALL PREMFR(TSEG//'"', EMDISP, ERRHND)
C
C   Delete the segment.
C
      CALL PDELET(TSEG//'"',ERRHND)
      NDSEG = NDSEG - 1
C
C   Delete the primitives associated with the segment.
C
      DO 30 I=SEGINF(4,LOCAT),SEGINF(5,LOCAT)
         CALL KBLDN (I, TSEG)
         CALL PDELET('K'//TSEG(1:3)//'"', ERRHND)
   30 CONTINUE
C
C   Delete entry from internal memory.
C
      DO 40 I=LOCAT,NDSEG
         DO 40 J=1,6
            SEGINF(J,I) = SEGINF(J,I+1)
   40 CONTINUE

      NSEGS = NDSEG
      IF (NSEGS .EQ. 0) THEN
         CALL PPURGE(ERRHND)
      ENDIF
      RETURN
      END
