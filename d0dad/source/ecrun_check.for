      LOGICAL FUNCTION ECRUN_CHECK
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-Jul-1994   John D. Hobbs
C-   Modified 19-Jan-1995   JDH - Add checksum testing...
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IRPREC,IRECEC,NRUNS,I,J,NEVTS,NERECS,NRECS,CHECK
      INTEGER CHECKSUM32
      LOGICAL EVEN
C-----------------------------------------------------------------------
C
      ECRUN_CHECK=.TRUE.
      IF( LRUNS.LE.0 ) GOTO 999
C
C  Check that the record range is consistant with number of events...
C
C
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)
C
      DO I=1,NRUNS
        J=IRECEC*(I-1)
        NEVTS=IQ(LRUNS+J+4)
        NRECS=IQ(LRUNS+J+3)-IQ(LRUNS+J+2)+1
        NERECS=MAX(NEVTS-1,0)/IRPREC + 1
        EVEN = MOD(NEVTS,IRPREC).EQ.0
        IF( NRECS.NE.NERECS .AND. .NOT.EVEN ) ECRUN_CHECK=.FALSE.
        IF( (NRECS.LT.NERECS .OR. NRECS.GT.(NERECS+1)) .AND.
     >        EVEN ) ECRUN_CHECK=.FALSE.
      ENDDO
C
C  Starting with V1.05 of the files, check the checksum...
C
      IF( IQ(LECHD+NDEC+JEC_RECVER).GE.105 ) THEN
        CHECK=CHECKSUM32(IQ(LRUNS+1),NRUNS*IRECEC,0)
        IF( CHECK.NE.IQ(LECHD+NDEC+JEC_RCHECK) ) THEN
          ECRUN_CHECK=.FALSE.
          WRITE(D0DAD_ERRTXT,9001) CHECK,IQ(LECHD+NDEC+JEC_RCHECK)
 9001     FORMAT('Checksum failed. Old value=',I10,', Current=',I10)
          IF( LDDBG.GT.0) CALL ERRMSG('EC_CHECKSUM','ECRUN_CHECK',
     >      D0DAD_ERRTXT,'E')
        ENDIF
      ENDIF
C
      IF( .NOT.ECRUN_CHECK .AND. LDDBG.GT.0 ) THEN
        D0DAD_ERRTXT='Corrupted run section'
        CALL ERRMSG('ECCORRUPT','ECRUN_CHECK',D0DAD_ERRTXT,'E')
      ENDIF
C
 999  RETURN
      END
