      SUBROUTINE ECDUMP(ILUN,IOPTS,NOPTS,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Dump an event catalog file.
C-
C-   INPUTS  : ILUN    - LOGICAL UNIT OF EVENT CATALOG.
C-             IOPTS(1)- TRUE ==>DUMP EVENTS
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER NOPTS
      INTEGER ILUN,IOPTS(NOPTS),IERR,I,J,NR,NEV,LEVT,EXTRA,CHECK
      INTEGER IREC(JSIZE),IEREC(JSIZE),LR,IRECNO,IZERO(2),IVER,ID,IT
      INTEGER NEVTOT,NALLOC,IRPREC,IRECEC,ISPACE,ISDIRT,INOORD,INDRCT
      REAL    EFFI
      CHARACTER*8 CSTR
      LOGICAL D0DAD_RE_RANGE,LEVENT
      EXTERNAL D0DAD_RE_RANGE
C----------------------------------------------------------------------
C
C  Make sure the unit/bank correspondance is correct
C
      CALL ECLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
         IERR = -4
         GOTO 999
      ENDIF
C
      LEVENT=IOPTS(1)
      CALL VZERO(IZERO,2)
      NEVTOT=0
      NALLOC=0
      CALL UHTOC(IQ(LECHD+NDEC+1),4,CSTR,8)
      IVER=IQ(LECHD+NDEC+JEC_RECVER)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      NR=IQ(LECHD+NDEC+JEC_IECRUN)
      ISPACE=IQ(LECHD+NDEC+JEC_IRUN1)-IQ(LECHD+NDEC+JEC_IRUN0)+1
      ISPACE=ISPACE*IRPREC
      ISDIRT=IQ(LECHD+NDEC+JEC_ISDIRT)
      INOORD=IQ(LECHD+NDEC+JEC_INOORD)
      INDRCT=IQ(LECHD+NDEC+JEC_INDRCT)
      EXTRA=IQ(LECHD+NDEC+JEC_EXTRA)
      CHECK=IQ(LECHD+NDEC+JEC_RCHECK)
C
      WRITE(*,1001) CSTR,IVER,CECTAG,NR,ISPACE,ISDIRT,INOORD,INDRCT,
     > EXTRA,CHECK
C
      IF( IOPTS(1).NE.0 ) THEN
         WRITE(*,2001)
      ELSE
         WRITE(*,1002)
      ENDIF      
      DO 10 I=1,NR
         LR=LRUNS+IRECEC*(I-1)+1
         CALL UCOPY(IQ(LR),IREC,IRECEC)
         IF( .NOT.D0DAD_RE_RANGE(IOPTS(2),IZERO,IREC(1),IZERO)) GOTO 10
         NEV=IRPREC*(IREC(3)-IREC(2)+1)
         ID=IAND(IREC(6),2**20-1)
         IT=ISHFT(IREC(6),-20)
         WRITE(*,1003) IREC(JRUNNO),IREC(JNEVTS),IREC(JLRBEG),
     +      IREC(JLREND),NEV,ID,IT,IREC(JCHECK)
         IF( IOPTS(1).NE.0 ) THEN
            CALL ECGRUN(IREC(1),IRECNO,IERR)
            IF( IERR.NE.0 ) THEN
              IERR = -1
              GOTO 999
            ENDIF
            DO 20 J=1,IREC(4)
               LEVT=LRDAT+IRECEC*(J-1)+1
               CALL UCOPY(IQ(LEVT),IEREC,IRECEC)
               IF( .NOT.D0DAD_RE_RANGE(IOPTS(2),
     +              IOPTS(4),IREC(1),IEREC(1)) ) GOTO 20
               IF( .NOT.LEVENT ) GOTO 20
               WRITE(*,2002) IEREC(1),IEREC(2),IEREC(3),IEREC(4),
     +             IEREC(5),IEREC(6)
 20         CONTINUE
         ENDIF
         NEVTOT=NEVTOT+IREC(4)
         NALLOC=NALLOC+NEV
 10   CONTINUE
      EFFI=1.0
      IF( NALLOC.NE.0 ) EFFI=FLOAT(NEVTOT)/FLOAT(NALLOC)
      WRITE(*,1004) NEVTOT,NALLOC,100.0*EFFI
C
 999  CONTINUE
      RETURN
 1001 FORMAT(/,' HEADER:',/,
     +         '    Type: ',A8,/,
     +         '    Version: ',I6,/,
     +         '    Tag: ',A20,/,
     +         '    Number of runs stored: ',I6,/,
     +         '    Maximum number of runs: ',I6,/,
     +         '    Dirty flag: ',I8,', NoOrder: ',L4,
     +                      ', Indirect:',L4,/,
     +         '    Number of extra records on end:',I5,/,
     +         '    Run section checksum: ',Z8.8,/)
 1002 FORMAT(/,' RUN DATA:',/,
     +  '    Run   No. Events  First Rec.  Last Rec.   ',
     +          'Maxevents    T(Update)    Checksum')
 1003 FORMAT(I8,1X,I8,4X,I8,4X,I8,4X,I8,4X,I6,1X,I4.4,4X,Z8.8)
 1004 FORMAT(9X,I8,27X,I9/,
     +     '      Space effificiency = ',F8.2,'%',/,/)
 2001 FORMAT(/,' RUN DATA:',/,
     +  '    Run   No. Events  First Rec.  Last Rec.   ',
     +                          'Maxevents    T(Update)    Checksum',
     +  '      Event       Stream   Fileid   Record    Byte')
 2002 FORMAT(73X,'       ',I8,2X,2(Z8.8),3I8)
      END
