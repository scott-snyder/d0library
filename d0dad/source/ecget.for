      SUBROUTINE ECGET(ILUN,IRUN,IEVENT,IMSK,IFID,IZRN,IZBO,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Get a data record from a d0dad
C-      Event catalog.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IRUN,IEVENT,IMSK(*),IFID,IZRN,IZBO,IERR,II
      INTEGER IREC,IDAT(JSIZE),LTMP,IR,ND,IRECEC
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
      IF( IRUN.LT.0 ) THEN
C
C    Read as if it is a sequential file...
C
         IF( LDDBG.GT.10 .AND. IQ(LECHD+JIEVT).EQ.0 ) WRITE(*,9004)
 9004    FORMAT(' ECGET: Getting using ECGNXT')
         CALL ECGNXT(IRUN,IDAT,6,IERR)
         IF( IERR.EQ.1 ) GOTO 999
         IF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9003) IERR
 9003       FORMAT(' ECGET: Error ',I3,' Returned from ECGNXT')
            IERR = -3
         ENDIF
         IEVENT=IDAT(1)
         IMSK(1)=IDAT(2)
         IMSK(2)=IDAT(3)
         IFID=IDAT(4)
         IZRN=IDAT(5)
         IZBO=IDAT(6)
         GOTO 999
C
      ELSE
C
C   Scan as a direct access binary list
C
C     Get the run data into memory...
C
         CALL ECGRUN(IRUN,IREC,IERR)
         IF( IERR.NE.0 ) THEN
            IF( LDDBG.GT.10 ) WRITE(*,9001) IRUN
 9001       FORMAT(' ECGET: Run ',I6,' not found in EC. ')
            IERR = -1
            GOTO 999
         ENDIF
C
C     Find the event...
C
         IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
         ND=IQ(LRUNS+IRECEC*(IREC-1)+4)
         CALL ECFIND(IQ(LRDAT+1),ND,IRECEC,IEVENT,JEVNT,IREC,IDAT,IERR)
         IF( IERR.EQ.1 ) THEN
            IF( LDDBG.GT.10 ) WRITE(*,9002) IEVENT
 9002       FORMAT(' ECGET: Event ',I6,' not found in EC. ')
            IERR = -2
         ELSEIF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.10 ) WRITE(*,9005) IERR
 9005       FORMAT(' ECGET: Error ',I6,' returned from ECFIND')
            IERR = -3
         ELSE
            IMSK(1)=IDAT(2)
            IMSK(2)=IDAT(3)
            IFID=IDAT(4)
            IZRN=IDAT(5)
            IZBO=IDAT(6)
            IERR = 0
         ENDIF
      ENDIF
C
 999  CONTINUE
      RETURN
      END
