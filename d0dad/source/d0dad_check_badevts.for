      SUBROUTINE D0DAD_CHECK_BADEVTS(ECLUN,FID,RUN)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-OCT-1996   John Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ECLUN,FID,RUN
C- Temporary local variables
      INTEGER IREC,IERR,IRECEC,IOFFS,NEVTS,NIN,EVENT,THIS_FID,CHAROFF,I
      CHARACTER*80 OUTSTR
C-----------------------------------------------------------------------
C
      CALL ECGRUN(RUN,IREC,IERR)
      IF( IERR.NE.0 ) THEN
        WRITE(*,*) ' Error ',IERR,' getting run ',run,' from EC.'
        GOTO 999
      ENDIF

C- Do not use the ECGET/ECPUT interface.  Instead just loop...

      NIN=1
      OUTSTR=' '
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IOFFS = IRECEC*(IREC-1)
      NEVTS = IQ(LRUNS+IOFFS+JNEVTS)
      DO I=1,NEVTS
        IOFFS = IRECEC*(I-1)
        THIS_FID = IQ(LRDAT+IOFFS+JFID)
        IF( THIS_FID.EQ.FID ) THEN
          EVENT = IQ(LRDAT+IOFFS+JEVNT)
          CHAROFF = (NIN-1)*7+8
          WRITE(OUTSTR(CHAROFF:CHAROFF+6),'(I7)') EVENT
          IF( NIN.EQ.10 ) THEN
            WRITE(*,1001) OUTSTR
 1001       FORMAT(' ',A78)
            NIN=0
            OUTSTR=' '
          ENDIF
          NIN=NIN+1
        ENDIF
      ENDDO
C
      IF( NIN.GT.1 ) WRITE(*,1001) OUTSTR
C
  999 RETURN
      END
