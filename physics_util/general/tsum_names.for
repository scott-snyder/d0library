      SUBROUTINE TSUM_NAMES
     &  (NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      give list of triggers that are on for an event
C-      by comparing full list on data base to bit words
C-      on HEAD bank
C-
C-   Inputs  : 
C-     NTRIGON= number of trigger bits on
C-     TRIGBON=  list of trigger bits on
C-     TRIGNON=  list of names of triggers on
C-     NFILTON= number of filter bits on
C-     FILTBON= list of filter bits on
C-     FILTNON= list of names of filters on
C-
C-   ENTRY TSUM_RUNPAR(IER): read in trigger definitions for run
C-     Output:  IER= 0 if succesful read
C-
C-   Created  20-MAR-1992   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NFILTON,NTRIGON
      CHARACTER*(*) TRIGNON(*),FILTNON(*)
      INTEGER TRIGBON(*),FILTBON(*)
      INTEGER RUN,NTRIG,TRIGBIT(32),NFILT,FILTBIT(128),TRIG_FILT(128)
      INTEGER RUNNO,I,IER
      CHARACTER*64 TRIGNAME(32),FILTNAME(128)
      LOGICAL L2BIT_ON,L1BIT_PASSED,FLGVAL,READ_TRIG_FILE,FIRST
      SAVE NTRIG,NFILT,TRIGBIT,TRIGNAME,FILTBIT,FILTNAME
      SAVE READ_TRIG_FILE
      DATA NTRIG,NFILT/0,0/
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C
      NTRIGON=0
      DO I=1,NTRIG
        IF(L1BIT_PASSED(TRIGBIT(I))) THEN
          NTRIGON=NTRIGON+1
          TRIGBON(NTRIGON)=TRIGBIT(I)
          TRIGNON(NTRIGON)=TRIGNAME(I)(1:32)
        ENDIF
      ENDDO
C
      NFILTON=0
      DO I=1,NFILT
        IF(L2BIT_ON(FILTBIT(I))) THEN
          NFILTON=NFILTON+1
          FILTBON(NFILTON)=FILTBIT(I)
          FILTNON(NFILTON)=FILTNAME(I)(1:32)
        ENDIF
      ENDDO
      IF(NTRIG+NFILT.GT.0.AND.NTRIGON+NFILTON.EQ.0) THEN
        CALL ERRMSG(' Event has no legitimate triggers',
     &    'TSUM_NAMES',' ','W')
      ENDIF
      GOTO 999
C
C
      ENTRY TSUM_RUNPAR(IER)
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        READ_TRIG_FILE=FLGVAL('READ_TRIG_FILE')
        CALL FLGUBK('READ_TRIG_FILE',1)
      ENDIF
      RUN=RUNNO()
      NTRIG=0
      NFILT=0
      IF(READ_TRIG_FILE) THEN
        CALL READ_TRIG_FILT_RUN_FILE(RUN,NTRIG,TRIGBIT,TRIGNAME,
     &    NFILT,FILTBIT,FILTNAME,TRIG_FILT,IER)
      ELSE
        CALL DBO_GET_TRIGGERS
     &    (RUN,NTRIG,TRIGBIT,TRIGNAME,NFILT,FILTBIT,FILTNAME,IER)
      ENDIF
      IF(IER.NE.0) 
     &  CALL ERRMSG('Could not get trigger names','TSUM_RUNPAR',
     &  ' ','W')
      IF(NTRIG+NFILT.EQ.0.AND.IER.EQ.0) 
     &  CALL ERRMSG(' No trigger bits for this run','TSUM_RUNPAR',
     &  ' ','W')
  999 RETURN
      END
