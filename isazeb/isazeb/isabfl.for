      SUBROUTINE ISABFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     book and fill ISAB (ISAJET begin-of-run) bank
C-
C-   ENTRY ISA_STOP(DONE)
C-   Output:
C-   DONE = no more runs in command file
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:ITAPES.INC'
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:JETLIM.INC'
      INCLUDE 'D0$INC:FIXPAR.INC'
      INCLUDE 'D0$INC:SGNPAR.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER K,IOHEAD,IOISAB,LOUT,LISAB,NREAC,IUH
      INTEGER ISA_RUNNO,NRUNS,N_ENDS
      LOGICAL FIRST,FLGVAL,DONE
      SAVE FIRST,NRUNS,N_ENDS
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
      DATA NRUNS,N_ENDS/0,1/
C
      NRUNS=NRUNS+1
      IF(NRUNS.GT.N_ENDS) GOTO 999
C
C  set flag for reaction
      DO 6 K=1,6
        IF(KEYS(K)) NREAC=K
    6 CONTINUE
C
C  create head bank
      CALL BKHEAD
C  identify this as an isajet initial record
      IQ(LHEAD+1)=1001
      CALL UCTOH('ISAJ',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('BEG ',IUH,4,4)
      IQ(LHEAD+3)=IUH
      IQ(LHEAD+6)=ISA_RUNNO()
      IQ(LHEAD+14)=1
      LOUT=LHEAD+2
C                                        
C  create Zebra bank ISAB 
      IF(FIRST) CALL MZFORM('ISAB','3I,-F',IOISAB)
      CALL MZBOOK(IXMAIN,LISAB,LHEAD,-IZISAB,
     $            'ISAB',2,2,22,IOISAB,-1)
      IQ(LISAB+1)=IDVER
      IQ(LISAB+2)=NREAC
      IQ(LISAB+3)=NEVENT
      Q(LISAB+4)=ECM
      IF(.NOT.FLGVAL('PARTONS')) THEN
        CALL UCOPY(PTMIN,Q(LISAB+5),6)
        CALL UCOPY(THMIN,Q(LISAB+11),6)
        CALL UCOPY(PHIMIN,Q(LISAB+17),6)
        CALL ISCMFL(ITCOM,N_ENDS)
      ENDIF
      GOTO 999
C
      ENTRY ISA_STOP(DONE)
      DONE=.FALSE.
      IF(NRUNS.GE.N_ENDS) DONE=.TRUE.
C
  999 RETURN
      END
