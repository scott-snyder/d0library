      SUBROUTINE ISARCP_BFL(IRN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     book and fill ISAB (ISAJET begin-of-run) bank
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-   Updated  10-NOV-1989   Rajendran Raja  Added runno as argument 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:KEYS.INC'
      INCLUDE 'D0$INC:JETLIM.INC'
      INCLUDE 'D0$INC:FIXPAR.INC'
      INCLUDE 'D0$INC:SGNPAR.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER K,IOHEAD,IOISAB,LOUT,LISAB,NREAC,IUH,IRN,IER,ICOMMAND
      INTEGER N_ENDS
      LOGICAL FIRST,DO_ONE_TRACK
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C
C
C  set flag for reaction
       DO 1 K=1,6
       IF(KEYS(K)) NREAC=K
    1  CONTINUE
C
C  create head bank
      IF(FIRST) CALL MZFORM('HEAD','1I 2H 11I',IOHEAD)
      CALL MZBOOK(IXMAIN,LHEAD,LHEAD,1,
     $            'HEAD',18,18,14,IOHEAD,0)
C  identify this as an isajet initial record
      IQ(LHEAD+1)=1001
      CALL UCTOH('ISAJ',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('BEG ',IUH,4,4)
      IQ(LHEAD+3)=IUH
      IQ(LHEAD+6)=IRN
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
      CALL UCOPY(PTMIN,Q(LISAB+5),6)
      CALL UCOPY(THMIN,Q(LISAB+11),6)
      CALL UCOPY(PHIMIN,Q(LISAB+17),6)
C
C ****  make ISCM ISAJET command file bank under ISAB
C
      CALL EZPICK('ISARCP_RCP')
      CALL EZGET('DO_ONE_TRACK_EVENTS',DO_ONE_TRACK,IER)
C
      IF(.NOT.DO_ONE_TRACK)THEN
        CALL EZGET('COMMAND_UNIT',ICOMMAND,IER)      
        CALL ISCMFL(ICOMMAND,N_ENDS)
      END IF
      CALL ISRCFL ('ISARCP_RCP')
      CALL EZRSET
      FIRST=.FALSE.
  999 RETURN
      END
