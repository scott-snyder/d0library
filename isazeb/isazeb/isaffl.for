      SUBROUTINE ISAFFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Book and fill ISAF (ISAJET end-of-run) bank
C-
C-   Created   7-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:IDRUN.INC'
      INCLUDE 'D0$INC:FINAL.INC'
      INCLUDE 'D0$INC:PRIMAR.INC'
      INCLUDE 'D0$LINKS:IZISAF.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IOHEAD,IOISAF,LOUT,IRN,LISAF,IUH,ITEMP(3)
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C
      CALL UCOPY (IQ(LHEAD+4),ITEMP,3)
      CALL MZWIPE(0)
C                                                        
C  create head bank
      CALL BKHEAD
C  identify this as an isajet end record
      IQ(LHEAD+1)=1002
      CALL UCTOH('ISAJ',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('END ',IUH,4,4)
      IQ(LHEAD+3)=IUH
      CALL UCOPY(ITEMP,IQ(LHEAD+4),3)
      IQ(LHEAD+14)=1
      LOUT=LHEAD+2
C
C  create Zebra bank ISAF  (end record)
      IF(FIRST) CALL MZFORM('ISAF','2I-F',IOISAF)
      CALL MZBOOK(IXMAIN,LISAF,LHEAD,-IZISAF,
     $            'ISAF',0,0,4,IOISAF,-1)
      IQ(LISAF+1)=IEVT
      IQ(LISAF+2)=NEVENT
      Q(LISAF+3)=SIGF*1000.
      IF(IEVT.GT.0) Q(LISAF+4)=SIGF/IEVT*1000.
      FIRST=.FALSE.
  999 RETURN
      END
