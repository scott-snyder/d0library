      SUBROUTINE PYTFFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills PYTHIA end of run bank ISAF
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-FEB-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAF.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER NGEN
      REAL XSEC
      INTEGER IOHEAD,IOISAF,LOUT,LISAF,IUH,ITEMP(3)
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C
      CALL UCOPY (IQ(LHEAD+4),ITEMP,3)
      CALL MZWIPE(0)
C                                                        
C  create head bank
C
      CALL BKHEAD
C
C  identify this as a PYTHIA end record
C
      IQ(LHEAD+1)=1002
      CALL UCTOH('PYTH',IUH,4,4)
      IQ(LHEAD+2)=IUH
      CALL UCTOH('END ',IUH,4,4)
      IQ(LHEAD+3)=IUH
      CALL UCOPY(ITEMP,IQ(LHEAD+4),3)
      IQ(LHEAD+14)=1
      LOUT=LHEAD+2
C
C  create Zebra bank ISAF  (end record)
C
      IF(FIRST) CALL MZFORM('ISAF','2I-F',IOISAF)
      CALL MZBOOK(IXMAIN,LISAF,LHEAD,-IZISAF,
     $            'ISAF',0,0,4,IOISAF,-1)
      IQ(LISAF+1)=NGEN(0,3)             ! Number of points generated
      IQ(LISAF+2)=NGEN(0,1)             ! Number of points tried
      Q(LISAF+3)=XSEC(0,3)              ! Total cross section
      IF(NGEN(0,3).GT.0) Q(LISAF+4)=XSEC(0,3)/NGEN(0,3)
      FIRST=.FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
