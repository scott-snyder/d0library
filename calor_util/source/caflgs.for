      SUBROUTINE CAFLGS(IW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Set calorimeter data type flags by interpreting
C-       controller word
C-
C-   Input:
C-   IWORD= controller word
C-
C-   Outputs :
C-   NOTPED= true if this is not pedestal data
C-   PULSON= true if pulser is on
C-   PEDSUB= true if pedestal subtracted
C-   ZSUP  = true if zero suppressed
C-   GNCORR= true if data is renormalized (gain corrected)
C-
C-   Created  23-SEP-1987   Serban D. Protopopescu
C-   Updated  15-APR-1992   Chip Stewart  Unix compatible 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER IWORD,IW,IW2
      LOGICAL ZSUPRSO,BTEST
      BYTE IWBYTE(4)             
      EQUIVALENCE(IWBYTE,IWORD)  
C----------------------------------------------------------------------
C
      IWORD=IW
C
C         flags from first byte
      NOTPED=IAND(IWORD,1).NE.0
      ZSUPRS=IAND(IWORD,4).NE.0
      SSUPRS=IAND(IWORD,2).NE.0
      PEDSUB=IAND(IWORD,8).NE.0
      GNCORR=.FALSE.
      ZSUPRSO =IAND(IWORD,16).NE.0
      SBUFF=BTEST(IWORD,11)     !IAND(IWORD,'800'X).NE.0
      IF(.NOT.NOTPED) PEDSUB=.FALSE.
      PULSON=BTEST (PLSWRD,23)  !IAND(PLSWRD,'800000'X).NE.0
C         data type from second byte
      IW2=IWBYTE(BYTE2)              
      DATYPE=MOD(IW2,8)
C
  999 RETURN
      END
