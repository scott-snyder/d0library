      SUBROUTINE DMPPRH(LUN,IZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Hexadecimal dump to unit LUN of raw data Zebra bank
C-      Machine dependent 
C-
C-   Inputs  : 
C-   LUN = unit number
C-   IZ  = link
C-
C-   Created   6-SEP-1988   Serban D. Protopopescu
C-   Updated  25-Feb-1992   Herbert Greenlee
C-     Got rid of machine blocks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:ZEB_RAW_NAMES.DEF'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      CHARACTER*4 NAME
      INTEGER LUN,IZ,LZ,ND,I,K
      INTEGER I4(10)
      INTEGER*2 I2(20)
      EQUIVALENCE (I2 ,I4 )
C----------------------------------------------------------------------
C
      IF ( LHEAD.GT.0 ) THEN
        LZ=LQ(LHEAD-IZ)
        ND=IQ(LZ-1)
C
C              print header
        WRITE(LUN,1000) (IQ(LHEAD+K),K=6,11),IQ(LZ-4),IZ
C
C        check for right name
        IF(IZ.LT.MAXNAM) THEN
          CALL UHTOC(IQ(LZ-4),4,NAME,4)
          IF(NAME.NE.ZEBRAW(IZ)) WRITE(LUN,1002) ZEBRAW(IZ)
        ENDIF
        WRITE(LUN,1003) (K,K=1,10)
C
C        print content
        DO 1 I=1,ND,10
          IF(MOD(I,1000).EQ.1) WRITE(LUN,1003) (K,K=1,10)
          CALL UCOPY(IQ(LZ+I),I4,10)
          WRITE(LUN,1001) I,(I2(K+WORD1),I2(K+WORD2),K=0,19,2)
 1001     FORMAT(1X,I5,10(3X,Z4,1X,Z4))
    1   CONTINUE
C
      ELSE
        WRITE(LUN,*) ' >>>NO EVENT INFORMATION IN THIS RECORD<<<'
      ENDIF
C
  999 RETURN
C
 1000 FORMAT(1H1,/' Run#',I5,',  Beam crossing#',2I10,',  event#',I6,
     &  ',  event type=',I4,',  trigger type=',I5,
     &  //,' Hexadecimal dump for bank= ',A4,' (link=',I2,')')
 1003 FORMAT(//,13X,10(I2,10X))
 1002 FORMAT('  Name should be= ',A4)
      END
