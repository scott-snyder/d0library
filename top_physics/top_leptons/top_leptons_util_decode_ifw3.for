      SUBROUTINE TOP_LEPTONS_UTIL_DECODE_IFW3(LMUOT,NO_BITS_SET,
     1  IFW3_BITS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Muon IFW3 flag bit pattern
C-
C-   Inputs  : 
C-              LMUOT - pointer to MUOT Bank
C-   Outputs : 
C-              NO_BITS_SET - no. of non zero bits found
C-              IFW3_BITS   - array of set bits
C-              IER         - error flag = -1/1 for bad/good call
C-   Controls: 
C-
C-   Created  16-May-1994   Stephen J. Wimpenny
C-   Modified 20-May-1994   Fix problem with bit/value decoding
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LMUOT,NO_BITS_SET,IFW3_BITS(32)
      INTEGER JBIT,ITEMP,JTEMP,IV,IER,ISHIFT
C
      IER=-1
      ISHIFT=65536
      NO_BITS_SET=0
      CALL VZERO(IFW3_BITS,32)
C
      IF(LMUOT.GT.0) THEN
C
C *** loop over word and unpack set bits
C
        ITEMP=IQ(LMUOT+6)
C        write(12,9876) itemp
C9876    format(' decode IFW3 : Flag word = ',I12)
C
C *** Decode Mixed Orientation Information
C
        JTEMP=ITEMP/ISHIFT
        ITEMP=IQ(LMUOT+6)-JTEMP*ISHIFT
C        write(12,9877) IQ(LMUOT+6),JTEMP,ITEMP,ISHIFT
C 9877   format(' ifw3..... jtemp,itemp,ishift = ',4I8)
        IF(ITEMP.EQ.1) THEN
          NO_BITS_SET=NO_BITS_SET+1
          IFW3_BITS(NO_BITS_SET)=0
        ELSEIF(ITEMP.EQ.2) THEN
          NO_BITS_SET=NO_BITS_SET+1
          IFW3_BITS(NO_BITS_SET)=1
        ELSEIF(ITEMP.EQ.3) THEN
          NO_BITS_SET=NO_BITS_SET+1
          IFW3_BITS(NO_BITS_SET)=2
        ELSEIF(ITEMP.EQ.4) THEN
          NO_BITS_SET=NO_BITS_SET+1
          IFW3_BITS(NO_BITS_SET)=3
        ELSE
        ENDIF
C        if(no_bits_set.gt.0)
C     1    write(12,9875) no_bits_set,(IFw3_bits(IV),Iv=1,no_bits_set)
C
C *** Now Check Trigger Confirm Bits
C          
        DO IV=1,16
          IF(JBIT(JTEMP,IV).EQ.1) THEN
            NO_BITS_SET=NO_BITS_SET+1
            IFW3_BITS(NO_BITS_SET)=IV+15
          ENDIF
        ENDDO
        IER=1
C        if(no_bits_set.gt.0)
C     1    write(12,9875) no_bits_set,(IFw3_bits(IV),Iv=1,no_bits_set)
C 9875   format(' no_bits_set = ',I3,' bits are = ',8i3)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
