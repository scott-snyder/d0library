      SUBROUTINE TOP_LEPTONS_UTIL_DECODE_IFW2(LPMUO,NO_BITS_SET,
     1  IFW2_BITS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Muon IFW2 flag bit pattern
C-
C-   Inputs  : 
C-              LPMUO - pointer to PMUO Bank
C-   Outputs : 
C-              NO_BITS_SET - no. of non zero bits found
C-              IFW2_BITS   - array of set bits
C-              IER         - error flag = -1/1 for bad/good call
C-   Note :
C-              Internal accounting numbers bits for 1 as LSB
C-              whereas D0 Convention is for 0 as LSB. This is
C-              corrected in the stored information returned from
C-              the routine.
C-   Controls: 
C-
C-   Created  19-OCT-1992   Stephen J. Wimpenny
C-   Modified 16-May-1994   Scintillator Veto Masking Added
C-   Modified 20-May-1994   Run 1a scintillator information explicitly
C-                          suppressed (unusable - Nancy Grossman).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPMUO,NO_BITS_SET,IFW2_BITS(32)
      INTEGER JBIT,ITEMP,IV,IER,GZHEAD
      INTEGER NO_RUN,MC_RUN_CUT,START_RUN_1B
C
      DATA MC_RUN_CUT,START_RUN_1B / 40000, 70000/
C
      IER=-1
      NO_BITS_SET=0
      CALL VZERO(IFW2_BITS,32)
C
      IF(LPMUO.GT.0) THEN
C
C *** loop over word and unpack set bits
C
        ITEMP=IQ(LPMUO+44)
        DO IV=1,20
          IF(IV.LT.17) THEN
            IF(JBIT(ITEMP,IV).EQ.1) THEN
              NO_BITS_SET=NO_BITS_SET+1
              IFW2_BITS(NO_BITS_SET)=IV-1
            ENDIF
          ELSEIF(IV.EQ.17) THEN
C
C *** Test for valid Scintillator Veto Configuration
C *** (Bit 16). Skip higher bits if not avialable
C *** .... as information is not meaningful
C
            IF(JBIT(ITEMP,IV).NE.1) GO TO 100
C
C *** OK data available, but is it trustworty ? All Run1a
C *** information is unusable so drop it on the basis of
C *** run number
C
      NO_RUN=-9999
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
        NO_RUN=IQ(LHEAD+6)
      ENDIF
      IF(NO_RUN.GT.MC_RUN_CUT.AND.NO_RUN.LT.START_RUN_1B) GO TO 100
C
C *** Store Scintillator Available Bit
C
            NO_BITS_SET=NO_BITS_SET+1
            IFW2_BITS(NO_BITS_SET)=IV-1
          ELSEIF(IV.EQ.18) THEN
C
C *** Look at Bit 17 .... if set => matching scintillator hit
C *** was found (ie. probably a Cosmic)
C
            IF(JBIT(ITEMP,IV).EQ.1) THEN
              NO_BITS_SET=NO_BITS_SET+1
              IFW2_BITS(NO_BITS_SET)=IV-1
            ENDIF
          ELSE
C
C *** Disregard other bits as information is not useful
C *** as yet (5/17/94)
C
          ENDIF
        ENDDO
  100   CONTINUE
        IER=1
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
