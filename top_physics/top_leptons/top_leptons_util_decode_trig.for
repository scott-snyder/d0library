      SUBROUTINE TOP_LEPTONS_UTIL_DECODE_TRIG(NO_L1_SET,L1_BITS_SET,
     1 NO_L2_SET,L2_BITS_SET,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns List of Level 1 and Level 2
C-                         Trigger bits which are set for current 
C-                         event
C-
C-   Inputs  : None
C-
C-   Outputs : 
C-             NO_L1_SET    No. of Level 1 trigger bits set
C-             L1_BITS_SET  Array containing id's of set bits
C-             NO_L2_SET    No. of Level 2 trigger bits set
C-             L2_BITS_SET  Array containing id's of set bits
C-             IER = -1/1   Bad/OK
C-
C-   Controls: None
C-
C-   Created   9-OCT-1992   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name cahnged for library compatibility
C-                          (was Which_Trig_Bits)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER IER,IV,JV,JBIT,TEMP_L1,TEMP1_L2,TEMP2_L2,GZHEAD
      INTEGER NO_L1_SET,NO_L2_SET,L1_BITS_SET(32),L2_BITS_SET(64)
C
      IER=-1
      NO_L1_SET=0
      NO_L2_SET=0
      CALL VZERO(L1_BITS_SET,32)
      CALL VZERO(L2_BITS_SET,64)
C
      LHEAD=GZHEAD()
      IF(LHEAD.NE.0) THEN
        TEMP_L1=IQ(LHEAD+11)
        TEMP1_L2=IQ(LHEAD+15)
        TEMP2_L2=IQ(LHEAD+16)
        DO IV=1,32
          IF(JBIT(TEMP_L1,IV).EQ.1) THEN
            NO_L1_SET=NO_L1_SET+1
            L1_BITS_SET(NO_L1_SET)=IV-1
          ENDIF
          IF(JBIT(TEMP1_L2,IV).EQ.1) THEN
            NO_L2_SET=NO_L2_SET+1
            L2_BITS_SET(NO_L2_SET)=IV-1
          ENDIF
        ENDDO
        DO IV=1,32
          JV=IV+32
          IF(JBIT(TEMP2_L2,IV).EQ.1) THEN
            NO_L2_SET=NO_L2_SET+1
            L2_BITS_SET(NO_L2_SET)=JV-1
          ENDIF
        ENDDO
        IER=1
      ENDIF        
C----------------------------------------------------------------------
  999 RETURN
      END
