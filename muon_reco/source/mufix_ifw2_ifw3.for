      SUBROUTINE MUFIX_IFW2_IFW3(LPMUO)
C----------------------------------------------------------------------
C-
C-   Purpose: set ifw2 and ifw3 bits in MUFIX
C-
C-   Inputs  : LPMUO = address of support bank
C-
C-   Outputs : none- modifies ifw2 and ifw3 bits in MUOT and PMUO
C-
C-   Created  17-JUL-1995   Paul Quintas
C-   Modified 08-AUG-1995   RE Hall pass only pmuo link- get muot link          
C-                          inside, change also LPMUO+44 to new IFW2
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      
      INTEGER LPMUO, LMUOT, ISET, IFW2, IFW3, NS
      INTEGER WAMHITS(6), SAMHITS(3), NWAMUS, NSAMUS, I
      INTEGER IFILTPT, FILTQ, ESUMBIT
      INTEGER OLD2, OLD3 
      REAL ETA, PHI, FILTPT

      DATA ISET /1/

      NS = IQ(LPMUO-2)
      LMUOT = LQ(LPMUO-NS-1)
      IF(LMUOT.GT.0) THEN

        IFW2 = IQ(LMUOT+5)
        IFW3 = IQ(LMUOT+6)
        OLD2 = IFW2
        OLD3 = IFW3

C-set ifw2 bit 11 (SWW) if this is an overlap track
C-note: I am assuming that there is NO SSW code run

        CALL DECODE_MUON_PLANE_INFO(LPMUO,1,WAMHITS,SAMHITS)
        NWAMUS = 0
        NSAMUS = 0
        DO I = 1, 3
          NWAMUS = NWAMUS + WAMHITS(I) + WAMHITS(I+3)
          NSAMUS = NSAMUS + SAMHITS(I)
        ENDDO
        IF (NWAMUS.GT.0 .AND. NSAMUS.GT.0) THEN
          CALL MVBITS(ISET,0,1,IFW2,11)
        ENDIF

C Match by eta and phi with FILT banks

        ETA = Q(LPMUO+16)
        PHI = Q(LPMUO+17)
        CALL MUOT_FILT(ETA,PHI,FILTPT,FILTQ,ESUMBIT)
        IF (FILTQ.LE.1) THEN
          IFILTPT = IFIX(MIN(FILTPT,15.9))
          CALL MVBITS(IFILTPT,0,4,IFW3,24)
          CALL MVBITS(FILTQ,0,1,IFW3,28)
          CALL MVBITS(ESUMBIT,0,1,IFW3,29)
        ENDIF

C modify word in appropriate banks...

        IQ(LMUOT+5) = IFW2
        IQ(LPMUO+44) = IFW2

        IQ(LMUOT+6) = IFW3

      END IF
      RETURN
      END
