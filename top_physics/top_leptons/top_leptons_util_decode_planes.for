      SUBROUTINE TOP_LEPTONS_UTIL_DECODE_PLANES(LPMUO,ICALL,
     1  WAM_HIT,SAM_HIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return unpacked arrays of muon plane 
C-                         information = extracted from PMUO vers3
C-                         words 46 and 47.
C-
C-   Inputs  : 
C-              LPMUO - pointer to current PMUO Bank
C-              ICALL - 1/2 available / used hits
C-
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JAN-1993   Stephen J. Wimpenny
C-   Modiied  10-Feb-1993   Typo in Wamus A' decoding fixed
C-   Modified 21-Mar-1993   Name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPMUO,ICALL,IOFF
      INTEGER WAM_HIT(6),SAM_HIT(3)
C
      IOFF=45
      IOFF=IOFF+ICALL
      CALL VZERO(WAM_HIT,6)
      CALL VZERO(SAM_HIT,3)
C
C *** Wamus 6 : B' + C' (mixed orientation)
C ***       5 : A' (mixed orientation)
C 
      WAM_HIT(6)=(IQ(LPMUO+IOFF)/100000000)
      WAM_HIT(5)=(IQ(LPMUO+IOFF)/10000000)  
C
C *** Samus 3 : C Station
C ***       2 : B Station
C ***       1 : A Station
C
      SAM_HIT(3)=(IQ(LPMUO+IOFF)/1000000) 
      SAM_HIT(2)=(IQ(LPMUO+IOFF)/100000)
      SAM_HIT(1)=(IQ(LPMUO+IOFF)/10000)
C
C *** Wamus 4 : D Layer (EF)
C ***       3 : C Layer
C ***       2 : B Layer
C ***       1 : A Layer
C
      WAM_HIT(4)=(IQ(LPMUO+IOFF)/1000)
      WAM_HIT(3)=(IQ(LPMUO+IOFF)/100)
      WAM_HIT(2)=(IQ(LPMUO+IOFF)/10)
      WAM_HIT(1)=(IQ(LPMUO+IOFF))
C
      WAM_HIT(1)=WAM_HIT(1)-WAM_HIT(2)*10
      WAM_HIT(2)=WAM_HIT(2)-WAM_HIT(3)*10
      WAM_HIT(3)=WAM_HIT(3)-WAM_HIT(4)*10
      WAM_HIT(4)=WAM_HIT(4)-SAM_HIT(1)*10
      SAM_HIT(1)=SAM_HIT(1)-SAM_HIT(2)*10
      SAM_HIT(2)=SAM_HIT(2)-SAM_HIT(3)*10
      SAM_HIT(3)=SAM_HIT(3)-WAM_HIT(5)*10
      WAM_HIT(5)=WAM_HIT(5)-WAM_HIT(6)*10
C
C----------------------------------------------------------------------
  999 RETURN
      END
