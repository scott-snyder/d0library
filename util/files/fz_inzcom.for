      SUBROUTINE INZCOM(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Initialize ZEBCOM (event data Zebra common)
C-
C-   Inputs  : I = 1 data in division 1, otherwise in division 2
C-   Outputs : NONE
C-
C-   Created  28-OCT-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=2000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    ! store number
     &       ,IXMAIN   ! event division number
     &       ,IXDVR    ! run division number
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     ! pointer to event HEAD bank
      INTEGER LHEADR    ! pointer to begin run HEAD bank
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
C
      INTEGER I
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
C
C **** Initialize store in /ZEBCOM/ common (store 0)
C
        IXCOM=0
        CALL MZSTOR (IXCOM,'/ZEBCOM/','Q',FENCE,LHEAD,LREF(1),ZSTOR(1),
     &   ZSTOR(40000),ENDZS)
C                                        
C **** Use division IXMAIN for event data
C
        IXMAIN=IXCOM+2
        IF(I.EQ.1) IXMAIN=IXCOM+1
C
C **** Create a division for run header (3rd division)
C
        CALL MZDIV(IXCOM,IXDVR,'RUN DIV',100,40000,'L')
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
