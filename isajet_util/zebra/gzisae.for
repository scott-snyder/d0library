      FUNCTION GZISAE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointers to ISAE 
C-
C-   Returned value: present pointer
C-
C-   ENTRY GZISAE_NEXT 
C-   returns pointer to next ISAE on linear chain
C-   (for multiple ISAJET events in same record)
C-   After a call to GZISAE_NEXT next call to
C-   GZISAE will return the same pointer unless GZISAE_NEXT=0.
C-   In the latter case GZISAE returns value of pointer to first
C-   bank in chain.
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISAE,GZISAE_NEXT
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INTEGER LISAE,RUN,ID,PREV_RUN,PREV_ID,PREV_LHEAD
      SAVE LISAE,PREV_RUN,PREV_ID,PREV_LHEAD
      DATA LISAE/0/
      DATA PREV_RUN,PREV_ID,PREV_LHEAD/-1,-1,-1/
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      IF(RUN.NE.PREV_RUN.OR.ID.NE.PREV_ID.OR.LHEAD.NE.PREV_LHEAD) 
     &  LISAE=0
      IF(LHEAD.NE.0.AND.LISAE.EQ.0)  LISAE=LQ(LHEAD-IZISAE)
      GZISAE=LISAE
      PREV_RUN=RUN
      PREV_ID=ID
      PREV_LHEAD=LHEAD
C
      GOTO 999
C
      ENTRY GZISAE_NEXT
      IF(LISAE.NE.0) LISAE=LQ(LISAE)
      GZISAE_NEXT=LISAE
  999 RETURN
      END
