      FUNCTION WROSTR(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Set flags to decide whther event shoud be written
C-      to output stream I
C-   Inputs  : 
C-       I  = output stream
C-
C-   ENTRY DOWRIT(I)
C-      return true if one should write to output stream I
C-      false otherwise
C-
C-   Created   7-MAR-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL WROSTR,DOWRIT,DOLIST(8)
      INTEGER I
C----------------------------------------------------------------------
      DATA DOLIST/8*.FALSE./
C
      DOLIST(I)=.TRUE.
      WROSTR=.TRUE.
  999 RETURN
C
      ENTRY DOWRIT(I)
      DOWRIT=DOLIST(I)
      DOLIST(I)=.FALSE.
      RETURN
      END
