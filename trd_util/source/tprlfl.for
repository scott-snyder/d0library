      SUBROUTINE TPRLFL(LTPRL,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TPRL
C-
C-   Inputs  :LTPRL = link of bank to be filled.
C-                  < 0, routine will get link using GZTPRL
C-                  = 0, routine will book bank.
C-            LAYER =TRD LAYER NUMBER
C-            
C-   Outputs :
C-   Controls:
C-
C-   Created  30-OCT-1989 17:56:44.87  A. Zylberstej
C-   Updated  18-APR-1991   A. Zylberstejn   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LAYER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER LTPRL, GZTPRL
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
      ENDIF

C
      IF(LTPRL.LT.0)LTPRL = GZTPRL(LAYER)! GET LINK.
C
      IF(LTPRL.EQ.0)CALL BKTPRL(LTPRL,LAYER)        
C Book the bank if argument = 0.
C
      IQ(LTPRL+1) = 1               ! Bank version
C fill in the rest of the bank here.
C----------------------------------------------------------------------
  999 RETURN
      END

