
      SUBROUTINE CJET_MUCAEPFL(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Book and make a copy of the CAEP bank.  Put this CAEP where old
C-      one was and leave old one attached to new one, so when new one 
C-      is dropped, old one will be restored.
C-
C-   Inputs  : NONE
C-   Outputs : OK - TRUE if CAEP filled
C-   Controls: CAHITS_RCP 
C-
C-   Created   1-MAR-1993   Alex Smith
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INCLUDE 'D0$LINKS:IZCAHT.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
C
      INTEGER   LCAEP_SAVE,NCH,IWORD,NR
      INTEGER   GZCAEP,GZCAHT,PT_CAEP
      LOGICAL   OK
C----------------------------------------------------------------------
      OK = .FALSE.
      LCAEP = GZCAEP()
      IF (LCAEP .LE. 0 ) THEN
        CALL ERRMSG
     &    ('CJET_MUCAEPFL NO CAEP','CJET_MUCAEPFL',
     &    'Unable to copy CAEP','W')
        GOTO 999
      END IF
      NCH = IQ(LCAEP+3)
      NR = IQ(LCAEP+2)
C
C *** Book new CAEP, verify that new CAEP is second in linear structure:
C
      CALL BKCAEP_2(NCH,LCAEP_SAVE)
      PT_CAEP = GZCAEP()
      IF (LQ(PT_CAEP) .NE. LCAEP_SAVE) THEN
        CALL ERRMSG
     &    ('CJET_MUCAEPFL SAVE OF OLD CAEP FAILED','CJET_MUCAEPFL',
     &    'Unable to save CAEP','W')
        GOTO 999
      END IF
C
C *** Copy the contents of old CAEP to new (SAV)CAEP
C
      PT_CAEP = GZCAEP()
      NCH = IQ(PT_CAEP+3)
      NR = IQ(PT_CAEP+2)
      DO IWORD = 1,3
        IQ(LQ(PT_CAEP)+IWORD) = IQ(PT_CAEP+IWORD)
      END DO
      DO IWORD = 0,NCH-1
        IQ(LQ(PT_CAEP)+4+(IWORD*NR) ) = IQ(PT_CAEP+4+(IWORD*NR) )
        Q(LQ(PT_CAEP)+5+(IWORD*NR) ) = Q(PT_CAEP+5+(IWORD*NR) )
      END DO
      OK = .TRUE.
  999 RETURN
      END
