      SUBROUTINE TB90_READ_TABLE(ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in the TB90 calorimeter ordered
C-                         Binary versions of cal. address file:
C-                         D0$STP$TB90_DATA:TB90L1_PHYSICS_ADRRESS.DAT
C-                         Puts table into TB90_PHYS_ADDR common.
C-
C-   Inputs  : none
C-   Outputs : ISTAT - error code returned
C-   Controls: none
C-
C-   Created  7-MAR-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB90_PHYS_ADDR.INC'
      CHARACTER*48, MSG_STRING
      INTEGER ISTAT,UNIT
C----------------------------------------------------------------------
      ISTAT=0
C
C ****  FILL TB90_PHYS_ADDR ARRAY
C
      CALL GTUNIT(6585,UNIT,ISTAT)
      IF (ISTAT.NE.0)THEN
        WRITE(MSG_STRING,200)
        CALL INTMSG(MSG_STRING)
        GO TO 999
      ENDIF
C&IF IBMAIX
C&      OPEN(     UNIT=UNIT,
C&     &          FILE='TB90L1_PHYSICS_ADDRESS_DAT',
C&     &          FORM='UNFORMATTED', STATUS='OLD',ERR=999,
C&     &          IOSTAT=ISTAT)
C&ELSE
      OPEN(     UNIT=UNIT,
     &          FILE='TB90L1_PHYSICS_ADDRESS_DAT',
     &          FORM='UNFORMATTED', STATUS='OLD',ERR=999,
     &          IOSTAT=ISTAT,READONLY,SHARED)
C&ENDIF
      READ (UNIT,ERR=999,IOSTAT=ISTAT) TB90_PHYS_ADDR
      CALL RLUNIT(6585,UNIT,ISTAT)
      CLOSE(UNIT)
      ISTAT = 0
  200 FORMAT(' ERROR GETTING FREE UNIT, RETURN')
  999 RETURN
      END
