      SUBROUTINE JPAUSE(NDEVIC)
C
C    Purpose:
CD   This subroutine updates the screen output from any previous
CD   graphics actions, writes a prompt to the ASYNC screen, and waits
CD   for the user to press a carraige return. The parameter is a dummy
CD   in the case of the Emulator.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-Aug-1988
CH   History:
CH      16-AUG-88  ATV  Add the call to PPURGE to flush the GSR's buffers.
CH      03-AUG-88  ATV  Modify FORMAT statement.
CH      18-JUN-88  ATV  Add call to the vector update routine.
CH      16-JUN-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GENRL-R, GRFPAR-R, DEVSTS-R, SEGINF-R
C
C    Calls:
CC      KUPDV, PPURGE
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NDEVIC
C
C    Then local declarations of variables (non-common variables).
C
      CHARACTER*1 ICH
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GENRL.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
C   If device specified is not the device turned on then error.
C
      IF (NDEVIC .NE. DSPDEV) THEN
        CALL ERROR('INVALID DEVICE SPECIFIED')
      ENDIF
C
C   If device is not initialized.
C
      IF (.NOT. DINIT) THEN
        CALL ERROR('DEVICE HAS NOT BEEN INITIALIZED')
      ENDIF
C
C   If device is not on then return.
C
      IF (.NOT. DEVON) THEN
        RETURN
      ENDIF
C
C   Else perform the update sequence if necessary for the particular
C   complex operations.
C
C   The first thing to do is to determine if there are any vectors that
C   need to be displayed.
C
      CALL KUPDV
      CALL PPURGE(ERRHND)
      WRITE (6,100)
      READ (5,110,END=1,ERR=1) ICH
    1 RETURN
  100 FORMAT(1X,'PRESS RETURN TO CONTINUE')
  110 FORMAT(A1)
      END
