      SUBROUTINE JDEND (DEV)
C
C    Purpose:
CD   This routine de-initializes the PS390 display device.
CD   JDEND performs any finish up operating system functions
CD   required to drive the PS390.
CD   
CD      DEV -- integer value dummy presently
CD
C    Authors: R.Heckelsberg, A.Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 27-May-1988
CH   History:
CH        27-MAY-88  ATV   Add the EXTERNAL statement for the error 
CH                         handler.
CH        18-MAY-88  ATV   Initial entry
C
      IMPLICIT NONE
C
C    Common blocks:
CB        GRFPAR-W, DEVSTS-R, SEGINF-R
C
C    Calls:
CC        ERROR, PDTACH
      EXTERNAL ERRHND
C    
C    Parameters: 
C
      INTEGER DEV

      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'

      IF (DINIT) THEN
         IF (SEGOPN) THEN
            CALL ERROR('A SEGMENT IS OPEN')
         ELSE 
            DINIT = .FALSE.
            CALL PDTACH(ERRHND)
         ENDIF
      ELSE
         CALL ERROR ('DSPDEV IS NOT INITIALIZE')
      ENDIF
      RETURN
      END
