      LOGICAL FUNCTION FFIMUO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Muon FFREAD cards defined here
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckkhere
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated  22-JAN-1992   Susumu Igarashi  Set default value for SMUO 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER I
      LOGICAL PRT_FFIMUO
C----------------------------------------------------------------------
      FFIMUO = .TRUE.
      IF ( DMUO .LE. 0 ) GOTO 999
C
      SMUO(1) = 0. ! Geant display switch  Module:1. Plane:2. Cell:3.
      SMUO(2) = 2. ! Digitization version  Version 1:1. Version 2:2.
      SMUO(3) = 1. ! Mag. field map vers.  Uniform map:0. Map version 1:1. 
      DO I = 4, 10
        SMUO(I) = 0.
      ENDDO
C
      CALL FFKEY('SMUO',SMUO,10,'REAL')
C
      ENTRY PRT_FFIMUO
C
      PRT_FFIMUO = .TRUE.
      WRITE (LOUT,9000) DMUO,PMUO,SMUO
 9000 FORMAT(
     & ' FFIMUO ** DMUO ',I3,' PMUO ',I3,/
     & '           SMUO ',10F6.1)
C
  999 RETURN
      END
