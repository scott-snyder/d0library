      FUNCTION CLSMEN(E)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Smearing of the TRD clusters energies
C-                         Smear with a Gaussian or a Poissonian law
C-                         according to the number of detected electrons
C-                         determined from the dS/S measured during
C-                         calibration.
C-   Inputs  : E       = ENERGY OF THE CLUSTER
C            : DELOE   =dE/E
C-   Outputs : CLSMEN  =SMEARD ENERGY
C-
C-   Created  11-DEC-1987   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:NORTRD.INC'
      INCLUDE 'D0$INC:PRNEVT.INC'
      REAL CLSMEN,DE,E,NSEC
      INTEGER IERR,NEFF
C----------------------------------------------------------------------
      NSEC=E/DELOE2
      IF (NSEC.LT.20.) THEN
        CALL POISSN(NSEC,NEFF,IERR)
        CLSMEN=NEFF*DELOE2
      ELSE
        CALL NORRAN(DE)
        CLSMEN=E*(1.+DE/SQRT(NSEC))
      ENDIF
C      IF(IPRNT.EQ.1)THEN
C       PRINT*,' E',E,' NSEC ',NSEC,' E SMEAREE ',CLSMEN,' IERR',IERR
C      END IF
  999 RETURN
      END
