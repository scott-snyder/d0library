      SUBROUTINE MTC_EXYZEM3(IETA,IPHI, X,Y,Z,IERR)
C----------------------------------------------------------------------
C- MTC_EXYZEM3: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : find the energy weighted average
C-      x,y,z coordinates at the input IETA,IPHI in EM3
C-
C-   Inputs  : ieta,iphi
C-   Outputs : x,y,z
C-             ierr =  0 if OK
C-                  = -1 if no energy was found in these EM3 cells
C-
C-   Created  18-OCT-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input and output
      INTEGER IETA,IPHI, IERR
      REAL X,Y,Z
C- local
      INTEGER NLYR,ILYR, NTEMP,ILTEMP(4), OK
      REAL    XTEMP,YTEMP,ZTEMP, ELTEMP(4), ESUM
C- output of GTCAEP_ADDR
      REAL    energy
      INTEGER ier_caep
ccC- output of GTCAEH_ADDR
cc      REAL EX,EY,EZ,ENERGY,ET_CAEH,SEX,SEY,CWEIGHT
cc      INTEGER STATUS,IER_CAEH
C----------------------------------------------------------------------
      NTEMP = 0
      DO 24 ILYR=3,6

cc        CALL GTCAEH_ADDR(IETA,IPHI,ILYR,EX,EY,EZ,ENERGY,ET_CAEH,
cc     &  SEX,SEY,CWEIGHT,STATUS,IER_CAEH)

        CALL GTCAEP_ADDR(IETA,IPHI,ILYR,ENERGY,IER_CAEP)
        IF(IER_CAEP.EQ.0) THEN
          NTEMP = NTEMP + 1
          ILTEMP(NTEMP) = ILYR
          ELTEMP(NTEMP) = ENERGY
        END IF
   24 CONTINUE
      ESUM  = 0.
      XTEMP = 0.
      YTEMP = 0.
      ZTEMP = 0.
      DO 25 NLYR=1,NTEMP
        ILYR = ILTEMP(NLYR)
        CALL CELXYZ(IETA, IPHI, ILYR, x, y, z, ok)
        XTEMP = XTEMP + X*ELTEMP(NLYR)
        YTEMP = YTEMP + Y*ELTEMP(NLYR)
        ZTEMP = ZTEMP + Z*ELTEMP(NLYR)
        ESUM = ESUM + ELTEMP(NLYR)
   25 CONTINUE
      IF(NTEMP.NE.0 .AND. ESUM.NE.0.) THEN
        X = XTEMP / (ESUM)
        Y = YTEMP / (ESUM)
        Z = ZTEMP / (ESUM)
        IERR = 0
      ELSE
        X = 0.
        Y = 0.
        Z = 0.
        IERR = -1
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
