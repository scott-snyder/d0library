      SUBROUTINE CNEIGH2(IHAD,I,IMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a CATE tower I, works out
C-                         The CATE index of the cell with Maximum
C-                         ENERGY/ET in a neighboring Grid
C-                         with SRCP_SETTABLE parameters.
C-
C-   Inputs  : I, CATE tower number
C-             IHAD EM/Had flag. 1= EM, 2= EM+ HAD
C-   Outputs : IMAX Tower with Maximum E or ET
C-   Controls:
C-
C-   Created  31-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CLUPAR.INC'
      INTEGER IER
      INTEGER I,J,IHAD,IHADR,CATW,CATI,CATJ
      INTEGER ETAI,PHII,ETAJ,PHIJ,IETA,IPHI
      INTEGER IMAX
      REAL    EMAX,ETOWI,ETOWJ
      REAL    ETAIM,ETAJM
      INTEGER DELETA,DELPHI
C
      LOGICAL DOENER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CATW(J) = (J-1)*NREP+LCATE        ! FUNCTION TO GIVE CATE OFFSET.
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZGET('ETA_NEIGHBOR_LIMIT',DELETA,IER)
        CALL EZGET('PHI_NEIGHBOR_LIMIT',DELPHI,IER)
        CALL EZGET('DO_NEIGHBOR_ENERGY',DOENER,IER)
      ENDIF
C
      CATI = CATW(I)
      ETAI = IQ(CATI+12)
      ETAIM=ETAI-SIGN(0.5,FLOAT(ETAI))         ! Solve the displaced zero
      PHII = IQ(CATI+13)
      EMAX = 0.
      IMAX = 0
      DO 100 IETA = -DELETA,DELETA
        ETAJM = ETAIM +IETA
        ETAJ=ETAJM+SIGN(0.5,ETAJM)
C
C this gives nearest neighbor across 0
C
        IF(IABS(ETAJ).GT.NETAL)GO TO 100
        DO 200 IPHI = -DELPHI,DELPHI
          IF(IETA.EQ.0.AND.IPHI.EQ.0)GO TO 200        ! SKIP OVER ITSELF
          PHIJ = PHII + IPHI
          IF(PHIJ.EQ.0)PHIJ=NPHIL   ! wrap-around
          IF(PHIJ.EQ.NPHIL+1)PHIJ=1
          J = PTCATE(ETAJ,PHIJ,IHAD)
          IF(J.EQ.0)GO TO 200
          CATJ = CATW(J)
          ETOWJ = Q(CATJ+7)
          IF(.NOT.DOENER)ETOWJ = Q(CATJ+8)      ! ET IF NOT ENERGY
          IF(ETOWJ.GT.EMAX)THEN
            EMAX = ETOWJ
            IMAX = J             ! NEIGHBOR WITH MAXIMUM ENERGY
          ENDIF
  200   CONTINUE
  100 CONTINUE
  999 RETURN
      END
