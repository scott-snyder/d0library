      SUBROUTINE ZEROSUP_CAEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-APR-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Protected Link area
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      CHARACTER*4 PATH
      INTEGER GZCAEP,NCH,NR,ICOUNT,IHD
      INTEGER I,POINT,WORD,IENR,JBYT,ISCALE,PACKED(50000)
      REAL ENERGY,ENERS(50000),ZERSP(3)
C
      BYTE BYTES(4)
      INTEGER PACKWD
      EQUIVALENCE (PACKWD,BYTES(1))
C
      DATA ZERSP / .050,  .050,  0.0 /    ! EM,  HAD,  ICD/MG
C
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      CALL PATHST('RECO')
C
      LCAEP = GZCAEP()
      NCH = IQ(LCAEP+3)
      NR = IQ(LCAEP+2)
      ICOUNT = 0
      DO I = 1, NCH
        PACKWD = IQ(LCAEP + (I-1)*NR + 4)
        ENERGY = Q(LCAEP + (I-1)*NR + 5)
        IF (BYTES(2).LT.8) THEN
          IHD = 1
        ELSEIF (BYTES(2).LT.11) THEN
          IHD = 3
        ELSE
          IHD = 2
        ENDIF
        IF (ENERGY.GT.ZERSP(IHD)) THEN
          ICOUNT = ICOUNT + 1
          PACKED(ICOUNT) = PACKWD
          ENERS(ICOUNT) = ENERGY
        ENDIF
      ENDDO
C
      CALL MZDROP(IXMAIN,LCAEP,' ')
      CALL BKCAEP(ICOUNT,LCAEP)
      IQ(LCAEP+3) = ICOUNT
      DO I = 1, ICOUNT
        IQ(LCAEP+ (I-1)*NR + 4) = PACKED(I)
        Q(LCAEP+ (I-1)*NR + 5) = ENERS(I)
      ENDDO
C
  999 RETURN
      END
