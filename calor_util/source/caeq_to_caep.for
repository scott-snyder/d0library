      SUBROUTINE CAEQ_TO_CAEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-MAR-1992   Andrew J. Milder
C-   Modified 7-FEB-1994    Richard V. Astur " For D0 MDST"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Protected Link area
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      CHARACTER*4 PATH
      INTEGER LCAEQ,GZCAEQ,GZCAEP,NCH,NR,ILYR,IETA,IPHI
      INTEGER I,POINT,WORD,IENR,JBYT,ISCALE,HL, IVERS
      REAL ENERGY
      INTEGER IEMAX             ! Energy is stored in 13 bits - assume this
C                               ! is always true!!
      data iemax / z'1ffff' /
C
      BYTE BYTES(4)
      INTEGER PACKWD
      EQUIVALENCE (PACKWD,BYTES(1))

      REAL OFFSET, E_SCALE_1, E_PREC_2
      DATA OFFSET    /.500/
      DATA E_SCALE_1 /8.91/
      DATA E_PREC_2  /.050/
C----------------------------------------------------------------------
C
C: Want to make this compatible with QCD mdst as well as new D0 mdst
C
      LCAEQ    = GZCAEQ()         ! Handles QCD or D0 mdst
      IF (LCAEQ.LE.0) THEN
        CALL ERRMSG('NO CAEQ','CAEQ_TO_CAEP','CAEQ bank not present',
     &    'W')
        GOTO 900
      ENDIF
C
      IVERS = IQ( LCAEQ + 1)        ! Version number
      HL    = IQ(LCAEQ+2)           ! Header length
      NCH   = IQ(LCAEQ-1) - HL      ! # of cells = total - #header
C
C: If version 2, packing is different
C
      IF ( IVERS .GT. 1 ) THEN
        OFFSET    = Q( LCAEQ + 4)
        E_SCALE_1 = Q( LCAEQ + 5)
        E_PREC_2  = Q( LCAEQ + 6)
      ELSE
        OFFSET    = .500
        E_SCALE_1 = 8.91
        E_PREC_2  = .05
      ENDIF

C
      LCAEP = GZCAEP()                   ! If CAEP exists, drop it
      IF ( LCAEP .GT. 0 ) CALL MZDROP( IXCOM, LCAEP, ' ')
      CALL BKCAEP(NCH,LCAEP)             ! Book CAEP of the same size
      CALL SBYT( 1, IQ(LCAEP+0), 1, 1 )  ! Set lsb to flag use of CAEQ to make this CAEP
      LCAEQ = GZCAEQ()              ! Refind CAEQ pointer after BK of CAEP
      IQ(LCAEP+3) = NCH
      NR = IQ(LCAEP+2)
C
      DO I = 1, NCH
        WORD = IQ(LCAEQ+I+HL)
C:IETA
        IETA = JBYT(WORD, 1, 7) - 37
        IF (IETA.LE.0) IETA = IETA - 1
C:IPHI
        IPHI = JBYT(WORD, 8, 6) + 1
C:ILAY
        ILYR = JBYT(WORD,14, 5)
C:SCALE
        ISCALE = JBYT(WORD,19,1)
C:IENERGY
        IENR = JBYT(WORD,20,13)
        IF (ISCALE.EQ.0) THEN
          ENERGY = FLOAT(IENR)*(E_SCALE_1+OFFSET)/FLOAT(IEMAX)
C          ENERGY = FLOAT(IENR)*.001
        ELSE
          ENERGY = E_PREC_2*IENR + E_SCALE_1 + OFFSET
C          ENERGY = FLOAT(IENR)*.05
        ENDIF
        ENERGY = ENERGY - OFFSET
C
        BYTES(BYTE1) = 0
        BYTES(BYTE2) = ILYR
        BYTES(BYTE3) = IPHI
        BYTES(BYTE4) = IETA
C
        POINT = LCAEP + (I-1)*NR + 3
        IQ(POINT+1) = PACKWD
        Q(POINT+2)  = ENERGY
      ENDDO
C
  900 CONTINUE
  999 RETURN
      END
