      SUBROUTINE CAEQFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill CAEQ bank using CAEP
C-
C-   Inputs  : CAEP bank
C-   Outputs :
C-   Controls:
C-
C-   Created  24-MAR-1992   Andrew J. Milder
C-   Modified 7-FEB-1994    Richard Astur "For d0 mdst"
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER   GZCAEP, GZCAEQ
      EXTERNAL  GZCAEP, GZCAEQ
      INTEGER I,J,K,IER,LCAEP,LCAEQ,ILYR,IETA,IPHI,IVER, INFO
      INTEGER NCH,NR,NCELL,POINT,ISTAT,IHD,IENR,PACKCELL,ISCALE
      REAL SMALL,THETA, OFFSET, ENERGY
      PARAMETER ( OFFSET = 10.0 )      ! See negative cells up to -8.0 GeV
      REAL ENERGY_THRESH
      PARAMETER( ENERGY_THRESH = 8.0) ! Range 1 : -OFFSET:ENERGY_THRESH
      INTEGER IEN_1_MAX
      PARAMETER( IEN_1_MAX = '1FFF'X ) ! # in which to map range
      REAL E_PREC_2
      PARAMETER( E_PREC_2 = .05 )
      INTEGER NCAEQ_H, PACKWD, PACKWDB
      BYTE BYTE_A(4), BYTE_B(4)
      EQUIVALENCE (PACKWD,BYTE_A(1))
      EQUIVALENCE (PACKWDB, BYTE_B(1) )
      DATA SMALL /  0.0000001  /
C----------------------------------------------------------------------
C
C: Make sure there is a CAEP to make CAEQ with
C
      LCAEP = GZCAEP()
      IF ( LCAEP .LE. 0 ) THEN
        CALL ERRMSG('No CAEP','CAEQFL',
     &    'No CAEP to make CAEQ with - abort', 'W' )
        GOTO 999
      ENDIF

C
C: Drop old CAEQ
C
      LCAEQ = GZCAEQ()
      IF ( LCAEQ .GT. 0 ) CALL MZDROP(IXCOM, LCAEQ, ' ')

C
C: Get the # of channels and repetition number of CAEP
C: Also, get the low byte status word
C
      PACKWDB  = IQ( LCAEP + 4 )    ! BYTE_B( BYTE1 ) has the status word
      NCH      = IQ(LCAEP+3)
      NR       = IQ(LCAEP+2)
C
C: Book a CAEQ of the same size
C
      CALL BKCAEQ(LCAEQ,NCH)
C
C: Store status word
C
      PACKWD         = IQ( LCAEQ + 0 )
      BYTE_A( BYTE1 )= BYTE_B( BYTE1 )    ! Fill status word
      IQ(LCAEQ + 0 ) = PACKWD
C
      Q( LCAEQ + 4 ) = OFFSET
      Q( LCAEQ + 5 ) = ENERGY_THRESH
      Q( LCAEQ + 6 ) = E_PREC_2
      NCAEQ_H        = IQ( LCAEQ + 2)
C

C
C  Loop over CAEP channels, compress and save those over threshold
C
      LCAEP = GZCAEP()                ! Refind CAEP after book of CAEQ
      DO I = 1, NCH
        POINT = LCAEP + (I-1)*NR + 3
        PACKWD = IQ(POINT+1)
        ENERGY = Q(POINT+2)+OFFSET    ! Add OFFSET TO GET NEGATIVE CELLS
        INFO = BYTE_A(BYTE1)
        ILYR = BYTE_A(BYTE2)
        IPHI = BYTE_A(BYTE3)
        IETA = BYTE_A(BYTE4)
C
        IF (ENERGY.GT. ENERGY_THRESH + OFFSET) THEN
          ISCALE = 1
          IENR = NINT((ENERGY-ENERGY_THRESH-OFFSET)/E_PREC_2)
        ELSE
          ISCALE = 0
          IENR = NINT(IEN_1_MAX*ENERGY/(ENERGY_THRESH+OFFSET))
        ENDIF
C
C: Check and issue message on over/under flows
C
        IF (IENR.GT.8191) THEN
          CALL ERRMSG('CAEQ- ENERGY OVERFLOW','CAEQFL',
     &          'ENERGY OVERFLOW IN BIT-PACKING ','W')
          IENR = 8191
        ELSEIF ( IENR .LT. 0) THEN
          CALL ERRMSG('CAEQ- ENERGY UNDERFLOW','CAEQFL',
     &      'ENERGY IN CELL BELOW -10 GEV','W')
          IENR = 0
        ENDIF
C
C: Even if cells under/overflow, we keep them at the saturated value
C

        IF (IETA .LT. 0) THEN
          IETA = IETA + 37 + 1
        ELSE
          IETA = IETA + 37
        ENDIF
        IPHI = IPHI - 1
        CALL SBYT(IETA, PACKCELL,  1, 7)
        CALL SBYT(IPHI, PACKCELL,  8, 6)
        CALL SBYT(ILYR, PACKCELL, 14, 5)
        CALL SBYT(ISCALE, PACKCELL, 19, 1)
        CALL SBYT(IENR, PACKCELL, 20,13)

        IQ( LCAEQ + NCAEQ_H + I ) = PACKCELL
      ENDDO


  999 RETURN
      END
