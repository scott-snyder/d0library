      SUBROUTINE CPPPR(IPPAT,NCMAX,NCELL,LPRBRD,LROTOW,LDEPTH,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create pulser pattern in PREAMP scheme
C-                         There is a separate call for ICD preamps
C-
C-   Inputs  : IPPAT pulser pattern number, 0 to 31
C-             NCMAX limit of size of array, should be 144 (+12 for ICD)
C-   Outputs : NCELL number of channels on lists, 0 to 144 (+12 for ICD)
C-             LPRBRD preamp board coordinate list, 0 to 95
C-             LROTOW read out tower coordinate list, 0 to 3
C-             LDEPTH depth coordinate list, 0 to 11
C-   Controls: ICOND  0 if valid, 20 if passed array is too small
C-
C-   Created  23-NOV-1988   James Kourlas  NYUHEP::KOURLAS
C-   Updated  11-MAR-1992   Joan Guida  added ICD 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPPAT, NCMAX, NCELL, ICOND
      INTEGER LPRBRD(NCMAX), LROTOW(NCMAX), LDEPTH(NCMAX)
      INTEGER IPRBRD, IROTWP, IDEPTC
      INTEGER JPRBRD, JROTWP, JDEPTC, J, IJ, JK, KL
C----------------------------------------------------------------------
C
      ICOND = 0
C
C     Compute a list of channels hit by pulse IPPAT (0-31)
C     There are 144 channels in the list. They are each specified
C     by board number (0-95), tower number (0-3), and depth (0-11).
C
C     First, test if output array is large enough:
C
      NCELL = 144
      IF ( NCMAX .LT. NCELL ) THEN
        ICOND = 20
        GOTO 999
      ENDIF
C
C     Test pulse pattern repeats every four channels within
C     a readout tower. To find the first channel (i.e. base 
C     channel or base depthc) from the pulser number:
C
      IDEPTC  =  MOD( IPPAT, 4 )
C
C     Each pulse pulses preamps within two concecutine readout 
C     towers on each preamp board and would do the same on bls
C     boards if the merge were 'turned off'. Half the pulses
C     will effect preamps in towers 0 and 1. The other pulses
C     will effect preamps in towers 2 and 3. The base readout
C     tower is found:
C
      IF (   MOD(IPPAT,8) .LT. 4  )  THEN
        IROTWP = 2
      ELSE
        IROTWP = 0
      ENDIF
C
C     The first preamp board (i.e. base preamp board) differs
C     for in each of the four cases:
C       for pulse number 0-7   the first board is 0
C       for pulse number 8-15  the first board is 4
C       for pulse number 16-23 the first board is 48
C       for pulse number 23-32 the first board is 52
C     If the merge is 'turned off' the preamp board number below
C     modulo 8 would be the BLS board number
C
      IPRBRD = (   MOD(IPPAT,16) / 8   ) * 4
      IF ( IPPAT .GE. 16 ) IPRBRD = IPRBRD + 48
C
C     Now compute the list of channels:
C
      J = 0
      DO 140 JPRBRD = 0, 23
        DO 130 JROTWP = 0, 1
          DO 120 JDEPTC = 0, 11, 4
            J = J + 1
            LPRBRD(J) = IPRBRD + JPRBRD + (JPRBRD/4) * 4
            LROTOW(J) = IROTWP + JROTWP
            LDEPTH(J) = IDEPTC + JDEPTC
  120     CONTINUE
  130   CONTINUE
        IF(IROTWP .EQ. 2) IDEPTC = MOD( IDEPTC + 1, 4)
        IF(IROTWP .NE. 2) IDEPTC = MOD( IDEPTC + 3, 4)
  140 CONTINUE
  999 RETURN
C
C
      ENTRY CPPPR_ICD(IPPAT,NCMAX,NCELL,LPRBRD,LROTOW,LDEPTH,ICOND)
C
C     This routine must be called after CPPPR
      NCELL = 156
      IF ( NCMAX .LT. NCELL ) THEN
        ICOND = 20
        GOTO 999
      ENDIF
C     Now compute the list of channels for ICD
C
      DO IJ=0,4,4
        DO JK=1,2
          IPRBRD = MOD(2*IPPAT+IJ+JK,32)
          DO KL=0,2
          J = J + 1
            LPRBRD(J) = IPRBRD
            IF (IPPAT .GE. 16) LPRBRD(J) = IPRBRD + 32
            LROTOW(J) = 1 - IJ/4
            LDEPTH(J) = 4 -3*IJ/4 + KL
          ENDDO
        ENDDO
      ENDDO
C
      RETURN
      END
