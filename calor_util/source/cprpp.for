      SUBROUTINE CPRPP(IPRBRD,IROTOW,IDEPTH,IPPAT,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find pulser pattern number for a given preamp
C-
C-   Inputs  : IPRBRD the preamp mother board
C-             IROTOW the preamp's read out tower
C-             IDEPTH the preamp's depth
C-   Outputs : IPPAT the pulser pattern number, 0 to 31
C-   Controls: ICOND, 0 = valid
C-
C-   Created  28-NOV-1988   James Kourlas  NYUHEP::KOURLAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IPRBRD, IROTOW, IDEPTH, IPPAT, ICOND, IB
C----------------------------------------------------------------------
      ICOND = 0
C     compute pulse number from preamp board, tower and depth
C     pattern repeats every 8 boards
      IB = MOD(IPRBRD,8)
      IF( IROTOW .LE. 1) IPPAT = MOD( IDEPTH + IB, 4) + 4
      IF( IROTOW .GE. 2) IPPAT = MOD( IDEPTH - IB + 8, 4) 
      IF( IB .GE. 4 )    IPPAT = IPPAT + 8
C     Pattern numbers 16 to 31 are for preamp boards 48 to 95
C     The 5th bit (i.e. 16hex) is called the HI bit by the pulser designer
      IF( IPRBRD .GE. 48 ) IPPAT = IPPAT + 16
  999 RETURN
C
      ENTRY CPRPP_ICD(IPRBRD,IROTOW,IDEPTH,IPPAT,ICOND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find pulser pattern number for a given preamp
C-                         for the ICD
C-
C-   Inputs  : IPRBRD the preamp mother board
C-             IROTOW the preamp's read out tower
C-             IDEPTH the preamp's depth
C-   Outputs : IPPAT the pulser pattern number, 0 to 31
C-   Controls: ICOND, 0 = valid
C-
C-   Created  14-FEB-1992   Joan Guida   modified CPRPP to add ICD
C-
C----------------------------------------------------------------------
      ICOND = 0
C     compute pulse number from preamp board, tower and depth
      IPPAT=MOD((IPRBRD/2-1 + MOD(IPRBRD,2) + (1-IROTOW)*14),16)
      IF (IPPAT.LT.0) IPPAT=IPPAT+16
C     Pattern numbers 16 to 31 are for preamp boards 32 to 63
C     The 5th bit (i.e. 16hex) is called the HI bit by the pulser designer
      IF( IPRBRD .GE. 32 ) IPPAT = IPPAT + 16
      RETURN
C
      END
