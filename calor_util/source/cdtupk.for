      SUBROUTINE CDTUPK(CRATE,IDATA,IETAC,IPHIC,ILYRC,PULSHT,SCALE,
     &  NEGLIM,ICOND)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Calorimeter DaTa UnPacKing routine.
C                         A shell routine which takes the raw data
C                         word from CAD1 or CAD2 and returns the
C                         pulse height and the address in the physics
C                         system, and the x1/x8 flag and the negative
C                         limit flag. The two flags will not ordinarily
C                         be of interest if hardware pedestal subtraction
C                         is on.
C                         It calls CADUPK, CADPH, and CALPH.
C                         It is imperative to check for ICOND = 0 before
C                         using the results! The arguments themselves are
C                         not reset to nonsense values each time, so only
C                         the return code tells the user if they are valid.
C
C   Inputs  : CRATE     ADC crate number         CRATE*10+BANK 
C                               where CRATE=0,5 and BANK=7,8
C                               old format: [96,101],[112,117]
C             IDATA     packed calorimeter address and pulse height
C
C   Outputs : IETAC     offline eta index        [-37,-1],[1,37]
C             IPHIC     offline phi index        [1,64]
C             ILYRC     offline radial index     [1,17]
C             PULSHT    pulse height decoded from IDATA
C             SCALE     gain flag for ADC channel: = 0 ==> X8; = 1 ==> X1
C             NEGLIM    =1 ==> negative limit set
C             ICOND     return code: 0 = OK
C                                    1 = invalid return from CADUPK
C                                    2 = invalid return from CADPH
C
C
C   Created  16-JAN-1989   K. Wyatt Merritt
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER CRATE,IADDR,IDATA,IETAC,IPHIC,ILYRC,PULSHT,ICOND
      INTEGER ICRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM
      INTEGER KCOND
C----------------------------------------------------------------------
      ICOND = 0                 ! Initialize return code OK
C
C        Get ADC address from packed IDATA
      CALL CADUPK(CRATE,IDATA,ICRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM)
C
C        Get PHysics address from ADC address
      CALL CADPH(CRATE,ADC,BLS,ROTOW,DEPTH,IETAC,IPHIC,ILYRC,KCOND)
      IF (KCOND .NE. 0) THEN
        ICOND = 2
        GO TO 999
      ENDIF
C
C        Get pulse height in the form of a 32-bit signed integer
      CALL CALPH(IDATA,PULSHT)
C
  999 RETURN
      END
