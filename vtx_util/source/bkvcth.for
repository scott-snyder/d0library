      SUBROUTINE BKVCTH(VCTH_LEN,N_VTXT,LVCTH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VCTH, containing infor linking tracks in VTXT
C-                         with the associated hits in VCHT.
C-
C-   Inputs  : VCTH_LEN = Number of words required for data, including the
C-                     header.
C-             N_VTXT = Number of VTXT banks
C-             (Get these two numbers by a call to VGET_VCTH_LEN; see below)
C-   Outputs : LVCTH = pointer to VCTH bank
C-
C-   Created  25-FEB-1994   Al Clark
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCTH.LINK'
C
      INTEGER VCTH_LEN, N_VTXT, LVCTH
      INTEGER LVTRH, GZVTRH, LVTXT, NTRK
      INTEGER NL, NS, ND, NIO, ISETVN
      INTEGER NHEAD, NHTSWD, MAX_VTXT
      INTEGER BANK_VERSION, NH
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA NHEAD, NHTSWD, MAX_VTXT/ 5, 4, 512 /
      DATA NL, NS / 0, 0 /
      DATA BANK_VERSION / 0 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VCTH','5I -B',NIO)
      ENDIF
C
      LVTRH = GZVTRH()
      IF ( LVTRH .GE. 0 ) THEN
        ND = VCTH_LEN
        CALL MZBOOK(IXMAIN,LVCTH,LVTRH,-IZVCTH,'VCTH',NL,NS,ND,NIO,0)
      ELSE
        CALL ERRMSG('VTRH does not exist','BKVCTH',
     &    'Cannot book VCTH - no supporting bank','F')
        LVCTH = 0
        GO TO 999
      ENDIF
C
C ****  Fill header info 
C
      IQ(LVCTH) = ISETVN(IQ(LVCTH),BANK_VERSION)
      IQ(LVCTH+1) = BANK_VERSION
      IQ(LVCTH+2) = NHEAD
      IQ(LVCTH+3) = N_VTXT
      IQ(LVCTH+4) = NHTSWD      ! Number of hits per word
      IQ(LVCTH+5) = MAX_VTXT
      GO TO 999
C
      ENTRY VGET_VCTH_LEN(VCTH_LEN,N_VTXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate length of VCTH bank and get the number
C-                          of VTXT banks
C-
C-   Inputs  : Requires existence of VTRH and VTXT banks.
C-   Outputs : VCHT_LEN = length of VCTH bank, including header
C-             N_VTXT   = Number of VTXT banks.
C----------------------------------------------------------------------
      VCTH_LEN = NHEAD
      N_VTXT   = 0
      LVTRH    = GZVTRH()
      IF ( LVTRH .LE. 0) GO TO 999    ! No tracks

C**  LOOP thru VTXT to count them and get the number of hits

      LVTXT = LQ(LVTRH-1)
      DO WHILE ( LVTXT .GT. 0 )
        NTRK = IQ(LVTXT-5)              ! VTXT track number
        IF (NTRK .LE. MAX_VTXT) THEN    ! Don't include trks over MAX_VTXT
          N_VTXT = N_VTXT + 1
          NH = IQ(LVTXT+2)              ! # of wires or hits on the track
          VCTH_LEN = VCTH_LEN + 2 + (NH-1)/NHTSWD
        ENDIF
        LVTXT = LQ(LVTXT)               ! Get next VTXT
      ENDDO   ! WHILE LVTXT .GT. 0
  999 RETURN
      END
