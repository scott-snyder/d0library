      SUBROUTINE VFADCMAP(CRATE, CARD, CHANNEL, LABEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate electronic VTX address into logical LABEL
C-
C-   Inputs  : CRATE (0-9), CARD (0-15), CHANNEL(0-15)
C-   Outputs : LABEL (bit-packed logical address)
C-   Controls:
C-
C-   Created  15-NOV-1990   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER CRATE, CARD, CHANNEL, LABEL
C
      INTEGER LAY, SEC, WIR, STR, END, ADDR
      INTEGER CRT, CRD, CHAN, NSEC(0:2)
      DATA NSEC / 16, 32, 32 /
      INTEGER MXCRT, MXCRD, MXCHAN, ICRT, ICRD, ICHAN
      PARAMETER ( MXCRT = 9 )
      PARAMETER ( MXCRD = 15 )
      PARAMETER ( MXCHAN = 15 )
      INTEGER FADCMAP(0:MXCRT,0:MXCRD,0:MXCHAN)
      SAVE FADCMAP
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Initialize FADCMAP array with all -1's; -1 is returned for unfilled
C ****  hardware channels (empty slots in the fadc crate)
C
        DO ICHAN = 0, MXCHAN
          DO ICRD = 0, MXCRD
            DO ICRT = 0, MXCRT
              FADCMAP(ICRT,ICRD,ICHAN) = -1
            ENDDO
          ENDDO
        ENDDO
        DO LAY = 0, 2
          DO SEC = 0, NSEC(LAY) - 1
            DO WIR = 0, 7
              DO END = 0, 1
                CALL VCODER(ADDR,0,LAY,SEC,WIR,STR,END,0,2)
                CALL VCHANMAP(ADDR,CRT,CRD,CHAN)
                FADCMAP(CRT,CRD,CHAN) = ADDR
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF
      LABEL = FADCMAP(CRATE,CARD,CHANNEL)
  999 RETURN
      END
