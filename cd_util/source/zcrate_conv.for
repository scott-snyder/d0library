      SUBROUTINE ZCRATE_CONV(CRATE,CONFIG,NMOD,CHAN,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For the input crate number and pulser configuration
C-                         return the channels being pulsed in this crate.
C-
C-   Inputs  : CRATE  = FADC crate number
C-             CONFIG = Pulser pattern
C-   Outputs : NMOD   = Total number of channels being pulsed
C-             CHAN   = List of electronic channel numbers being pulsed.
C-   Controls: IER    = Error if something goes wrong
C-
C-   Created  19-OCT-1990   Jeffrey Bantly
C-   Updated  25-JAN-1991   modified for CDC (Jim Cochran)
C-   Updated  11-JUN-1992   Srini Rajagopalan, Modified to accomodate 
C-                          changes in Calib.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C - inputs
C
      INTEGER CRATE,CONFIG
C
C - output
C
      INTEGER NMOD,CHAN(0:255)
C
C - local variables
C
      INTEGER DET_TYPE,MAXCRTID(3:6)
      DATA MAXCRTID /93,54,115,76/
C
      INTEGER FIRSTCD,LASTCD
      DATA LASTCD /15/
C
      INTEGER ICARD,ICHN,IER
C
      INTEGER AMPLTDA,AMPLTDB,POLARITY,ODD_EVEN,HALF,PREAMP,SHCARD
C
C  SHHALF describes which FADC crates are connected which Half
C  SHHALF = 0 (unconnected);      = 1 (Only Lower Half); 
C         = 2 (Only Upper Half);  = 3 (Mixture of Upper/Lower Half).
C
      INTEGER SHHALF(0:11,3:6)                  ! 
      DATA SHHALF /2,1,2,1,2,1,2,1,2,1,0,0,     ! VTX
     &             2,3,3,1,3,3,0,0,0,0,0,0,     ! CDC
     &             1,1,1,1,1,1,2,2,2,2,2,2,     ! FDC
     &             1,1,1,1,2,2,2,2,0,0,0,0/     ! TRD
C
C----------------------------------------------------------------------
C
      IER = 0                       ! Reset error upon entry.
C
C  Zero out output.
C
      NMOD = 0
      CALL VZERO(CHAN(0),256)
C
C  Check if inputs are valid
C
      DET_TYPE = CRATE - 10*(CRATE/10)
      IF (DET_TYPE .LT. 3 .OR. DET_TYPE .GT. 6) THEN
        IER = -1
        GO TO 999
      ENDIF
C
      IF (CRATE.LT.DET_TYPE .OR. CRATE.GT.MAXCRTID(DET_TYPE)) THEN
        IER = -2
        GO TO 999
      ENDIF
C
C Determine First crate number, Last Card Number is always 15.
C
      IF (DET_TYPE.EQ.3) THEN
        FIRSTCD = 8
        IF (CRATE.EQ.83) FIRSTCD = 6
      ELSE IF (DET_TYPE.EQ.4) THEN
        FIRSTCD = 0
      ELSE IF (DET_TYPE.EQ.5) THEN
        FIRSTCD = 7
        IF (CRATE.LE.15.OR.CRATE.EQ.65.OR.CRATE.EQ.75) FIRSTCD = 0
      ELSE IF (DET_TYPE.EQ.6) THEN
        FIRSTCD = 4   
      ENDIF
C
C  Decode Pulser information
C
      CALL ZDEC_PULSER(CONFIG,AMPLTDA,AMPLTDB,POLARITY,ODD_EVEN,HALF,
     &                 PREAMP,SHCARD,1)
C
      IF (SHCARD.EQ.24) THEN        ! Means all channels in a given shaper 
C                                   ! crate are being pulsed.
        IF (HALF .EQ. 0) THEN       ! Upper half being pulsed.
C
C  Check if all channels in this crate are connected to lower
C  half,  If so return.
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.1) GO TO 999
C
C Check if all channels in this crate are connected to upper half
C If so, return with all channels.
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.2) THEN
            DO ICARD = FIRSTCD,LASTCD
              DO ICHN = 0,15
                NMOD = NMOD + 1
                CHAN(NMOD-1) = ICARD*16 + ICHN
              ENDDO
            ENDDO
            RETURN
          ENDIF
C
C  Check if only some channels in this crate are connected to upper half.
C  This is true ony for crates 14,24,44 and 54 (CDC). 
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.3) THEN
            IF (DET_TYPE.EQ.4) THEN
              DO ICHN = 128,255
                NMOD = NMOD + 1
                CHAN(NMOD-1) = ICHN
              ENDDO
            ELSE
              GO TO 999
            ENDIF
          ENDIF
C
        ELSE IF (HALF.EQ.1) THEN          ! Lower half being pulsed
C
C  Repeat the above algorithm for lower half.
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.2) GO TO 999
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.1) THEN
            DO ICARD = FIRSTCD,LASTCD
              DO ICHN = 0,15
                NMOD = NMOD + 1
                CHAN(NMOD-1) = ICARD*16 + ICHN
              ENDDO
            ENDDO
            RETURN
          ENDIF
C
          IF (SHHALF(CRATE/10,DET_TYPE).EQ.3) THEN
            IF (DET_TYPE.EQ.4) THEN
              DO ICHN = 0,127
                NMOD = NMOD + 1
                CHAN(NMOD-1) = ICHN
              ENDDO
            ELSE
              GO TO 999
            ENDIF
          ENDIF
C
        ELSE
          IER = -3
          GO TO 999             ! Wrong Half number
        ENDIF
C
      ELSE
        IER = -4
        GO TO 999               ! No provision yet for pulsing subset of
C                               ! channels in a shaper crate. It should
C                               ! otherwise go in here
      ENDIF

          
  999 RETURN
      END
