      SUBROUTINE CADZS(NCAD,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Zero Suppress CAD banks. Used in D0GEANT
C-                         routine MKCAD. CAD bank channels with exactly
C-                         zero (=0) ADC counts are deleted from CAD banks 
C-
C-   Inputs  : NCAD - 1 FOR CAD1, 2 FOR CAD2
C-   Outputs : OK   - .TRUE. if  success.
C-   Controls: none
C-
C-   Created  17-APR-1989   Chip Stewart
C-   Updated  30-OCT-1992   Chip Stewart  - Added 16 trailer words=0 
C-   Updated  27-NOV-1992   James T. Linnemann  correct trailer logic 
C-                          Chip Stewart   - Use stand-alone CADz bank
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
C
      INTEGER LCAD,POINTIN,SYNC,CONTRL,NCAD,IDATA,LDAT,JBYT
      INTEGER CSYNC,CRATE,NCRDS,SYNFLG,NCRATE
      INTEGER LZS,POINTOU,WCNT,PWCNT,I,J,K,NZS
      INTEGER NW_BANK
      INTEGER NKEEP,NTOSS,NKEEP_PREV
      INTEGER NW_CRATE,NW_CARD,NW_CAD_TRAIL
      INTEGER NW_CRATE_HEAD,NW_CARD_HEAD,HEADER_LEN,NW_CRATE_TRAIL
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK/LIST'
      INCLUDE 'D0$LINKS:IZCAD2.LINK/LIST'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS/LIST'
      INCLUDE 'D0$INC:ZLINKA.INC'
C----------------------------------------------------------------------
      PARAMETER (NW_CRATE_HEAD=5)           ! number of words in crate header
      PARAMETER (NW_CARD_HEAD=1)            ! number of words in card header
      PARAMETER (NW_CRATE_TRAIL=4)          ! number of words in crate trailer
      PARAMETER (SYNFLG=2**16-1)            ! synch word least sig 16 bits
      PARAMETER (NW_CAD_TRAIL=16)           ! number of words in CAD trailer
C
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL GSLINK('CADZS',NZS )
        LSLINK(NZS) = 0
      ENDIF
      OK = .TRUE.
      NKEEP=0                          ! NUMBER WORDS KEEP
      NKEEP_PREV=0
      NTOSS=0                          ! NUMBER WORDS TOSS
      NW_CRATE  =  (NDEPTC*NEFC*NBLSC+NW_CARD_HEAD)*NADCC +
     &      NW_CRATE_HEAD + NW_CRATE_TRAIL
      NW_BANK =  NW_CRATE*NADCRC+NW_CAD_TRAIL
C
C ****  BOOK TEMPORARY BANK FOR SUPPRESSED DATA
C
      CALL MZBOOK(IXMAIN,LZS,0,2,'CADZ',6,6,NW_BANK,2,0)
      IF(LZS.EQ.0) THEN
        CALL ERRMSG('NO_CAD_BOOKED','CADZS','SKIP THIS CAD BANK','W')
        GO TO 999
      END IF
      LSLINK(NZS) = LZS
C
C ****  FIND CAD BANK
C
      IF(NCAD.EQ.1) THEN
        LCAD=LQ(LHEAD-IZCAD1)
      ELSE
        LCAD=LQ(LHEAD-IZCAD2)
      ENDIF
      IF(LCAD.EQ.0) GO TO 998
C
C **** INITIALIZE POINTERS TO BOTH TEMPORARY AND FULL CAD BANK
C
      POINTIN = LCAD + 1
      POINTOU = LZS + 1
C
      NCRATE = 0
C
C ****  CRATE LOOP
C
   20 SYNC = IQ(POINTIN + 1)
      CSYNC = JBYT(SYNC,1,16)
C
C ****  CHECK SYNCH WORD FOR VALID DATA
C
      IF (CSYNC .NE. SYNFLG) THEN
        OK = .FALSE.
        CALL ERRMSG('MKCAD','CADZS',
     &    'BAD SYNCH WORD - NO ZERO SUPPRESS','W')
        GOTO 998
      ENDIF
      HEADER_LEN  = IQ(POINTIN)             ! header length
      IQ(POINTOU) = IQ(POINTIN)             ! header length
      IQ(POINTOU + 1) = IQ(POINTIN + 1)     !SYNCH WORD
      CONTRL = IQ(POINTIN + 2)              !CONTROL WORD
      CALL SBIT(1,CONTRL,3)                 !zero suppress bit in control word
      IQ(POINTOU + 2) = CONTRL              !CONTROL WORD
      IQ(POINTOU + 3) = IQ(POINTIN + 3)     !VERSION NUMBer
      IQ(POINTOU + 4) = IQ(POINTIN + 4)     !VERT = IQ(POINTIN + 3)
      NCRATE = NCRATE + 1
      CRATE = JBYT(CONTRL,25,8) / 10
      NCRDS = JBYT(CONTRL,17,8)
C
C ****  UPDATE POINTERS
C
      POINTIN = POINTIN + NW_CRATE_HEAD
      POINTOU = POINTOU + NW_CRATE_HEAD
      NKEEP = NKEEP + NW_CRATE_HEAD
C
C ****   Loop over ADC cards
C
      DO 50 I = 0 , NCRDS
        WCNT = IQ(POINTIN)
        PWCNT = POINTOU                 !WORD COUNT POINTER
        IQ(PWCNT) = WCNT
        DO 40 J = 1 , WCNT
          POINTIN = POINTIN + 1
          IDATA = IQ(POINTIN)
          LDAT = JBYT(IDATA,1,16)
C
C ****  CHECK FOR ZEROES
C
          IF(LDAT.EQ.0) THEN
            IQ(PWCNT) =  IQ(PWCNT) - 1
            NTOSS = NTOSS + 1
          ELSE
            POINTOU = POINTOU + 1
            IQ(POINTOU) =  IDATA
            NKEEP = NKEEP + 1
          END IF
   40   CONTINUE
C
C ****  NEXT WORD COUNT POINTER
C
        POINTIN = POINTIN + 1
        POINTOU = POINTOU + 1
        NKEEP = NKEEP + 1
   50 CONTINUE
C
C ****  CRATE TRAILER
C
      IQ(POINTOU)=NKEEP-NKEEP_PREV+NW_CRATE_TRAIL !Total word count in trailer
      IQ(POINTOU + 1) = IQ(POINTIN + 1)           ! event number/crate number
      IQ(POINTOU + 2) = IQ(POINTIN + 2)           ! Token value 
      IQ(POINTOU + 3) = IQ(POINTIN + 3)           ! Checksum
      POINTIN = POINTIN + NW_CRATE_TRAIL
      POINTOU = POINTOU + NW_CRATE_TRAIL
      NKEEP = NKEEP + NW_CRATE_TRAIL
      NKEEP_PREV = NKEEP
C
C ****  CHECK FOR LAST CRATE -
C
      IF (NCRATE.GE. NADCRC ) GOTO 500
      IF (IQ(POINTIN).NE. HEADER_LEN ) GO TO 500
      GO TO 20
  500 CONTINUE
C
C ****  TOTAL WORD COUNT
C
      CALL VZERO(IQ(LZS+NKEEP+1),NW_CAD_TRAIL)  ! ZERO CAD TRAILER WORDS
      IQ(POINTOU) = NKEEP           ! put data count into 1st word
      NKEEP = NKEEP + NW_CAD_TRAIL
C
C ****  COPY SUPPRESSED BANK INTO CAD BANK AND SQUEEZE IT
C
      NW_BANK = IQ(LCAD-1)                
      NTOSS = NW_BANK - NKEEP 
      CALL UCOPY(IQ(LZS+1),IQ(LCAD+1),NKEEP )
      CALL MZPUSH(IXMAIN,LCAD,0,-NTOSS,'R')
  998 CONTINUE
      CALL MZDROP(IXMAIN,LZS,'L')
  999 RETURN
      END
