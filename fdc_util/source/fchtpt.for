      SUBROUTINE FCHTPT(HALF,UNIT,QUAD,SECTOR,WIRE,FIRSTHIT,NHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given channel,
C-     return the pointer to the first hit and number of hits 
C-     in the FCHT bank (or CDH3 bank if FCHT doesn't exist).
C-
C-   Inputs  :  HALF,UNIT,QUAD,SECTOR,WIRE  Specifies channel.
C-   Outputs :  FIRSTHIT = pointer to hit in FCHT (relative to LFCHT).
C-              NHITS    = number of hits in that channel.
C-   Controls: 
C-
C-   Updated   3-NOV-1993   Robert E. Avery   
C-   Updated  13-JUL-1994   Robert E. Avery   Fix pointer off by 1.
C-   Updated  25-MAR-1995   Yi-Cheng Liu  Get rid of redundant variable
C-                          GZCDH3, and define RUNSAV,IDSAV in before use.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER FIRSTHIT, NHITS 
C
      INTEGER LFCHT, GZFCHT
      INTEGER RUN,ID,RUNSAV,IDSAV
      INTEGER LENGTH_FCHT
      INTEGER CRATE_LENGTH
      INTEGER NHITS_CRATE, HIT, HIT_PTR, POINT
      INTEGER CHANNEL_ID, UBIT 
      INTEGER HA,UN,QU,SE,WI
      INTEGER HEADER_LENGTH, TRAILER_LENGTH
      PARAMETER (HEADER_LENGTH = 4)
      PARAMETER (TRAILER_LENGTH = 2)
      INTEGER MAXSECT(0:1),MAXWIRE(0:1)
C
      INTEGER MXCHANT
      PARAMETER( MXCHANT = MXWIRT+2  )
      INTEGER TH_MAP(2,0:MXCHANT,0:MXSECT,0:MXQUAD,0:1)
      INTEGER PH_MAP(2,0:MXWIRP ,0:MXSECP ,0:1)
      INTEGER TH_MAP_SIZE
      INTEGER PH_MAP_SIZE
      PARAMETER( TH_MAP_SIZE = 2*(MXCHANT+1)*(MXSECT+1)*(MXQUAD+1)*2 )
      PARAMETER( PH_MAP_SIZE = 2*(MXWIRP+1)*(MXSECP+1)*2 )
C    
      SAVE MAXSECT, MAXWIRE
      DATA MAXSECT / MXSECT, MXSECP /
      DATA MAXWIRE / MXCHANT, MXWIRP /
      DATA RUNSAV,IDSAV/-1,-1/
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
        RUNSAV=RUN
        IDSAV=ID
C
C  Once per event, fill pointer maps:
C
        LFCHT = GZFCHT()
        IF ( LFCHT.LE.0 ) THEN
          GOTO 999
        ENDIF
C
        CALL VZERO(TH_MAP,TH_MAP_SIZE)
        CALL VZERO(PH_MAP,PH_MAP_SIZE)
C
        LENGTH_FCHT = IQ(LFCHT -1)  
        POINT = LFCHT + LENGTH_FCHT                           
        IF ( IQ(LFCHT-5).eq.3 ) POINT = POINT -1  ! CDH3 bank
        DO WHILE (POINT .GT. (LFCHT + HEADER_LENGTH))
C
          CRATE_LENGTH = IQ(POINT - TRAILER_LENGTH + 1)
          POINT = POINT - CRATE_LENGTH
          NHITS_CRATE = (CRATE_LENGTH-HEADER_LENGTH-TRAILER_LENGTH)/2
c
          HIT_PTR = POINT + HEADER_LENGTH + 1
          DO HIT = 1, NHITS_CRATE 
            CHANNEL_ID = IBITS(IQ(HIT_PTR), 0, 16)
            CALL FCODER(CHANNEL_ID,HA,UN,QU,SE,WI,UBIT,1)
            IF (   (UBIT .NE. 0) 
     &        .OR. (SE .GT. MAXSECT(UN))
     &        .OR. (WI .GT. MAXWIRE(UN)) )  THEN
              UN = 2
              CALL ERRMSG('FCHTPT','FCHTPT',
     &                   'Invalid channel ID in FCHTPT', 'W')
            ENDIF
C 
C If this is the first hit on this channel, store pointer
C
            IF ( UN.EQ.0 ) THEN
              IF (TH_MAP(2,WI,SE,QU,HA) .EQ. 0) THEN
                TH_MAP(1,WI,SE,QU,HA) = HIT_PTR-LFCHT 
              ENDIF
              TH_MAP(2,WI,SE,QU,HA) = TH_MAP(2,WI,SE,QU,HA) + 1
            ELSEIF ( UN.EQ.1 ) THEN
              IF (PH_MAP(2,WI,SE,HA) .EQ. 0) THEN
                PH_MAP(1,WI,SE,HA) = HIT_PTR-LFCHT 
              ENDIF
              PH_MAP(2,WI,SE,HA) = PH_MAP(2,WI,SE,HA) + 1
            ENDIF
            HIT_PTR = HIT_PTR + 2
          ENDDO
        ENDDO
      ENDIF
C
C Return results for this channel
C
      IF ( UNIT.EQ.0 ) THEN
        FIRSTHIT = TH_MAP(1,WIRE,SECTOR,QUAD,HALF) 
        NHITS    = TH_MAP(2,WIRE,SECTOR,QUAD,HALF) 
      ELSE
        FIRSTHIT = PH_MAP(1,WIRE,SECTOR,HALF) 
        NHITS    = PH_MAP(2,WIRE,SECTOR,HALF) 
      ENDIF
  999 RETURN
      END
