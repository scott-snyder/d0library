      SUBROUTINE ZEANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do CD Electronics Examine analysis
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-OCT-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
C
      INTEGER I, J, K, L
      INTEGER ICRATE, ICARD
      INTEGER CRATEID(100,2), NID, NCRATE, TASK, LKCDDN(4)
      INTEGER DUM(100,2)
      INTEGER LCHN(0:15)
      INTEGER NCARDS
      INTEGER NPULSE
      INTEGER IER
      INTEGER ID
C
      REAL HITS(2,10)
      REAL OFFSET(2)
C
      CHARACTER*80 STRING
C
      LOGICAL FIRST
C
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('CD_ELECTRONICS_RCP')
        CALL EZGET('OFFSET',OFFSET,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      CALL DHDIR('CD_ELECTRONICS_RCP','HBOOK_ELECT',IER,' ')
C
C FIND CDD BANKS WITH DATA
C
      DO I = 1, 4
        LKCDDN(I)=LQ(LHEAD-IZCDD1-I+1)
        IF(LKCDDN(I).GT.0) THEN
C
C Get crate ids for this bank
          TASK = 1
          CALL ZCRATE(I,ICRATE,ICARD,NID,CRATEID,TASK)
          IF (NID.LT.0) THEN
            IF (NID.EQ.-1) THEN
              WRITE(STRING,1010) I
 1010         FORMAT(' Error in CDD bank ',I1,
     &          ', channel length too short (< 4 words).')
            ELSE IF (NID.EQ.-2) THEN
              WRITE(STRING,1011) I
 1011         FORMAT(' Error in CDD bank ',I1,
     &          ', channel length too long (> 512 words).')
            END IF
            CALL INTMSG(STRING)
          ELSE
            NCRATE = NID
C
C Loop over crates
            DO J = 1, NCRATE
              NCARDS = CRATEID(J,2)
C
C Loop over cards in crate
              DO K = 0, NCARDS-1
C
                TASK = 3
                CALL ZCRATE(I,CRATEID(J,1),K,NID,DUM,TASK)
                IF (NID.LT.0) THEN
                  IF (NID.EQ.-1) THEN
                    WRITE(STRING,1010) I
                  ELSE IF (NID.EQ.-2) THEN
                    WRITE(STRING,1011) I
                  END IF
                  CALL INTMSG(STRING)
                ELSE
C
                  DO L = 0, 15
                    LCHN(L) = DUM(L+1,1)
                  END DO
C
                  ID = CRATEID(J,1)*100 + K + 6
C
C Loop over channels in card
                  DO L = 0, 15
C
C Find hits
                    CALL ZEPULS(LCHN(L),OFFSET,NPULSE,HITS)
C
                    CALL HF1(ID,FLOAT(L),FLOAT(NPULSE))
C
                  END DO
                END IF
              END DO
            END DO
          END IF
        ENDIF
      ENDDO
C
  999 RETURN
      END
