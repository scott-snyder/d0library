      SUBROUTINE ZEBKHST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book histograms for CD electronics Examine
C-
C-   Inputs  : none
C-   Outputs : none
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
      INTEGER I, J, K
      INTEGER ICRATE, ICARD
      INTEGER CRATEID(100,2), NID, NCRATE, TASK, LKCDDN(4)
      INTEGER ID, NCARDS
      INTEGER SCRATE,SSLOT,SUL
      INTEGER IER
C
      CHARACTER*3 CFC,CSC
      CHARACTER*2 CFS,CSS
      CHARACTER*1 CSUL
      CHARACTER*40 TITLE
C
C----------------------------------------------------------------------
C
      CALL DHDIR('CD_ELECTRONICS_RCP','HBOOK_ELECT',IER,' ')
C
C FIND CDD BANKS WITH DATA
C
      DO I=1,4
        LKCDDN(I)=LQ(LHEAD-IZCDD1-I+1)
        IF(LKCDDN(I).GT.0) THEN
C
C Get crate ids for this bank
          TASK = 1
          CALL ZCRATE(I,ICRATE,ICARD,NID,CRATEID,TASK)
          NCRATE = NID
C
C Loop over available crates
          DO J = 1, NCRATE
            WRITE(CFC,'(I3)') CRATEID(J,1)
            NCARDS = CRATEID(J,2)
C
C Loop over slots in crate
            DO K = 6, 6 + NCARDS - 1
              WRITE(CFS,'(I2)') K
C
C Find shaper crate, slot, upper/lower
C              CALL FADC_TO_SHAPER(J,K,SCRATE,SSLOT,SUL)
              SCRATE = CRATEID(J,1)
              SSLOT = K
              SUL = 0
C
              WRITE(CSC,'(I3)') SCRATE
              WRITE(CSS,'(I2)') SSLOT
              IF (SUL.EQ.0) THEN
                CSUL = 'U'
              ELSE
                CSUL = 'L'
              END IF
C
              TITLE = 'HITS/CHAN - FADC C'//CFC//
     &          ' S'//CFS//', SHAPER C'//CSC//' S'//CSS//CSUL
C
              ID = CRATEID(J,1)*100 + K
              CALL HBOOK1(ID,TITLE,16,-.5,15.5,0.)
C
            END DO
          END DO
C
        ENDIF
      ENDDO
C
  999 RETURN
      END
