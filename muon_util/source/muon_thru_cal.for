      SUBROUTINE MUON_THRU_CAL(IMUTHRU)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Divide Muon Chambers according to Top East
C-                         Top West, Bot East, and Bot West  in order
C-                         to decide if a muon went through Calorimeter.
C-                         
C-   Returned value  : Returns 1 if true, 0 if false.
C-   Inputs  : None
C-   Outputs : Answer 0 or 1
C-   Controls: None
C-
C-   Created  21-APR-1991   H T DIEHL
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER IMUTHRU

      INTEGER NMURAW,NMUPROC,NMODH,LPMUOF(310)
      INTEGER I,J,NMOD,NRAW,IMUD1,NPROC,IMUON,NPLH0,NPLH1,NPLH2,NPLH3

      INTEGER ITOPEAST,ITOPWEST,IBOTEAST,IBOTWEST
      INTEGER ITOPSIDESEAST,ITOPSIDESWEST
      INTEGER IBOTSIDESEAST,IBOTSIDESWEST
      INTEGER ISIDESAEAST,ISIDESAWEST

      INTEGER TOP_EAST(9),TOP_WEST(9),BOT_EAST(11),BOT_WEST(11)
      INTEGER TOP_SIDES_EAST(6),TOP_SIDES_WEST(6)
      INTEGER BOT_SIDES_EAST(6),BOT_SIDES_WEST(6)
      INTEGER SIDES_A_EAST(3),SIDES_A_WEST(3)

      DATA TOP_EAST/211,221,231,111,121,131,011,021,031/
      DATA TOP_WEST/212,222,232,112,122,132,012,022,032/
      DATA BOT_EAST/246,236,216,206,146,136,116,106,036,026,016/
      DATA BOT_WEST/245,235,215,205,145,135,115,105,035,025,015/
      DATA TOP_SIDES_EAST/230,220,210,130,120,110/
      DATA TOP_SIDES_WEST/233,223,213,133,123,113/
      DATA BOT_SIDES_EAST/237,227,217,137,127,117/
      DATA BOT_SIDES_WEST/234,224,214,134,124,114/
      DATA SIDES_A_WEST/033,023,013/
      DATA SIDES_A_EAST/030,020,010/

C----------------------------------------------------------------------
      ITOPEAST = 0
      ITOPWEST = 0
      IBOTEAST = 0
      IBOTWEST = 0
      ITOPSIDESEAST = 0
      ITOPSIDESWEST = 0
      IBOTSIDESWEST = 0
      IBOTSIDESEAST = 0
      ISIDESAEAST = 0
      ISIDESAWEST = 0

      CALL GTMUHT(NMURAW,NMUPROC,NMODH,LPMUOF)
      DO I = 1,NMODH
        CALL GTMUOF(I,NMOD,NRAW,IMUD1,NPROC,IMUON,NPLH0,NPLH1,
     $              NPLH2,NPLH3)
        IF(NPROC.GT.0) THEN
          DO J=1,9
            IF(NMOD.EQ.TOP_EAST(J)) THEN 
              ITOPEAST=ITOPEAST+1
              GOTO 100
            ENDIF            
            IF(NMOD.EQ.TOP_WEST(J)) THEN 
              ITOPWEST=ITOPWEST+1
              GOTO 100
            ENDIF   
          ENDDO
          DO J=1,11
            IF(NMOD.EQ.BOT_EAST(J)) THEN 
              IBOTEAST=IBOTEAST+1
              GOTO 100
            ENDIF            
            IF(NMOD.EQ.BOT_WEST(J)) THEN 
              IBOTWEST=IBOTWEST+1
              GOTO 100
            ENDIF            
          ENDDO
          DO J=1,6
            IF(NMOD.EQ.TOP_SIDES_EAST(J)) THEN 
              ITOPSIDESEAST=ITOPSIDESEAST+1
              GOTO 100
            ENDIF            
            IF(NMOD.EQ.TOP_SIDES_WEST(J)) THEN 
              ITOPSIDESWEST=ITOPSIDESWEST+1
              GOTO 100
            ENDIF   
            IF(NMOD.EQ.BOT_SIDES_EAST(J)) THEN 
              IBOTSIDESEAST=IBOTSIDESEAST+1
              GOTO 100
            ENDIF 
            IF(NMOD.EQ.BOT_SIDES_WEST(J)) THEN 
              IBOTSIDESWEST=IBOTSIDESWEST+1
              GOTO 100
            ENDIF 
          ENDDO
          DO J=1,3
            IF(NMOD.EQ.SIDES_A_WEST(J)) THEN 
              ISIDESAWEST=ISIDESAWEST+1
              GOTO 100
            ENDIF
            IF(NMOD.EQ.SIDES_A_EAST(J)) THEN 
              ISIDESAEAST=ISIDESAEAST+1
              GOTO 100
            ENDIF
          ENDDO
        ENDIF              !NPROC LOOP
  100   CONTINUE
      ENDDO      

C--------- DECIDE ANSWER ---------------------------------

      IMUTHRU = 0
      IF(ITOPEAST.GT.0.AND.IBOTWEST.GT.0) IMUTHRU = 1
      IF(ITOPWEST.GT.0.AND.IBOTEAST.GT.0) IMUTHRU = 1
      IF(ITOPSIDESEAST.GT.0.AND.IBOTSIDESWEST.GT.0) IMUTHRU = 1
      IF(ITOPSIDESWEST.GT.0.AND.IBOTSIDESEAST.GT.0) IMUTHRU = 1
      IF(ITOPSIDESEAST.GT.0.AND.ISIDESAWEST.GT.0) IMUTHRU = 1
      IF(ITOPSIDESWEST.GT.0.AND.ISIDESAEAST.GT.0) IMUTHRU = 1


  999 RETURN
      END
