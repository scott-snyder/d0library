      SUBROUTINE HCFILL
C
      INCLUDE 'D0$INC:HCAL.INC/LIST'
C
      REAL SUM,PER
C
C      CALL HFILL(4000,TOTUCA)
C      CALL HFILL(4001,TOTECA)
C      CALL HFILL(4002,TOTCRK)
      SUM=TOTUCA+TOTECA+TOTCRK
C      CALL HFILL(4003,SUM)
      IF(SUM.NE.0) THEN
        PER=TOTCRK/SUM
C        CALL HFILL(4004,PER)
      ENDIF
C ENERGY TOTALS
      TOTECA = 0
      TOTUCA = 0
      TOTCRK = 0
      TOTMSG = 0
      TOTSCN = 0
C SMEARED TOTALS
      TOTUCS = 0
      TOTECS = 0
      TOTMSS = 0
C
      END
