      SUBROUTINE GTSWCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gets the Switches FROM bank
C-                         and puts them Judiciously into D0LOG
C-                         SVSWCH puts the D0LOG switches and puts them
C-                         into bank hanging off user hook off RUNG.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INTEGER LD0RG,LGSWT,I,IXIO,K,GZGSWT,IOF
      INCLUDE 'D0$INC:GCMAIL.INC'
C----------------------------------------------------------------------
C
C ****  Can over-ride FFREAD switches in certain cases. The logic
C ****  is that the Geometry Switches such as DCEN etc cannot be
C ****  increased above the save file.
C
      LGSWT = GZGSWT()
      IF ( LGSWT.EQ.0 ) THEN
        WRITE(CHMAIL,*) ' NO GSWT BANK PRESENT '
        RETURN
      ELSE
C
C ****  CENTRAL DETECTOR
C
        CALL SUPER_GSWT(DCEN,IQ(LGSWT+20))
        CALL SUPER_GSWT(DVTX,IQ(LGSWT+21))
        CALL SUPER_GSWT(DTRD,IQ(LGSWT+22))
        CALL SUPER_GSWT(DCDC,IQ(LGSWT+23))
        CALL SUPER_GSWT(DFDC,IQ(LGSWT+24))
C
C ****  CALORIMETER
C
        CALL SUPER_GSWT(DCAL,IQ(LGSWT+80))
        CALL SUPER_GSWT(DECA,IQ(LGSWT+81))
        CALL SUPER_GSWT(DUCA,IQ(LGSWT+82))
        CALL SUPER_GSWT(DCRY,IQ(LGSWT+83))
        CALL SUPER_GSWT(DEAD,IQ(LGSWT+84))
C
C ****  MUONS
C
        CALL SUPER_GSWT(DMUO,IQ(LGSWT+96))
C
C
C ****  Level 0
C
        CALL SUPER_GSWT(DLV0,IQ(LGSWT+108))
C
      ENDIF
      WRITE(CHMAIL,*) ' D0LOG SWITCHES RESET FROM THE SAVE FILE '
      CALL GMAIL(0,0)
C
      CALL PRGSWT                       ! PRINT SWITCHES
C
      RETURN
C
      ENTRY SVSWCH
C
C ****  SAVE SWITCHES INTO A BANK HANGING OFF LQ(-1) IN JRUNG.
C
      CALL BKD0RG(LD0RG)
      CALL BKGSWT(LGSWT)
C
      IQ(LGSWT+2) = NSWTS               ! NUMBER OF SWITCHES
      DO 100 I = 1,NSWTS
        IQ(LGSWT+2+I) = ID0LOG(I)
  100 CONTINUE
C
      WRITE(CHMAIL,*) ' GSWT BANK FILLED WITH D0LOG SWITCHES'
      CALL GMAIL(0,0)
C
  999 RETURN
      END
