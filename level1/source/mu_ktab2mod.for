	SUBROUTINE MU_KTAB2MOD(KMOD,MODNO)
C******************************************************************
C
C	CONVERTS 8 BIT COMPRESSED PHI MARTIN NUMBER USED IN K TABLES
C	 TO STANDARD PHIL MARTIN NUMBER
C
C       NOTE : ALL MODULES UP TO 256 WILL HAVE THE SAME KMOD,
C         WHILE THOSE GREATER THAN 256 WILL BE FILLED INTO THE
C         SPACES(<256) WHERE NO PHIL MARTIN NUMBER WAS USED
C
C  DF 1/92
C  Rename from KTAB2MOD to MU_KTAB2MOD  10-18-92  K. Bazizi
C************************************************************
C
        IMPLICIT NONE
C
	INTEGER MODNO,KMOD
C
	IF (KMOD.GE.40.AND.KMOD.LE.59) THEN
	  MODNO = KMOD + 220
	ELSE
	  IF (KMOD.GE.70.AND.KMOD.LE.89) THEN
	    MODNO = KMOD + 210
	  ELSE
	    IF (KMOD.GE.170.AND.KMOD.LE.179) THEN
	      MODNO = KMOD + 130
	    ELSE
	      MODNO = KMOD
	    END IF
	  END IF
	END IF
C
	RETURN
	END
