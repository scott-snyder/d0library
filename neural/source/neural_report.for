      SUBROUTINE NEURAL_REPORT(LUNOUT,ICYCLE,NCYCLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : REPORT STATISTICS ON NETWORK
C-   Inputs  : LUNOUT [I]   OUTPUT UNIT NUMBER
C-             ICYCLE [I]   CYCLE ITERATION NUMBER
C-             NCYCLE [I]   MAXIMUM NUMBER OF CYCLES
C-   Outputs : None
C-   Controls: None
C-
C-   Created  16-JAN-1994   Chip Stewart
C-   Updated   7-MAR-1995   Harrison B. Prosper
C-    Implement event weighting
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNOUT, ICYCLE, NCYCLE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:JETNET.INC'
      INCLUDE 'D0$INC:JNDAT1.INC'
C----------------------------------------------------------------------
      INTEGER IPAT,I,J,NBINS,ID(0:20)
      INTEGER LCYCLE, TOTAL, NREPORT, OFFSET
C
      REAL    GLOBAL_ERROR, LAST_GLOBAL_ERROR, DERROR, E, F, D
      REAL    POW, WT, HSUM, HI, S1, S2
      CHARACTER*80 STRING
C
      LOGICAL FIRST
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./,LCYCLE/1/,NBINS/40/
      DATA OFFSET /300/
      SAVE FIRST, LCYCLE, NBINS, ID
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        DO I =  0, 4
          ID(I) = OFFSET+I
        ENDDO
C
        I = NCYCLE/NBINS
        D = I*NBINS
        CALL HBOOK1(ID(0),'SIGNAL-BACKGROUND',NBINS,0.0,1.0,0.0)
        CALL HBOOK1(ID(1),'ERROR',NBINS,0.0,D,0.0)
        CALL HBOOK1(ID(2),'POWER',NBINS,0.0,D,0.0)
        CALL HBOOK1(ID(3),'SIGNAL',NBINS,0.0,1.0,0.0)
        CALL HBOOK1(ID(4),'BACKGROUND',NBINS,0.0,1.0,0.0)
      ENDIF
C
      IF ( ICYCLE.LE.LCYCLE ) THEN
        NREPORT = 0
        LAST_GLOBAL_ERROR = 1.0
      END IF
      NREPORT = NREPORT+1
C
C ****  Compute Global Error
C
      E  = 0.0
C
C ****  Loop over events
C
      CALL HRESET(ID(3),' ')
      CALL HRESET(ID(4),' ')
C
      DO IPAT = 1, NPATTERNS
C
        DO I = 1, NINPUTS
          OIN(I)=PATTERN_IN(PATTERN_SELECT(I),IPAT)
        ENDDO
C
        CALL JNTEST                   ! Compute net output
C
C ****  Fill histogram for 1st output only
C
        D  = PATTERN_OUT(1,IPAT)    ! Desired output
        F  = OUT(1)                 ! Actual output
        IF ( D .GT. 0.5 ) THEN
          CALL HF1(ID(3),F,1.0)
        ELSE
          CALL HF1(ID(4),F,1.0)
        ENDIF
C
        WT  = PATTERN_WT(IPAT)        ! Pattern weight
        DO I = 1, NOUTPUTS
          D  = PATTERN_OUT(I,IPAT)    ! Desired output
          F  = OUT(I)                 ! Actual output
          E  = E + WT*(F-D)**2
        ENDDO
      ENDDO
C
      TOTAL = NOUTPUTS*NPATTERNS
      GLOBAL_ERROR = E/TOTAL          ! <WT*(F-D)**2>
      DERROR = (LAST_GLOBAL_ERROR-GLOBAL_ERROR)
     &         /LAST_GLOBAL_ERROR
C
C ****  CALC OUTPUT POWER FOR 1st OUTPUT ONLY
C
      S1 = 0.5/HSUM(ID(3))
      S2 = 0.5/HSUM(ID(4))
      CALL HOPERA(ID(3),'-',ID(4),ID(0),S1,S2)
C
      POW = 0.0
      DO J = 1, NBINS
        POW = POW + ABS(HI(ID(0),J))
      ENDDO
      IF ( POW.GT.1 ) THEN
        POW=-1
      ENDIF
C
C ****  Histogram error and power
C
      CALL HF1(ID(1),FLOAT(ICYCLE),GLOBAL_ERROR)
      CALL HF1(ID(2),FLOAT(ICYCLE),POW)
C
C ****  Write out errors
C
      IF ( MOD(NREPORT+50,50).EQ.1 ) WRITE(6,1000)
      WRITE(LUNOUT,1000)
      WRITE(STRING,1005) ICYCLE,GLOBAL_ERROR,DERROR,POW
      WRITE(6,'(A)')      STRING
      WRITE(LUNOUT,'(A)') STRING
C
      LCYCLE = ICYCLE
      LAST_GLOBAL_ERROR   = GLOBAL_ERROR
  999 RETURN
C
C ****  FORMATS
C
 1000 FORMAT(/1X,
     &       2X,'CYCLES',
     &       1X,'   FUNCTION',
     &       1X,'DF/FUNCTION',
     &       3X,'POWER')
 1005 FORMAT(1X,I8,2(1X,1PE11.3),1X,0PF7.4)
 1010 FORMAT(1X,1X,I10,1X,'Dumping Weights')
      END
