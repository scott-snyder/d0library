      SUBROUTINE GTMDFT(NMOD,IFAST,TCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get time to distance corrections
C-
C-   Input:   NMOD    Module ID
C-
C-   Output:  TCOR    16 TIME-->DISTANCE PARAMETERS + 21  CORRECTION VALUES
C-            IFAST  = 0  Monte Carlo
C-            IFAST  = 1  Old time-to-distance function
C-            IFAST  = 2  New time-to-distance function, 5% CF4
C-            IFAST  = 3  New time-to-distance function, 6% CF4
C-
C-   Created: DH 9-91
C-   Modified: DH 2/92 use word 3 for gas speed flag
C-             DH 5/92 default is fast gas
C-             RM 12/93 default is now 6% CF4
C-             MF 7/95 call errmsg only once
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL TCOR(42),TCORA(21)
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER MC,GZMUD1,L,I,NMOD,GZMDFT,LCON,TIME,IFAST
      INTEGER MUDVER
      EXTERNAL MUDVER
      LOGICAL FIRST, ERR_OUT
      REAL T2D(2:3,16)
C
      DATA (T2D(2,I),I=1,16) /
     A  5.9701959E-03,-1.6212543E-04, 2.1847379E-03,
     A 6.1811381E-03,1.4860094E-06,8.5249812E-06,-3.8166989E-05,
     A -1.0902422E-06,1.1466075E-09,-2.8546268E-08,1.1918927E-07,
     A -5.5505023E-08,-3.0412472E-12,2.4168509E-11,-9.8727776E-11,
     A  6.6484457E-11 /
C
      DATA (T2D(3,I),I=1,16) /
     A 0.62844059E-02, -.15985904E-02, 0.41781063E-02, 0.69831037E-02,
     A 0.24794772E-05, 0.18834664E-04, -.36491496E-04, -.26081514E-04,
     A 0.82158808E-09, -.53521656E-07, 0.10187843E-06, 0.18434831E-07,
     A -.41228106E-11, 0.42948901E-10, -.80007612E-10, 0.51889027E-11 /
C
      DATA TCORA / -.03,-.01,-.03,-.02,-.01,.0,.02,.05,.05,
     A .05,.06,.08,.08,.08,.08,.11,.11,.16,.17,.28,.36 /
C
      DATA FIRST/.TRUE./, ERR_OUT/.FALSE./
C
      IF (FIRST) THEN
        MC = MUDVER(0)
        IF(MC.GE.0) THEN
          FIRST = .FALSE.
          MC = MC/10
          IF(MOD(MC,2).EQ.1) THEN       ! MONTE CARLO
            IFAST = 0
            DO I=1,42
              TCOR(I)=0.
            ENDDO
            GOTO 999
          ENDIF
        ELSE
          FIRST = .TRUE.
        ENDIF
      ENDIF
C
      LCON=GZMDFT(NMOD)
      IF(LCON.EQ.0) THEN ! NO CONSTANTS FROM DATABASE; USE DEFAULTS
        IF (.NOT.ERR_OUT) THEN
          CALL ERRMSG('No MDFT bank - use defaults','GTMDFT',' ','I')
          ERR_OUT = .TRUE.
        ENDIF
        IFAST=3
        DO I=1,16
          TCOR(I)=T2D(IFAST,I)
        ENDDO
        DO I=1,21
          TCOR(21+I)=0.0
        ENDDO
      ELSE
        IFAST=IC(LCON+3)
        IF (IFAST .EQ. 1) THEN          ! Use old time-to-distance function
          DO I=1,21
            TCOR(I)=C(LCON+10+I)
            TCOR(21+I)=0.0
          ENDDO
        ELSE                            ! Use new function
          DO I=1,16
            TCOR(I)=T2D(IFAST,I)
          ENDDO
          DO I=1,21
            TCOR(21+I)=C(LCON+10+I)
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
