      SUBROUTINE TOP_FIT_COMBS(JETS,ISR,FSR,NTOT_COMB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Works out Combinations of jets
C-   will only take the most likely ISR combination and the most likely FSR
C-   combination in the case of moe than 4 jets. If njets=6, will only consider
C-   the top 5 jets.
C-
C-   Inputs  :P25_JETS(7,NJETS) = JET CANDIDATES. NJETS
C-   MAXIMUM OF 5 HANDLED
C-   Outputs :JETS(1:7,1:4,1:NTOT_COMB) = JET QUANTITIES IN COMBINMATIONS OF 4
C-            ISR(1:NTOT_COMB) = ISR LIKELIHOOD FOR COMBINATION
C-            FSR(1:NTOT_COMB) = FSR LIKELIHOOD FOR COMBINATION
C-            NTOT_COMB=TOTAL NUMBER OF COMBINATIONS
C-   Controls:
C-
C-   Created  17-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN1.INC'
      REAL JETS(7,4,*),ISR(*),FSR(*)
      INTEGER NTOT_COMB,NTOT
      INTEGER I,J,K,L,IJT
      INTEGER NCOMB(4,12)
      SAVE NCOMB
C
      REAL    TOP_ISR_LIKELIHOOD
      REAL    TOP_FSR_LIKELIHOOD
      REAL    ISR_JETS(7,4),FSR_JETS(7,4)
      REAL    CM_FRAME(4)
      REAL    ISRL,FSRL,ISRLMX,FSRLMX
      INTEGER IMX,JMX,ICMB
      INTEGER NJET5
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
C WORK OUT PERMUTATIONS (12 IN ALL) OF DISTRIBUTING 4 JETS INTO B_LEPTON
C B_HADRON , HIGH ET W DECAY JET AND LOW ET W DECAY JET.
C IT IS ASSUMED THAT THE JETS ARE PASSED TO THE ROUTINE IN DECREASING ET ORDER
C IN P25_JETS.
        NTOT = 0
        DO I = 1 , 4
          DO J = 1 , 4
            IF ( I.NE.J ) THEN
              DO K = 1 , 4
                IF ( I.NE.K.AND.J.NE.K ) THEN
                  DO L = K , 4
                    IF ( I.NE.L.AND.J.NE.L.AND.K.NE.L ) THEN
                      NTOT = NTOT + 1
                      NCOMB(1,NTOT) = I
                      NCOMB(2,NTOT) = J
                      NCOMB(3,NTOT) = K
                      NCOMB(4,NTOT) = L
                      PRINT 1234, NTOT, I,J,K,L
 1234                 FORMAT(' NTOT,I,J,K,L ',5I5)
                    ENDIF
                  ENDDO
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
        DO K = 1 , 3
          CM_FRAME(K)= 0.
        ENDDO
        CM_FRAME(4) = 1800.  !LAB=CM
      ENDIF
      IF ( NJETS.LT.4 ) THEN
        RETURN
      ELSEIF ( NJETS.EQ.4 ) THEN
C NO ISR OR FSR
        ICMB = 0
        NTOT_COMB = NTOT
        DO I = 1 , NTOT
          ISR(I) = 0.
          FSR(I) = 0.
          ICMB = ICMB + 1
          DO K = 1 , 4
C 4 JETS ORDERED
            CALL UCOPY(P25_JETS(1,NCOMB(K,I)),JETS(1,K,ICMB),7)
          ENDDO
        ENDDO
      ELSEIF ( NJETS.GE.5 ) THEN
        NJET5=MIN(NJETS,5)
C
C ****  NOW FOR ISR
C
        ISRLMX = 0.
        IMX = 0
        DO I = 1 , NJET5
          ISRL = TOP_ISR_LIKELIHOOD(P25_JETS(1,I),CM_FRAME,' ')
          IF ( ISRL.GT.ISRLMX ) THEN
            IMX=I
            ISRLMX=ISRL
          ENDIF
        ENDDO
        IJT = 0
        DO K = 1 , NJET5
          IF ( K.NE.IMX ) THEN
            IJT = IJT + 1
            CALL UCOPY(P25_JETS(1,K),ISR_JETS(1,IJT),7)
          ENDIF
        ENDDO
C
        NTOT_COMB = NTOT
        ICMB = 0
        DO I = 1 , NTOT
          ICMB = ICMB + 1
          ISR(I) = ISRLMX
          FSR(I) = 0.
          DO K = 1 , 4
C 4 JETS ORDERED
            CALL UCOPY(ISR_JETS(1,NCOMB(K,I)),JETS(1,K,ICMB),7)
          ENDDO
        ENDDO
C
C ****  NOW FOR FSR
C
        FSRLMX = 0.
        IMX = 0
        JMX = 0
        DO I = 1 , NJET5
          DO J =I  , NJET5
            IF ( I.NE.J ) THEN
              FSRL = TOP_FSR_LIKELIHOOD(P25_JETS(1,I),P25_JETS(1,J),' ')
              IF ( FSRL.GT.FSRLMX ) THEN
                IMX=I
                JMX = J
                FSRLMX=FSRL
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
        IJT = 0
        DO I = 1 , NJET5
          IF ( I.NE.IMX.AND.I.NE.JMX ) THEN
C JET NOT PART OF MAX FSR PAIR
            IJT = IJT + 1
            CALL UCOPY(P25_JETS(1,I),FSR_JETS(1,IJT),7)
          ENDIF
        ENDDO

        CALL ADD_JETS(P25_JETS(1,IMX),P25_JETS(1,JMX),FSR_JETS(1,IJT+1))
C
        DO I = 1 , NTOT
          FSR(I) = FSRLMX
          ISR(I) = 0.
          ICMB = ICMB + 1
          DO K = 1 , 4
C 4 JETS ORDERED
            CALL UCOPY(FSR_JETS(1,NCOMB(K,I)),JETS(1,K,ICMB),7)
          ENDDO
        ENDDO
        NTOT_COMB = NTOT + NTOT_COMB
      ELSE
      ENDIF
  999 RETURN
      END
