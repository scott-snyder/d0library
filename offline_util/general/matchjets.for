      SUBROUTINE MATCHJETS(N1,P1,Q1,N2,P2,Q2,MATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Matching two arrays according to distance in
C-                         (P-Q)-space. The Default minimum R cut is 0.5.
C-                         Use entry point MATCHJETS_SET_RCUT(CUT) to
C-                         re-define the R cut.
C-
C-   Inputs  : N1       [I]     Number of elements in P1 and Q1 arrays
C-             P1       [R]     Array with N1 components
C-             Q1       [R]     Array with N1 components
C-             N2       [I]     Number of elements in P2 and Q2 arrays
C-             P2       [R]     Array with N2 components
C-             Q2       [R]     Array with N2 components
C-   Outputs : MATCH(N1)[I]     Array containing the array index of the P2,Q2
C-                              list that matchs to the P1,Q1 list.
C-   Controls: none
C-
C-   Created  20-JAN-1990   Boaz Klima
C-   Updated  26-JAN-1990   Chip Stewart
C-   Updated   2-May-1990   N.A. Graf  generalized
C-   Updated  31-JAN-1991   Boaz Klima, Harrison B. Prosper
C-   Updated   5-DEC-1991   Stan M. Krzywdzinski, Harrison B. Prosper
C-      Add entry point MATCHJETS_SET_RCUT
C-   Updated   5-JUL-1993   Chip Stewart   - RETRY
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N1, N2, MATCH(N1)
      REAL    P1(N1), Q1(N1), P2(N2), Q2(N2)
      INTEGER I, J, K
      INTEGER IJET,KJET,LJET,MJET,MATCHED(100)
      REAL    DEL_ETA,DEL_PHI,DEL_R,MIN_R,INIT_MIN_R,DEL_R2,DIFF_PHI
      SAVE    INIT_MIN_R
      REAL    CUT
      DATA INIT_MIN_R  /0.5/   ! Default cut
C----------------------------------------------------------------------
C
C ****  Are there any elements in the first array?
C
      IF ( N1.EQ.0 ) THEN
        CALL ERRMSG('NOFIRST','MATCHJETS',
     &    ' No elements in first array! ','W')
        GOTO 999
      ENDIF
C
C ****  Are there any elements in the second array?
C
      IF ( N2.EQ.0 ) THEN
        CALL ERRMSG('NOSECOND', 'MATCHJETS',
     &    ' No elements in second array! ','W')
        GOTO 999
      ENDIF
C
C ****  SORT WHAT GOES WITH WHAT (MINIMUM DELTA R)
C
      DO 1, IJET = 1, N1
        MATCH (IJET) = 0
        MIN_R = INIT_MIN_R
        DO KJET = 1, N2
          DEL_PHI = DIFF_PHI( P1(IJET), P2(KJET) )
          DEL_ETA = ABS ( Q1(IJET) - Q2(KJET) )
          DEL_R = SQRT ( DEL_ETA*DEL_ETA + DEL_PHI*DEL_PHI )
          IF(DEL_R .LT. MIN_R) THEN
            MATCH(IJET) = KJET
            MATCHED(KJET) = IJET
            MIN_R = DEL_R
          ENDIF
        ENDDO
        IF(MATCH(IJET).eq.0) GOTO 1
        DO KJET = 1, IJET -1
          IF (MATCH(KJET) .EQ. MATCH (IJET) ) THEN
C
C ****  TWO  LIST1 JETS MAPPED TO ONE LIST2 JET
C
            DEL_PHI = DIFF_PHI(P1(IJET), P2(MATCH(IJET)) )
            DEL_ETA = ABS (Q1(IJET) - Q2 (MATCH(IJET)) )
            DEL_R = SQRT ( DEL_ETA*DEL_ETA + DEL_PHI*DEL_PHI )
            DEL_PHI = DIFF_PHI(P1(KJET), P2(MATCH(IJET)) )
            DEL_ETA = ABS (Q1(KJET) - Q2 (MATCH(IJET)) )
            DEL_R2 = SQRT ( DEL_ETA*DEL_ETA + DEL_PHI*DEL_PHI )
C
C ****  KEEP ONLY MAP WITH MIN R
C
            IF (DEL_R .GT. DEL_R2 ) THEN
              LJET = IJET
            ELSE
              LJET = KJET
            END IF
            MATCH (LJET) = 0
C
C ****  TRY MATCH WITH REMAINING UNMATCHED JETS
C
            MIN_R = INIT_MIN_R
            DO MJET = 1, N2
              IF(MATCHED(MJET).EQ.0) THEN
                DEL_PHI = DIFF_PHI( P1(LJET), P2(MJET) )
                DEL_ETA = ABS ( Q1(LJET) - Q2(MJET) )
                DEL_R = SQRT ( DEL_ETA*DEL_ETA + DEL_PHI*DEL_PHI )
                IF(DEL_R .LT. MIN_R) THEN
                  MATCH(LJET) = MJET
                  MATCHED(MJET) = LJET
                  MIN_R = DEL_R
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
    1 CONTINUE
      RETURN
C
C ****  Set initial cut
C
      ENTRY MATCHJETS_SET_RCUT(CUT)
      INIT_MIN_R = CUT
  999 RETURN
      END
