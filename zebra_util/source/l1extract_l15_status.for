      SUBROUTINE L1EXTRACT_L15_STATUS( L1_BLOCK, 
     &                                 USED_L15, SKIPPED_L15, TIMED_OUT, 
     &                                 ST_USED_L15, ST_L15_ANSWER, 
     &                                 L15_TERM_DONE, L15_TERM_ANSWER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return status information regarding Level 1.5,
C-   extracted from the Level 1 Crate in the TRGR bank. 
C-
C-   Inputs  : L1_BLOCK [I]
C-
C-             First word of the Level-1 Crate header in the TRGR bank.
C-          -> Use IQ( LTRGR_LEVEL1 )
C-             Where LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C-             cf. header of D0$ZEBRA_UTIL$SOURCE:GZFIND_CRATE.FOR for details
C-
C-           +------------------------------------------------------------+
C-           | You must pass the variable IQ(LTRGR_LEVEL1)   DIRECTLY     |
C-           | and NOT A COPY of it.                                      |
C-           |------------------------------------------------------------|
C-           | YES :   L1EXTRACT_L15_STATUS ( IQ(LTRGR_LEVEL1),... )      |
C-           |------------------------------------------------------------|
C-           | NO  :   L1_BLOCK = IQ( LTRGR_LEVEL1 )                      |
C-           |         L1EXTRACT_L15_STATUS ( L1_BLOCK, ... )             |
C-           +------------------------------------------------------------+
C- 
C-   Outputs : USED_L15 [L]     Whether the Level 1.5 cycle was performed for
C-                              this event.
C-                              
C-                              Note that a Level 1.5 Cycle is not always
C-                              performed when Level 1.5 Specific Triggers
C-                              fired. (cf. SKIPPED_L15)
C-                              
C-                              Note that the specific trigger fired mask in
C-                              IQ(LHEAD+11) (and also in IQ(LTRGR_LEVEL1+5)
C-                              where LTRGR_LEVEL1 is defined above) shows the
C-                              set of triggers that survived the Level 1.5
C-                              Cycle.  
C-
C-             SKIPPED_L15 [L]  Whether a Level 1.5 Cycle was skipped when 
C-                              at least one Level 1.5 Specific Trigger fired
C-                              for this event. 
C-                              
C-                              Note that a Level 1.5 Cycle will only be
C-                              performed if it necessary to decide whether the
C-                              event needs to be transferred to the Level 2.
C-                              If at least one pure Level 1 Specific Trigger
C-                              has fired, the event is immediately transferred
C-                              and the Level 1.5 Cycle is skipped.  
C-                              
C-                              Note that the specific trigger fired mask in
C-                              IQ(LHEAD+11) includes the set of L1.5 Specific
C-                              Triggers that fired. 
C-
C-             TIMED_OUT [L]    Whether the Level 1.5 Framework timed out
C-                              before completing a decision. When a Level 1.5
C-                              cycle times out, all pending Specific Triggers
C-                              are automatically accepted and appear in the
C-                              set of SpTrg that fired (i.e. IQ(LHEAD+11))
C-
C-                              This data corresponds to the current event only
C-                              if USED_L15 = .TRUE.
C-                              
C-             ST_USED_L15(0:15) [L]  An array indicating whether the Specific
C-                              Trigger was part of the level 1.5 cycle. This
C-                              set of SpTrg is a superset of the set of
C-                              SpTrg that survived (i.e. IQ(LHEAD+11))
C-                              
C-                              This data corresponds to the current event only
C-                              if USED_L15 = .TRUE.
C-                              
C-             ST_L15_ANSWER(0:15) [L] An array indicating whether the 
C-                              Level 1.5 Framework received an answer
C-                              (positive or negative) for the state of a 
C-                              Specific Trigger. This set is a subset of the
C-                              set of SpTrg that used the level 1.5 Cycle
C-                              (ST_USED_L15). 
C-                              
C-                              If the L1.5 answer was positive, the Specific
C-                              Trigger was accepted and appears in the set of
C-                              SpTrg that survived (i.e. IQ(LHEAD+11)). If the
C-                              L1.5 answer was negative the Specific Trigger
C-                              was rejected and does not appear in
C-                              IQ(LHEAD+11). If no L1.5 answer was received,
C-                              the Specific Trigger was accepted and appears
C-                              in IQ(LHEAD+11). 
C-                              
C-                              Note that all Level 1.5 Specific Triggers are
C-                              not always fully processed in a Level 1.5
C-                              Cycle. A Level 1.5 Cycle is terminated as soon
C-                              as it has decided of the fate of the event.
C-                              It will be terminated either 
C-                                      - when at least one L1.5 Specific
C-                                        Trigger has confirmed the event, 
C-                                      - when all L1.5 Specific Triggers have
C-                                        rejected the event, 
C-                                      - or when the timeout has been reached.
C-                              
C-                              This data corresponds to the current event only
C-                              if USED_L15 = .TRUE.
C-                              
C-             L15_TERM_DONE(0:31) [L] An array indicating for each Level 1.5 
C-                              Term whether a state was returned to the 
C-                              Level 1.5 Framework.
C-                              
C-                              This data corresponds to the current event only
C-                              if USED_L15 = .TRUE.
C-                              
C-             L15_TERM_ANSWER(0:31) [L] The state returned to the Level 1.5
C-                              Framework for each Level 1.5 Term.
C-                              
C-                              This data corresponds to the current event only
C-                              if USED_L15 = .TRUE. and the coresponding
C-                              L15_TERM_DONE = .TRUE. 
C-   Controls: 
C-
C-   Created  17-NOV-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
C
      INTEGER L1_BLOCK(0:*)
      LOGICAL SKIPPED_L15, USED_L15, TIMED_OUT
      LOGICAL ST_USED_L15(L15_SPEC_TRIG_NUM_MIN:L15_SPEC_TRIG_NUM_MAX)
      LOGICAL ST_L15_ANSWER(L15_SPEC_TRIG_NUM_MIN:L15_SPEC_TRIG_NUM_MAX)
      LOGICAL L15_TERM_DONE(L15_TERM_NUM_MIN:L15_TERM_NUM_MAX)
      LOGICAL L15_TERM_ANSWER(L15_TERM_NUM_MIN:L15_TERM_NUM_MAX)
C
      INTEGER MASK, TERM, SPEC_TRIG, L1_MASK
C
      INTEGER USED_BIT, SKIPPED_BIT, TIMEOUT_BIT
      PARAMETER (USED_BIT = 3, SKIPPED_BIT = 1, TIMEOUT_BIT = 15)
      INTEGER L15_ST_BITS_L
      PARAMETER (L15_ST_BITS_L = (L15_SPEC_TRIG_NUM_MAX
     &                            -L15_SPEC_TRIG_NUM_MIN+1)
     &                           /BYTE_LENGTH)
C
      CALL PRTRGR_FIRST_BYTE_DECODING(1, 
     &                                L1_5_RESERVED, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      USED_L15    = BTEST(MASK, USED_BIT)
      SKIPPED_L15 = BTEST(MASK, SKIPPED_BIT)
C
      CALL PRTRGR_FIRST_BYTE_DECODING(2,
     &  L15_STATUS + 2,
     &  L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &  MASK )
      TIMED_OUT = BTEST(MASK, TIMEOUT_BIT)
C
C       If a Level 1.5 cycle was performed, then the Level 1 fired mask
C       indicates which Specific Triggers required a Level 1.5 cycle for a
C       complete decision.
C
      CALL PRTRGR_FIRST_BYTE_DECODING(4, 
     &                                SP_TRG_FIRED, 
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      DO SPEC_TRIG = L15_SPEC_TRIG_NUM_MIN, L15_SPEC_TRIG_NUM_MAX
        ST_USED_L15(SPEC_TRIG) = BTEST(MASK, SPEC_TRIG)
      END DO
C
C       A complete Level 1.5 decision was achieved for a Specific Trigger if 
C       the Specific Trigger was either Confirmed or Rejected.
C
      CALL PRTRGR_FIRST_BYTE_DECODING(L15_ST_BITS_L, 
     &                                L15_STATUS+2*L15_ST_BITS_L,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      DO SPEC_TRIG = L15_SPEC_TRIG_NUM_MIN, L15_SPEC_TRIG_NUM_MAX
        IF (BTEST(MASK, SPEC_TRIG) .EQV. .TRUE.) 
     &    ST_L15_ANSWER(SPEC_TRIG) = .TRUE.
      END DO
C
      CALL PRTRGR_FIRST_BYTE_DECODING(L15_ST_BITS_L, 
     &                                L15_STATUS+3*L15_ST_BITS_L,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      DO SPEC_TRIG = L15_SPEC_TRIG_NUM_MIN, L15_SPEC_TRIG_NUM_MAX
        IF (BTEST(MASK, SPEC_TRIG) .EQV. .TRUE.) 
     &    ST_L15_ANSWER(SPEC_TRIG) = .TRUE.
      END DO
C
C       The Level 1.5 Term Done states
C
      CALL PRTRGR_FIRST_BYTE_DECODING(4, 
     &                                L15_STATUS+6*L15_ST_BITS_L,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        L15_TERM_DONE(TERM) = BTEST(MASK, TERM)
      END DO
C
C       The Level 1.5 Term Answer states
C
      CALL PRTRGR_FIRST_BYTE_DECODING(4, 
     &                                L15_STATUS+4*L15_ST_BITS_L,
     &                                L1_BLOCK(TRGR_HEADER_LENGTH+1),
     &                                MASK)
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        L15_TERM_ANSWER(TERM) = BTEST(MASK, TERM)
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
