      SUBROUTINE KTCLFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book and fill the KTCL (KT cluster) bank.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-FEB-1995   Richard V. Astur
C-   Updated  1-oct-1995    Zero the third header word for safty
C-                          Gordon Watts
C-   Updated  30-oct-1995   Fixed up references to LHEAD to GZKTCL!
C-                          Gordon Watts
C-   Updated   2-nov-1995   Added some basic test histograms
C-                          Gordon Watts
C-   Updated   10-Nov-1995  Added number of cells into histograms
c-                          Brad Abbott
c-   Updated  20-Nov-1995   Don't zero lktcl+3 right after setting it!
c-                          Gordon Watts
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      REAL  ESWITCH, EMIN, EMAX
      PARAMETER( EMIN = 0.0 )
      PARAMETER( ESWITCH = 80.0 )
      PARAMETER( EMAX = 900. )
      INTEGER JBYT
      EXTERNAL JBYT
      REAL ENERGY, PHI, ETA, STEP,RAT
      INTEGER   LKTCL, SCALE
      INTEGER IWORD, J, I, PWORD, PWORD2, NCLUSTER
      INTEGER IWORD2
      INTEGER NUMBER_PACK_VALUES
      PARAMETER (NUMBER_PACK_VALUES = 10)
      REAL PACK_VALUES(NUMBER_PACK_VALUES)
      INTEGER PACK_INT(NUMBER_PACK_VALUES)
      REAL PACK_MIN(NUMBER_PACK_VALUES)
      REAL PACK_MAX(NUMBER_PACK_VALUES)
      SAVE PACK_MIN, PACK_MAX, PACK_PREC
      INTEGER PACK_PREC(NUMBER_PACK_VALUES)
      INTEGER START_BIT
      REAL EM_FRAC, ICD_FRAC, FH_FRAC
      REAL CELL_EM, CELL_ICD, CELL_FH, CELL_CH

      INTEGER GZKTCL

C                       ETA     PHI   ENERGY EMf  ICDf fhf
C          #em, #icd, #fh, #ch
      DATA PACK_MIN  /   -5.0,   0.0,  0.0,  0,  0.0,  0.0,
     $     0.0, 0.0, 0.0, 0.0/                    ! MINIMUM VALUES
      DATA PACK_MAX  /    5.0,   6.2832, 80.,  1.0, 1.0, 1.0,
     $     63.0, 15.0, 31.0, 7.0/                      ! MAXIMUM VALUES
      DATA PACK_PREC /   9, 9, 13, 6, 6, 6,
     $     5, 3, 4, 2/                                 ! # of BITS

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
C
C If we are doing histograms, then get into our subdirectory.
C

      IF (DO_BASIC_HIST) THEN
        CALL HCDIR ('//PAWC/KTJET', ' ')
      ENDIF

C
C: Check if KTCL is already filled
C
      LKTCL = GZKTCL ()
      IF ( LKTCL .GT. 0 ) THEN
C        CALL ERRMSG('KTCL there','KTCLFL','Will not remake','W')
C        RETURN
      ENDIF
C
C: Find number of valid clusters
C
      NCLUSTER  = 0
      DO I = 1, IQ(LKVEC+3)
        IF ( IQ( POINT(I)) .GT. 0 ) NCLUSTER  = NCLUSTER + 1
      ENDDO

      IF (DO_BASIC_HIST) THEN
        CALL HFILL (100, REAL(NCLUSTER), 0.0, 1.0)
      ENDIF

C
C: Book bank with number of clusters (iq(lkvec+3))
C
      IF ( NCLUSTER .GT. 0 ) THEN
        CALL BKKTCL( LKTCL, NCLUSTER)
        IF (DO_BASIC_HIST) THEN
          CALL HFILL (106, REAL(3+NCLUSTER*2), 0.0, 1.0)
        ENDIF
      ELSE
        CALL ERRMSG('No KT Clusters?','KTCLFL','Nothing to store','W')
        RETURN
      ENDIF

C
C: Did bank get booked?
C
      IF ( LKTCL .GT. 0 ) THEN
        IQ( LKTCL + 4 ) = INPUT_TYPE        ! Where the clusters were made from
      ELSE
        CALL ERRMSG('Book error','KTCLFL','Error booking KTCL bank','F')
      ENDIF

C
C  Bank was booked.  Pack word1 and word2 with all our info (gulp).
C  Word 3 is left for future expansion.  Set it to zero.
C

      CALL KTCL_HEADER_PACK_ONE (IQ(LKTCL+1),
     $     1, NCLUSTER, LARGE_NEGATIVE_CELLS, ET_REMOVED)

      CALL KTCL_HEADER_PACK_TWO (IQ(LKTCL+2),
     $     MIN_PREC_ET, SEARCH_SIZE, INPUT_TYPE, PREC_WIDTH,
     $     IETDEF, IETADEF, .NOT. USE_DET_ETA, ETA_ORDERED,
     $     REMOVE_LOW_ET_CELLS, DO_PRECLUSTER)

      CALL KTCL_HEADER_PACK_THREE (IQ(LKTCL+3),
     $     MINIMUM_ET, DELTA_ET)

      IF (DO_BASIC_HIST) THEN
        CALL HFILL (104, REAL(ET_REMOVED), 0.0, 1.0)
        CALL HFILL (105, REAL(LARGE_NEGATIVE_CELLS), 0.0, 1.0)
      ENDIF

C
C: Fill bank
C
      DO I = 1, NCLUSTER
        IF ( IQ( POINT(I)) .GT. 0 ) THEN
C
C: Store Cluster ETA,PHI and Energy
C
          PACK_VALUES(1)  = Q( KTETA(I) )
          PACK_VALUES(2)  = Q( KTPHI(I) )
          PACK_VALUES(3)  = Q( P0(I) )
          PACK_VALUES(4) = Q(KT_EM_FRAC(I))
          PACK_VALUES(5) = Q(KT_ICD_FRAC(I))
          PACK_VALUES(6) = Q(KT_FH_FRAC(I))
          PACK_VALUES(7) = Q(KT_CELL_EM(I))
          PACK_VALUES(8) = Q(KT_CELL_ICD(I))
          PACK_VALUES(9) = Q(KT_CELL_FH(I))
          PACK_VALUES(10) = Q(KT_TOT_CELLS(I)) -
     $         (Q(KT_CELL_EM(I))+Q(KT_CELL_ICD(I))+Q(KT_CELL_FH(I)))


          IF (DO_BASIC_HIST) THEN
            CALL HFILL (101, REAL(Q(KTETA(I))), 0.0, 1.0)
            CALL HFILL (102, REAL(Q(KTPHI(I))), 0.0, 1.0)
            CALL HFILL (103, REAL(Q(P0(I))), 0.0, 1.0)
            CALL HFILL (107, REAL(Q(KT_CELL_EM(I))), 0.0, 1.0)
            CALL HFILL (108, REAL(Q(KT_CELL_ICD(I))), 0.0, 1.0)
            CALL HFILL (109, REAL(Q(KT_CELL_FH(I))), 0.0, 1.0)
          ENDIF

C
C: Determine which scale we are using
C
          IF ( PACK_VALUES(3) .LT. ESWITCH ) THEN
            PACK_MIN(3) = EMIN
            PACK_MAX(3) = ESWITCH
            SCALE = 0
          ELSE
            PACK_MIN(3) = ESWITCH
            PACK_MAX(3) = EMAX
            SCALE = 1
          ENDIF
C
C: Place values in range
C
          DO J = 1, NUMBER_PACK_VALUES
            PACK_VALUES(J)  = MIN( PACK_MAX(J), MAX( PACK_MIN(J),
     &        PACK_VALUES(J) ))
          ENDDO
C
C: Convert to integer
C
          DO J = 1, NUMBER_PACK_VALUES
            RAT = ( ((2**PACK_PREC(J))-1)*(PACK_VALUES(J)
     &        -PACK_MIN(J))/
     &        (PACK_MAX(J)-PACK_MIN(J)) )
            PACK_INT(J) = NINT(RAT)
            PACK_INT(J)
     &          = MAX( 0, MIN( PACK_INT(J), 2**PACK_PREC(J)-1 ))
          ENDDO
C
C: Pack it
C
          IWORD = 0
          CALL SBYT( PACK_INT(1), IWORD, 1, PACK_PREC(1) )
          CALL SBYT( PACK_INT(2), IWORD, 10, PACK_PREC(2) )
          CALL SBYT( PACK_INT(3), IWORD, 19, PACK_PREC(3) )
          CALL SBYT( SCALE, IWORD, 32, 1 )

          IWORD2 = 0
          START_BIT = 1
          DO J = 4, NUMBER_PACK_VALUES
            CALL SBYT (PACK_INT(J), IWORD2, START_BIT, PACK_PREC(J))
            START_BIT = START_BIT + PACK_PREC(J)
          ENDDO

C
C: Store it
C
          IQ (LKTCL + KTCL_HEADER_SIZE
     $         + (I-1)*KTCL_BLOCK_SIZE + 1) = IWORD
          IQ( LKTCL + KTCL_HEADER_SIZE
     $         + (I-1)*KTCL_BLOCK_SIZE + 2) = IWORD2

        ENDIF
      ENDDO
  999 RETURN

C**********************************************************************
C UNPKTCL( IWORD, ETA, PHI, ENERGY )
C***********************************************************************
      ENTRY UNPKTCL( PWORD, PWORD2, ETA, PHI, ENERGY,
     $     EM_FRAC, ICD_FRAC, FH_FRAC,
     $     CELL_EM, CELL_ICD, CELL_FH, CELL_CH)

      PACK_INT(1) = JBYT( PWORD, 1, PACK_PREC(1) )
      PACK_INT(2) = JBYT( PWORD, 10, PACK_PREC(2) )
      PACK_INT(3) = JBYT( PWORD, 19, PACK_PREC(3) )
      SCALE       = JBYT( PWORD, 32, 1 )

      START_BIT = 1
      DO J = 4, NUMBER_PACK_VALUES
        PACK_INT(J) = JBYT (PWORD2, START_BIT, PACK_PREC(J))
        START_BIT = START_BIT + PACK_PREC(J)
      ENDDO


C
C: Set range for Energy SCALE
C
      IF ( SCALE .EQ. 0 ) THEN
        PACK_MIN(3) = EMIN
        PACK_MAX(3) = ESWITCH
      ELSE
        PACK_MIN(3) = ESWITCH
        PACK_MAX(3) = EMAX
      ENDIF
C
C: Map into ranges
C
      DO J = 1, NUMBER_PACK_VALUES
        STEP  = ( PACK_MAX(J) - PACK_MIN(J) )/FLOAT(2**PACK_PREC(J)-1)
        PACK_VALUES(J)  = PACK_MIN(J) + STEP*PACK_INT(J)
      ENDDO
C
C: Load return values
C
      ETA = PACK_VALUES(1)
      PHI = PACK_VALUES(2)
      ENERGY = PACK_VALUES(3)

      EM_FRAC = PACK_VALUES(4)
      ICD_FRAC = PACK_VALUES(5)
      FH_FRAC = PACK_VALUES(6)
      CELL_EM = PACK_VALUES(7)
      CELL_ICD = PACK_VALUES(8)
      CELL_FH = PACK_VALUES(9)
      CELL_CH = PACK_VALUES(10)

      RETURN
      END
