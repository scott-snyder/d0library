      SUBROUTINE SHLB_STAT(SHLB,ISA_PARTID,ISA_ENERGY,TOT_ENERGY,
     &  TOT_CAL_ENERGY,TOT_DEAD_ENERGY,TOT_ICD_ENERGY,TOT_MG1_ENERGY,
     &  TOT_MG2_ENERGY,ENER_MISS,NUM_GCAH,NUM_HITS,DROP_ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYSES AN SHLB BANK
C-
C-   Inputs  : SHLB = SHLB ARRAY CONTAINING SHOWERLIBRARY INFO
C-   Outputs : ISA_PARTID = ISAJET ID OF PARENT PARTICLE IN SHLB BANK
C-             ISA_ENERGY = ENERGY OF ISAJET TRACK
C-             TOT_ENERGY = ENERGY AT ENTRY INTO CALORIMETER
C-             TOT_CAL_ENERGY = TOTAL ENERGY IN CALORIMETER
C-             TOT_DEAD_ENERGY = TOTAL DEAD MATERIAL ENERGY
C-             TOT_ICD_ENERGY = TOTAL ICD ENERGY
C-             TOT_MG1_ENERGY = TOTAL MASSLESS GAP1 ENERGY
C-             TOT_MG2_ENERGY = TOTAL MASSLESS GAP2 ENERGY
C-             ENER_MISS (4) = Energy not explicitly accounted for in
C-             HITS in Calorimeter, MG1, ICD and MG2 respy.
C-             NUM_GCAH = NUMBER OF GCAH TRACKS
C-             NUM_HITS = TOTAL NUMBER OF HITS STORED
C-             DROP_ENERGY = TOTAL ENERGY OF DROPPED GCAH  
C-   Controls:
C-
C-   Created   4-OCT-1990   Rajendran Raja
C-   Updated  13-AUG-1992   W.G.D.Dharmaratna  modified for version 2 
C-   Updated   3-DEC-1992   W. Dharmaratna  BACK TO DROPPED ENERGY 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL    SHLB(*),ENER_MISS(*)
      INTEGER ISA_PARTID
      REAL    ISA_ENERGY,TOT_ENERGY,DROP_ENERGY
      REAL    TOT_CAL_ENERGY,TOT_DEAD_ENERGY,TOT_ICD_ENERGY,
     &  TOT_MG1_ENERGY,TOT_MG2_ENERGY
      INTEGER NUM_GCAH,NUM_HITS,PT_GCAH,NUMHITS
      INTEGER INDEX,I
      REAL    ECELL
      INTEGER IL,IE,IP,IL_DEAD
C----------------------------------------------------------------------
      ISA_PARTID = SHLB(5)              ! ISAJET ID OF PARENT
!      ISA_KENERGY = SHLB(9)-SHLB(12)   !KINETIC ENERGY
      ISA_ENERGY = SHLB(9)             ! ENERGY
      PT_GCAH = SHLB(2)                 ! POINTER TO 1ST GCAH AREA IN SHLB
      DROP_ENERGY = SHLB(19) 
C
      TOT_ENERGY = 0.
      TOT_CAL_ENERGY = 0.
      TOT_DEAD_ENERGY = 0.
      TOT_ICD_ENERGY = 0.
      TOT_MG1_ENERGY = 0.
      TOT_MG2_ENERGY = 0.
C
      NUM_GCAH = 0
      NUM_HITS = 0
C
      DO I = 1 , 4
        ENER_MISS(I) = 0.0
      ENDDO
 1000 NUMHITS=SHLB(PT_GCAH+12)          ! NUMBER OF HITS PER GCAH BANK
C
      NUM_HITS = NUM_HITS + NUMHITS
      TOT_ENERGY = TOT_ENERGY + SHLB(PT_GCAH+4)
      TOT_DEAD_ENERGY = TOT_DEAD_ENERGY  + SHLB(PT_GCAH+10)
C
C ****  THE ENERGIES BELOW ARE WHAT ARE LEFT OVER AFTER HITS
C ****  ARE STORED AWAY.
C
      TOT_CAL_ENERGY = TOT_CAL_ENERGY + SHLB(PT_GCAH+6)
      TOT_MG1_ENERGY = TOT_MG1_ENERGY + SHLB(PT_GCAH+7)
      TOT_ICD_ENERGY = TOT_ICD_ENERGY + SHLB(PT_GCAH+8)
      TOT_MG2_ENERGY = TOT_MG2_ENERGY + SHLB(PT_GCAH+9)
      DO I  = 1,4
        IF(SHLB(PT_GCAH+I+5).LT.-0.1)THEN
          CALL ERRMSG('SHOWERLIBRARY','SHLB_STAT',
     &      'NEGATIVE MISSING ENERGY ','W')
          CALL ABORT
        ELSE
          ENER_MISS(I) = ENER_MISS(I) + SHLB(PT_GCAH+I+5)
        ENDIF
      ENDDO
C
      DO I = 1,NUMHITS
        INDEX=SHLB(PT_GCAH+12+2*I)
        ECELL=SHLB(PT_GCAH+11+2*I)
C
C ****  Unpack the index to IETAC, IPHI, ILYR coordinates
C
        IE=ISIGN(IABS(INDEX)/10000,INDEX)
        IP=MOD(IABS(INDEX)/100,100)
        IL=MOD(IABS(INDEX),100)
        IF ( IL.EQ.MNLYMG ) THEN
          TOT_MG1_ENERGY = TOT_MG1_ENERGY + ECELL
        ELSEIF ( IL.EQ.LYICD) THEN
          TOT_ICD_ENERGY = TOT_ICD_ENERGY + ECELL
        ELSEIF ( IL.EQ.MXLYMG ) THEN
          TOT_MG2_ENERGY = TOT_MG2_ENERGY + ECELL
        ELSE
          TOT_CAL_ENERGY = TOT_CAL_ENERGY + ECELL
        ENDIF
      ENDDO
      NUM_GCAH = NUM_GCAH + 1
C
      PT_GCAH = SHLB(PT_GCAH)
      IF(PT_GCAH.NE.0)GO TO 1000        ! MORE GCAH STUFF
C
  999 RETURN
      END
