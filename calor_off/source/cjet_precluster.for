      SUBROUTINE CJET_PRECLUSTER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds em+hadronic nearest neighbor clusters.
C-   using Saul Youssef's clustering algorithm. Modeled on CALCLU.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   3-OCT-1989   Gerald C. Blazey, Harrison B. Prosper
C-   Updated  17-MAR-1994   Chip Stewart  - replaced CACL E MIN WITH ETSEED 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Link area
      INCLUDE 'D0$INC:CLUPAR.INC'       ! Contains ILO, IHI and NREP
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C
      INTEGER HADRONIC
      PARAMETER( HADRONIC = 2 )         ! Hadronic + Em
C
      INTEGER TOWERI,TOWERJ,DELETA,DELPHI,IER,GZCATE
      REAL ETMIN,ETSEED
      LOGICAL DOENERGY
C
      INTEGER IETMIN,IDOENERGY,IETSEED
      EQUIVALENCE ( IETMIN, ETMIN )
      EQUIVALENCE ( IDOENERGY, DOENERGY )
      EQUIVALENCE ( IETSEED, ETSEED )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,DELETA,DELPHI,DOENERGY
C----------------------------------------------------------------------
C
      IDOENERGY = ALG_PARAMS(3)    ! DO_NEIGHBOR_ENERGY  T=E,F=ET in CNEIGH_CATE
      DELETA = ALG_PARAMS(4)       ! ETA NEIGHBOR LIMIT  
      DELPHI = ALG_PARAMS(5)       ! PHI NEIGHBOR LIMIT 
      IETMIN = ALG_PARAMS(6)       ! MINIMUM TOWER ET  (all CATE towers)
      IETSEED = ALG_PARAMS(7)      ! MINIMUM SEED ET/E (for connection logic)
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK ('CALEVT_RCP')      ! Select CALEVT SRCP bank
        CALL EZGET('CATE_CLASS_OFFSET',ICLASS,IER)
        CALL EZGET('CATE_NEXT_OFFSET',INEXT,IER)
        CALL EZRSET
      ENDIF
C
C ****  Check for CATE bank
C
      IF ( LCATE .LE. 0 ) THEN
        LCATE = GZCATE()
        IF ( LCATE .LE. 0 ) THEN
          CALL ERRMSG ('CAJETS','CJET_PRECLUSTER','NO CATE bank','W')
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Initialize some words in CLUPAR.INC; needed by CNEIGH_CATE
C
      NREP   = IQ(LCATE+2)                      ! Repetition number
      CALL GTCATE_TOTAL(NTOWER,NEMTWR,IER)
      ILO    = NEMTWR + 1                       ! First tower
C
      CALL GTCATE_MIN_TOWER(HADRONIC,ETMIN,IHI)          ! gives last tower
                                        ! corresponding to min energy
C
C ****  Make each tower into single-tower clusters
C
      CALL CONNECT_INIT (IQ(LCATE+1),NREP,ICLASS,INEXT,ILO,IHI)
C
C ****  Loop over towers in CATE bank
C
      DO TOWERI = ILO,IHI
C
C ****  Select a neighboring tower, TOWERJ, of TOWERI
C ****  with the highest energy or Et.
C
        CALL CNEIGH_CATE 
     &    (HADRONIC,DOENERGY,ETSEED,DELETA,DELPHI,TOWERI,TOWERJ)
C
C ****  If a suitable neighbor found then connect it to TOWERI
C
        IF(TOWERJ.NE.0) CALL CONNECT (IQ(LCATE+1),TOWERJ,TOWERI)
C
      ENDDO
C
  999 RETURN
      END
