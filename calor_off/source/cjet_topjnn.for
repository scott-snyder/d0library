      SUBROUTINE CJET_TOPJNN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : First step of TOPJ ALGORITHM - apply
C-                         Nearest Neighbor on CATE to form CACLs. 
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: CAJETS_RCP
C-
C-   Created 7-JUN-1993   Chip Stewart
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CATENM.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Link area
      INCLUDE 'D0$INC:CLUPAR.INC'       ! Contains ILO, IHI and NREP
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
C
C
      INTEGER IER,GZCATE,IDELTA_R,I,J,K,L,N,IETA,IPHI,DELPHI,DELETA
      INTEGER LAYER(17),NLAYER,IMIN,JMIN,IETMIN,CLASSI,CLASSJ,CATI,IMAX
      INTEGER CATJ,ETAI,ETAJ,PHII,PHIJ
      REAL ETMIN,ETAJM,ETAIM,ETOWJ,EMAX,DELTA_R,E(7)
      LOGICAL ETFLAG
C
      EQUIVALENCE ( IDELTA_R, DELTA_R )
      EQUIVALENCE ( IETMIN  , ETMIN )
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,ETMIN
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK ('CALEVT_RCP')      ! Select CALEVT SRCP bank
        CALL EZGET('CATE_CLASS_OFFSET',ICLASS,IER)
        CALL EZGET('CATE_NEXT_OFFSET',INEXT,IER)
        CALL EZRSET
      ENDIF
      IDELTA_R = ALG_PARAMS(2)   ! DELTA R FOR NEAREST NEIGHBOR (REAL)
      ETFLAG   = ALG_PARAMS(3)   ! NEIGHBOR ENERGY OR ET (TRUE=ET)
      IETMIN   = ALG_PARAMS(4)   ! ET MIN FROM CATE (REAL)
C
C ****  Check for CATE bank
C
      LCATE = GZCATE()
      IF ( LCATE .LE. 0 ) THEN
        CALL ERRMSG ('CAJETS','CJET_TOPJCLUSTER','NO CATE bank','W')
        GOTO 999
      ENDIF
C
C ****  Initialize some words in CLUPAR.INC; needed by CNEIGH_CATE
C
      NREP   = IQ(LCATE+2)                      ! Repetition number
      CALL GTCATE_TOTAL(NTOWER,NEMTWR,IER)
      ILO    = NEMTWR + 1                       ! First tower
      CALL GTCATE_MIN_TOWER(2,ETMIN,IHI)        ! last tower with min energy
C
C ****  Make each tower into single-tower clusters
C
      CALL CONNECT_INIT (IQ(LCATE+1),NREP,ICLASS,INEXT,ILO,IHI)
C
C ****  Loop over towers in CATE bank
C
      DO 10, I = ILO,IHI
        CALL GTCATE(I,IETA,IPHI,LAYER,NLAYER,E,IER)
        CATI = LCATE+NREP*(I-1)                ! Base pointer into CATE bank
        CLASSI = IQ(CATI+15)                   ! Class number of tower I
        ETAI = IQ(CATI+12)
        ETAIM=ETAI-SIGN(0.5,FLOAT(ETAI))       ! Solve the displaced zero
        PHII = IQ(CATI+13)
        EMAX = 0.
        IMAX = 0
C
        DELETA= DELTA_R
        DO 100 IETA = -DELETA,DELETA
          ETAJM = ETAIM +IETA
          ETAJ  = ETAJM+SIGN(0.5,ETAJM)   ! This gives n.neighbor across 0
          IF(IABS(ETAJ).GT.NETAL)GO TO 100
C
          DELPHI = DELTA_R       ! NO- IF(IABS(ETAJ).GE.33) DELPHI = 2*DELTA_R
C
          DO 200 IPHI = -DELPHI,DELPHI
            IF(IETA.EQ.0.AND.IPHI.EQ.0)GO TO 200         ! SKIP OVER ITSELF
            IF(IETA*IETA+IPHI*IPHI.GT.DELTA_R*DELTA_R)GO TO 200  ! CUT CORNER?
            PHIJ = PHII + IPHI
            IF(PHIJ.LE.0)PHIJ=NPHIL+PHIJ   ! wrap-around
            IF(PHIJ.GE.NPHIL+1)PHIJ=PHIJ-NPHIL
C
            J = PTCATE(ETAJ,PHIJ,2)    ! Get tower number of neighboring
            IF(J.EQ.0)GO TO 200           ! tower
C
            CATJ = LCATE+NREP*(J-1)       ! Base pointer into CATE bank
            CLASSJ = IQ(CATJ+15)          ! Class number of tower J
C
C ****  IGNORING TOWERS WHICH ARE ALREADY CONNECTED
C
            IF ( CLASSJ.EQ.CLASSI ) GOTO 200
C
            ETOWJ = Q(CATJ+7)
            IF(ETFLAG)ETOWJ = Q(CATJ+8)          ! ET IF NOT ENERGY
            IF(ETOWJ.GT.EMAX)THEN
              EMAX = ETOWJ
              IMAX = J                    ! NEIGHBOR WITH MAXIMUM ENERGY
            ENDIF
  200     CONTINUE
  100   CONTINUE
C
C ****  Apply Minimum Et/Energy cut
C
        IF ( EMAX .GE. ETMIN ) CALL CONNECT (IQ(LCATE+1),I,IMAX)
   10   CONTINUE
C
  999 RETURN
      END
