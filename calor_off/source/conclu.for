      SUBROUTINE CONCLU(MXPREC,NPRECL,PRECLU,IPRECL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO MAKE JET CLUSTERS USING FIXED CONE
C-                         ALGORITHM
C-
C-   Inputs  : PRECLUSTERS IN PRECLU BLOCK
C-             NPRECL = NUMBER OF PRECLUSTERS
C-             MXPREC = MAXIMUM NUMBER OF PRECLUSTERS
C-   Outputs : JETS IN JET BANK
C-
C-   ENTRY CONCLI
C-    Get parameters from CAJETS_RCP
C-
C-   Created  13-APR-1989   Nicholas Hadley
C-   Updated   2-OCT-1990   Chip Stewart  - MODIFIED RCP INPUT
C-   Updated  19-NOV-1991   Nick Hadley, Boaz Klima
C-     Fill JETS instead of JTSH
C-   Updated  17-MAY-1993   Harrison B. Prosper
C-    Add full error matrix; add ZLINKC; use PI.DEF
C-   Updated   2-JUN-1993   Harrison B. Prosper
C-    Fix JPTS pointer bug
C-   Updated   3-JUN-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-    Check dropped bit
C-   Updated  30-JUL-1993   Qizhong Li-Demarteau    fixed bad PHIST case
C-                            and fixed bug in IETALO,TETAHI calculation
C-   Optimized 31-OCT-1993  Richard Astur - added numerous speedup options
C-                          which are hardwired to be ACTIVE, but could be
C-                          turned off for studies, see parameters declared
C-                          below.
C-   Modified 10-OCT-1994   Bug fix. Make share array larger(4000). Als
C-                          ignore large negative towers. See ETOW_NEGDR
C-   Updated   2-FEB-1995   Bob Hirosky  - save Et of Seed Tower and Precluster,
C-                                         calculate underlying/noise energy
C-                                         storing ICR part separately
C-   Updated  18-SEP-1995   Bob Hirosky/Brad Abbott Default definition of
C-                                eta and phi now Snowmass, D0 angles
C-                                selected by choosing negative cone size
C-                                fill word 41 for ICR underlying noise
C-                                change UND/ZSP word fill to 'AREA ONLY'
C-   Updated  18-OCT-1995   Bob Hirosky - use nicer area calc routines
C-                                      - get Zvertex only once per call
C-   Updated   2-NOV-1995   Bob Hirosky - bug fix in getting Zvertex 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:ISTAT_DROP.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CJET_ALGORITHM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INCLUDE 'D0$LINKS:IZJPTS.LINK'
      INCLUDE 'D0$INC:CON_AREA.INC'
C----------------------------------------------------------------------
C: Optimization parameters used
      REAL ETOW_NEGDROP
      PARAMETER( ETOW_NEGDROP = -1.0)   ! Drop these towers
      LOGICAL ETA_LIMITS        ! Strict calculation of ieta,iphi limits
      LOGICAL KILL_FAR_CLUSTERS ! Dont consider preclusters close to jet
      REAL        FAR_RATIO     ! Kill if precl. is within FAR_RATION*CONE_SIZE
      LOGICAL INCREASE_DELTA_R  ! Use different jet axis convernce requirement
      REAL        DELTA_R_2
      LOGICAL JET_ET_MIN_ON_ITER ! Skip if jet ET is below some value
C                                 ! on any iteration
      REAL    ET_MIN_RATIO      ! Fraction of jet ET cut we must see/iteration
      PARAMETER( ETA_LIMITS = .TRUE. )
      PARAMETER( KILL_FAR_CLUSTERS = .TRUE. )
      PARAMETER( FAR_RATIO = .5 )
      PARAMETER( INCREASE_DELTA_R  = .TRUE. )
      PARAMETER( DELTA_R_2 = .01 )
      PARAMETER( JET_ET_MIN_ON_ITER = .TRUE. )
      PARAMETER( ET_MIN_RATIO = .5 )
C
      REAL INFO(3,2), ZVERTEX
      INTEGER NZRVA, IHOLD
      REAL RETA1, RETA2, RPHI1, RPHI2, DELTA_R, PIE
      PARAMETER( PIE = PI )
      INTEGER NJET_FOUND, IRVA, MAX_FOUND
      PARAMETER( MAX_FOUND = 50 )
      REAL JET_FOUND(3, MAX_FOUND )      ! keep track of jets found
      REAL PCLUS_SEP_CUT, RCUT
      LOGICAL OK
C
      INTEGER IER
      REAL AREA,AREA_STHW,AREA_ICR,AREA_ICR_STHW !AREA CALC WORDS
      INTEGER N_VERTEX
      LOGICAL VERTEX_OK
      REAL ETSEED,ETPRECL,VTX_INFO(3,1),ETADET,ZVTX,ETAD
C--------------------------------------------------------
      INTEGER NPRECL, MXPREC
      INTEGER IPRECL(10,MXPREC)
      REAL PRECLU(10,MXPREC)
      INTEGER MAXPTS
      PARAMETER( MAXPTS = 4000 )
      INTEGER  JPOINT(MAXPTS), TOWPNT(MAXPTS)
      REAL CONRAD, CN2RAD, THETA, ETSHARE(50)
      REAL JETMNE,PHITMP,ETATMP
      REAL PHISUM, ETASUM, ETSUM, ETA2SU, PHI2SU, THETH
      REAL PHINEW, PHIWID, ETAST, PHIST, PHIPHY, ETAPHY
      REAL THETANEW
      REAL R2TMP, EMESUM, RDIFF, SIG2XS, SIG2YS
      REAL EXSUM, EYSUM, EZSUM, ESUM, ETANEW, ETAWID, E(4), ETAOFF
      REAL ESUMV(4)
      EQUIVALENCE ( ESUMV(1), EXSUM )
      EQUIVALENCE ( ESUMV(2), EYSUM )
      EQUIVALENCE ( ESUMV(3), EZSUM )
      EQUIVALENCE ( ESUMV(4), ESUM )
      INTEGER IGOOD, IETAHI, IETALO
      INTEGER IPHILO, IPHIHI, JJ, I, J , MM, JPHI
      INTEGER GZJETS, NUMJET, NN, NTOWER
      INTEGER POINT, POINEM, GZCATE, NREP
      INTEGER GZCAPH, GZCAEH, IVERS, NJPTS
      INTEGER JETSHA(50), LL, ISHARE, II, NSHARE, JETMAX
      LOGICAL SHARE
C
      INTEGER ICONRAD,IJETMNE
      EQUIVALENCE ( ICONRAD , CONRAD  )
      EQUIVALENCE ( IJETMNE , JETMNE )
      LOGICAL D0_ANGLES
C: Statement function to determine delta r in eta-phi
      DELTA_R(RETA1,RETA2,RPHI1,RPHI2) = SQRT( ((RETA1)-(RETA2))**2 +
     &  MIN( MOD(ABS((RPHI1)-(RPHI2)),2*PIE) ,
     &  2*PIE-ABS(MOD(ABS((RPHI1)-(RPHI2)),2*PIE)) )**2 )
C----------------------------------------------------------------------
C
C
C            LOOP OVER ALL PRECLUSTERS AND MAKE CLUSTERS
C
C       FIRST REMOVE ALL JETS FROM THIS EVENT/ALGORITHM
C
      LJETS = GZJETS()
      IF (LJETS.GT.0) CALL MZDROP(IXMAIN,LJETS,'L')
C
C: Initialize and set some parameters
C
      D0_ANGLES = (CONRAD.LT.0.0)
      CONRAD = ABS(CONRAD)
      RCUT = .001**2                                ! Old convergence number
      IF ( INCREASE_DELTA_R ) RCUT = DELTA_R_2**2   ! New number
      NUMJET = 0
      PCLUS_SEP_CUT = CONRAD*FAR_RATIO              ! See above
      NJET_FOUND = 0                                ! # of jets found so far
      NUMJET = 0
C                                                   !get vertex
      CALL VERTEX_INFO( 1, NZRVA, INFO, OK )
      IF ( OK ) THEN
        ZVERTEX = INFO(1,1)
      ELSE
        ZVERTEX = 0.0
      ENDIF
C
      DO 490 JJ = 1, NPRECL
        IF(JJ.GE.MXPREC) RETURN
        LCATE = GZCATE()
        NREP = IQ(LCATE+2)
        PHISUM = 0.
        ETASUM = 0.
        ETSUM = 0.
        ETA2SU = 0.
        PHI2SU = 0.
C
C                GET PRECLUSTER CENTROID
C
        DO 10 I = 1 ,4
          E(I) = PRECLU(2+I,JJ)
   10   CONTINUE
        CALL ETOETA(E,PHIST,THETA,ETAST)
        ETAOFF = PRECLU(1,JJ)- INT(ETAST*10.)
C
C: CHECK TO SEE IF PRECLUSTER IS TOO CLOSE TO A FOUND JET
C
        IF ( KILL_FAR_CLUSTERS ) THEN
          DO IRVA = 1, NJET_FOUND
            IF ( DELTA_R( JET_FOUND(2,IRVA), ETAST, JET_FOUND(3,IRVA),
     &        PHIST ) .LT. PCLUS_SEP_CUT ) GOTO 490
          ENDDO
        ENDIF
C
C         START OF CONE BUILDING LOOP
C
        DO 60 MM = 1, 50
          EMESUM = 0.
          IGOOD = 0
          NTOWER = 0
          PHISUM = 0.
          ETASUM = 0.
          ETSUM = 0.
          EXSUM = 0.
          EYSUM = 0.
          EZSUM = 0.
          ESUM = 0.
          ETA2SU = 0.
          PHI2SU = 0.
          SIG2XS = 0.
          SIG2YS = 0.
          DO 15 I = 1, 50
            ETSHARE(I) = 0.
            JETSHA(I) = 0
   15     CONTINUE
          NSHARE = 0
          SHARE = .FALSE.
C
C               CALCULATE ETA, PHI LIMITS FOR CONE
C
          IF ( .NOT. ETA_LIMITS ) THEN
            IETAHI = INT(MAX(0.,ETAOFF)+(ETAST+CONRAD)*10.) + 3
            IETAHI = MIN(NETAL,IETAHI)
            IETALO = INT(MIN(0.,ETAOFF)+(ETAST-CONRAD)*10.) - 3
            IETALO = MAX(-NETAL,IETALO)
            IPHIHI = INT((PHIST+CONRAD)*64./TWOPI) + 2
            IPHILO = INT((PHIST-CONRAD)*64./TWOPI) - 2
          ELSE
            CALL IETA_LIMITS( ETAST, PHIST, CONRAD, ZVERTEX, IETAHI,
     &        IETALO, IPHIHI, IPHILO )
          ENDIF
C
C               LOOP OVER THE POSSIBLE TOWERS FOR THIS CONE
C
          DO 50 I = IETALO , IETAHI
            DO 40 J = IPHILO , IPHIHI
              JPHI = J
              IF (J.GE.65) JPHI = MOD(J,64)
              IF (J.LE.0)  JPHI = 64 + J
              POINT = PTCATE(I,JPHI,2)
              IF (POINT.EQ.0) GOTO 40           ! NO TOWER
              POINT = NREP*(POINT-1) + LCATE
              IF ( Q(POINT+8) .LE. ETOW_NEGDROP ) GOTO 40 ! SKIP NEG TOWER
              DO 20 II = 1 ,4
                E(II) = Q(POINT+3+II)
   20         CONTINUE
              CALL ETOETA(E,PHITMP,THETA,ETATMP)
              IF (ABS(PHITMP-PHIST).GT.(TWOPI-ABS(PHITMP-PHIST))) THEN
                IF(PHIST.LT.PHITMP) THEN
                  PHITMP = PHITMP - TWOPI
                ELSE
                  PHITMP = PHITMP + TWOPI
                END IF
              END IF
              R2TMP = (ETATMP-ETAST)**2 + (PHITMP-PHIST)**2
              IF (R2TMP.GT.CN2RAD) GOTO 40     !POINT OUTSIDE CONE
              NTOWER = NTOWER + 1
              TOWPNT(NTOWER) = POINT
C
              IF (IQ(POINT+16).EQ.0) THEN       ! TOWER NOT SHARED
                ISHARE = 1
                IQ(POINT+16) = NUMJET +1
              ELSE                  ! THIS TOWER IS ALSO IN ANOTHER JET
                SHARE = .TRUE.
                ISHARE = -1
                DO 30 II =1  , NSHARE
                  IF(JETSHA(II).EQ.IQ(POINT+16)) GOTO 35
   30           CONTINUE
                NSHARE = MIN(50,NSHARE + 1)
                II = NSHARE
                JETSHA(NSHARE) = IQ(POINT+16)
   35           IQ(POINT+16) = IQ(POINT+16) + 1000*(NUMJET+1)
                ETSHARE(II) = ETSHARE(II)+ Q(POINT+8)
              END IF
C
C       SAVE POINTERS TO CELLS IN THIS JET FOR JPTS
C
              DO 45 NN = 1 , NLYRL
C                IF ( JBIT(IQ(POINT+17),NN) .NE.0 ) THEN
C 9/1/94 Try to not use JBIT (RVA from suggestion by H. Greenlee )
C
                IHOLD = ( (IQ(POINT+17)/(2**(NN-1))) )
                IF ( 2*(IHOLD/2) .NE. IHOLD ) THEN
                  IGOOD = IGOOD + 1
                  IF (IGOOD.GT.MAXPTS) IGOOD = MAXPTS
                  JPOINT(IGOOD) = ISHARE*PTCAEP(I,JPHI,NN)
                END IF
   45         CONTINUE
C
              ETASUM = ETATMP*Q(POINT+8) + ETASUM
              PHISUM = PHITMP*Q(POINT+8) + PHISUM
              ETA2SU = ETATMP*ETATMP*Q(POINT+8) + ETA2SU
              PHI2SU = PHITMP*PHITMP*Q(POINT+8) + PHI2SU
              ETSUM = Q(POINT+8) + ETSUM
              EXSUM = Q(POINT+4) + EXSUM
              EYSUM = Q(POINT+5) + EYSUM
              EZSUM = Q(POINT+6) + EZSUM
              ESUM =  Q(POINT+7) + ESUM
              SIG2XS = Q(POINT+9) + SIG2XS
              SIG2YS = Q(POINT+10) + SIG2YS
              POINEM = PTCATE(I,JPHI,1)         ! CALCULATE EM ET
              IF (POINEM.GT.0) THEN
                POINEM = (POINEM-1)*NREP + LCATE
                EMESUM = Q(POINEM+8) + EMESUM
              END IF
   40       CONTINUE
   50     CONTINUE
          IF(ETSUM.LE.0) GOTO 70        ! protect against divide by 0
          ETANEW = ETASUM/ETSUM
          PHINEW = PHISUM/ETSUM
C          CALL ETOETA(ESUMV, PHINEW, THETANEW, ETANEW )
          RDIFF = (ETANEW-ETAST)**2 + (PHINEW-PHIST)**2
C
C: Require some minimum ET for this cluster on EVERY iteration
C
          IF ( JET_ET_MIN_ON_ITER ) THEN
            IF ( ETSUM .LT. JETMNE*ET_MIN_RATIO ) GOTO 70
          ENDIF
          IF (RDIFF.GE. RCUT .AND. MM .LT. 50) THEN    ! CENTROID NOT STABLE YET
            ETAST = ETANEW
            PHIST = PHINEW
C
            DO 55 LL = 1, NTOWER        ! CLEAR JET POINTER IN CATE
              IF (IQ(TOWPNT(LL)+16).EQ.NUMJET+1) THEN
                IQ(TOWPNT(LL)+16) = 0
              ELSE
                IQ(TOWPNT(LL)+16) = IQ(TOWPNT(LL)+16) - 1000*(NUMJET+1)
              END IF
   55       CONTINUE
            IF (PHIST.GT.TWOPI) PHIST = PHIST - TWOPI
            IF (PHIST.LT.0.)  PHIST = PHIST + TWOPI
            IF (PHIST .GT. TWOPI .OR. PHIST .LT. 0.) THEN
              ETSUM = 0.0     ! reset ETSUM to 0 when PHIST is illegal
              CALL ERRMSG('CAJETS','CONCLU',
     &          'Illegal jet PHI, ignore the jet ','W')
              GOTO 70
            ENDIF
C
          ELSE
            GOTO 70                     ! CONE STABLE, STORE JET
          END IF
   60   CONTINUE
   70   IF (ETSUM.LT.JETMNE) THEN       !NOT ENOUGH ET CLEAN UP
          DO 80 LL = 1, NTOWER        ! CLEAR JET POINTER IN CATE
            IF(IQ(TOWPNT(LL)+16).EQ.NUMJET+1+(NUMJET+1)*1000) THEN
              IQ(TOWPNT(LL)+16) = 0
            END IF
            IF (IQ(TOWPNT(LL)+16).EQ.NUMJET+1) THEN
              IQ(TOWPNT(LL)+16) = 0
            ELSE
              IQ(TOWPNT(LL)+16) = IQ(TOWPNT(LL)+16) - 1000*(NUMJET+1)
            END IF
   80     CONTINUE
          GOTO 490   ! NOT ENOUGH ET IGNORE JET
        END IF
        NUMJET = NUMJET + 1
        ETAWID = ETA2SU/ETSUM
        PHIWID = PHI2SU/ETSUM
        ETAWID = ETAWID-ETANEW**2
        ETAWID = SQRT(ABS(ETAWID))
        PHIWID = PHIWID - PHINEW**2
        PHIWID = SQRT(ABS(PHIWID))
        THETA = SQRT(EXSUM**2+EYSUM**2)
        THETA= ATAN2(THETA,EZSUM)
        PHIPHY = ATAN2(EYSUM,EXSUM)
        IF (PHIPHY.LT.0.) PHIPHY = PHIPHY + TWOPI
        THETH = 0.5*ABS(THETA)
        ETAPHY  = LOG(1./TAN(THETH))
        ETSEED  = PRECLU(9,JJ)
        ETPRECL = PRECLU(7,JJ)
C
C
  420   CALL BKJETS(LJETS)
        Q(LJETS+2)  = EXSUM
        Q(LJETS+3)  = EYSUM
        Q(LJETS+4)  = EZSUM
        Q(LJETS+5)  = ESUM
        Q(LJETS+6)  = ETSUM
C
C ****  Choose D0 or Snowmass Angles
C
        IF (D0_ANGLES) THEN
          Q(LJETS+7)  = THETA
          Q(LJETS+8)  = PHIPHY
          Q(LJETS+9)  = ETAPHY
        ELSE
          Q(LJETS+7)=2.*ATAN( EXP( -ETANEW) )
          Q(LJETS+8)=PHINEW
          Q(LJETS+9)=ETANEW
        ENDIF

C
C           FILL THE JPTS BANK
C
        CALL BKJPTS(LJETS,IGOOD,LJPTS)
        IQ(LJPTS+2) = IGOOD
        DO 430 I = 1, IGOOD
          IQ(LJPTS+2+I) = JPOINT(I)
  430   CONTINUE
C
C ****  Fill JETS instead of JTSH
C
        Q(LJETS+12) = ETAWID
        Q(LJETS+13) = PHIWID
        Q(LJETS+14) = EMESUM/ETSUM
        IQ(LJETS+15) = 0
C
C ****  SAVE ET of SEED / PRECL
C
        Q(LJETS+36) = ETSEED
        Q(LJETS+39) = ETPRECL
C
C ****  FIND DETECTOR ETA OF JET
C
        ETADET = ETAD(ZVERTEX,THETA)
C
C ****  ENERGY CORRECTION STATUS WORD
C
        IQ(LJETS+26) = 0
C
C ****   GET JET AREA FOR UNDERLYING EVENT AND NOISE CORRECTIONS
C
        CONE_USED = CONRAD
        CALL JET_CON_AREA(CONE_USED,ETADET,AREA,AREA_STHW,
     &    AREA_ICR, AREA_ICR_STHW)
        Q (LJETS+30) = AREA_STHW
        Q (LJETS+31) = AREA
        Q (LJETS+38) = AREA_ICR
        Q (LJETS+41) = AREA_ICR_STHW
C
C: Flag this jet as having the noise/underlying event area calculated
C
        CALL SET_JETS_BANK_CORRECTED(LJETS, 'ARA', IER )
C
C***  SPLIT THE JETS, BUT FIRST STORE EACH FOUND CONE
C
        IF (NUMJET.EQ.1) NUM_CONES=0 ! FIRST JET INIT ETA/PHI ARRAYS
        NUM_CONES = NUM_CONES+1
        CONE_ARRY(NUM_CONES,1) = ETADET
        CONE_ARRY(NUM_CONES,2) = PHIPHY
        CONE_ARRY(NUM_CONES,3) = NUMJET  ! ASSOCIATE CONE W/ A JET
C
        IF(SHARE) THEN
          JETMAX = 1
          ETSUM = 0.
          DO 440 I = 1 , NSHARE
            ETSUM = ETSHARE(I) + ETSUM
            IF ( ETSHARE(I).GT.ETSHARE(JETMAX) ) THEN
              JETMAX = I
            END IF
  440     CONTINUE
          CALL SPLJET(NUMJET,JETSHA(JETMAX),ETSUM)
        END IF
C
C ****  Bank may have been dropped, so check dropped bit
C
        IF ( IAND(IQ(LJETS),ISTAT_DROP) .NE. 0 ) THEN
          GOTO 490
        ENDIF
C
C ****  Compute full error matrix
C
        Q(LJETS+10) = 0.0   !SigEx**2
        Q(LJETS+11) = 0.0   !SigEy**2
        Q(LJETS+22) = 0.0   !SigEz**2
        Q(LJETS+23) = 0.0   !<dExdEy>
        Q(LJETS+24) = 0.0   !<dExdEz>
        Q(LJETS+25) = 0.0   !<dEydEz>
C
        LCAEH = GZCAEH()
        IF ( LCAEH .GT. 0 ) THEN
          LJPTS = LQ(LJETS-IZJPTS)
          IF ( LJPTS .GT. 0 ) THEN
            NJPTS = IQ(LJPTS+2) ! Get number of cells (may have changed)
            IVERS = IQ(LCAEH+1) ! Get version number
            NREP  = IQ(LCAEH+2)
C
            DO  I = 1 , NJPTS
              J = IQ(LJPTS+2+I)                         ! Cell Number
              POINT = LCAEH + NREP*(J-1)
C
              Q(LJETS+10) = Q(LJETS+10) + Q(POINT+9)    !sig(Ex)**2
              Q(LJETS+11) = Q(LJETS+11) + Q(POINT+10)   !sig(Ey)**2
C
              IF ( IVERS .GE. 3 ) THEN
                Q(LJETS+22) = Q(LJETS+22) + Q(POINT+17)   !sig(Ez)**2
                Q(LJETS+23) = Q(LJETS+23) + Q(POINT+18)   !<dExdEy>
                Q(LJETS+24) = Q(LJETS+24) + Q(POINT+19)   !<dExdEz>
                Q(LJETS+25) = Q(LJETS+25) + Q(POINT+20)   !<dEydEz>
              ENDIF
            ENDDO
          ELSE
            CALL ERRMSG('NO_JPTS','CONCLU',' No JPTS pointer bank','W')
          ENDIF
        ELSE
          CALL ERRMSG('NO_CAEH','CONCLU',' LCAEH = 0','W')
        ENDIF
C
C ****  Keep track of what jets are found
C
        NJET_FOUND = MIN( NJET_FOUND + 1, MAX_FOUND )
        JET_FOUND(1, NJET_FOUND ) = Q( LJETS + 6 )
        JET_FOUND(2, NJET_FOUND ) = Q( LJETS + 9 )
        JET_FOUND(3, NJET_FOUND ) = Q( LJETS + 8 )

  490 CONTINUE
C
      LCAPH = GZCAPH()
      IQ(LCAPH+3) = NUMJET
C***      CALL PRCAPH(55,LCAPH,NCAPHI,'A',IFL)
C
  999 RETURN
C
      ENTRY CONCLI
C
C                 get constants out of CJET_PARAMS common
C
      ICONRAD = ALG_PARAMS(2)
      IJETMNE = ALG_PARAMS(3)
      CN2RAD=CONRAD**2
C
      RETURN
      END
