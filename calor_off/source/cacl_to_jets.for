      SUBROUTINE CACL_TO_JETS (ETMIN,NJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transform CACL pre-clusters into JETS banks.
C-   Use the Cluster map bank CMAP to map pre-clusters into jets via the
C-   CLASS and NEXT words in CMAP.
C-
C-   Inputs  : ETMIN    [R]     Minimum JET Et
C-   Outputs : NJETS    [I]     Number of Jets found
C-   Controls: NONE
C-
C-   Created  11-OCT-1989   Chip Stewart
C-   Modified 16-OCT-1989   Gerald C. Blazey
C-   Updated  12-DEC-1989   Harrison B. Prosper
C-      Added ICLUST,ICLASS,INEXT,IREPEAT arguments
C-   Updated   3-JAN-1990   Boaz Klima  -  Apply ETMIN cut
C-   Updated  11-JAN-1990   Harrison B. Prosper
C-      ETMIN is now the only argument. Now uses CMAP.
C-   Updated   5-March-1990 NJ Hadley  fill JTSH bank
C-   Updated  10-OCT-1990   Boaz Klima   Chip Stewart - fill CAPH
C-   Updated  19-NOV-1991   Nick Hadley, Boaz Klima
C-     Fill JETS instead of JTSH
C-   Updated  17-MAY-1993   Harrison B. Prosper - Add full error matrix
C-   Updated   8-JUN-1993   Chip Stewart - Word 15 = number of CACLs
C-   Updated  17-MAY-1993   Harrison B. Prosper - Add full error matrix
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    ETMIN
      INTEGER NJETS
C
      INTEGER IVERS,NCACL,IREPEAT,ICLASS,INEXT,LCLASS,LNEXT,ICLUST
C
      INTEGER NJET_MAX,CELLS_MAX
      PARAMETER (NJET_MAX=20)
      PARAMETER (CELLS_MAX=2500)
      INTEGER CELLS(CELLS_MAX),ICELL
C
      INTEGER NCACH,NJPTS,NCL
C
      INTEGER GZCACL,GZCMAP, GZCAEH
C
      INTEGER ICACL,JCACL,KCACL,I,J,K,L,IER
      REAL AVE_ETA,ETSUM,AVE_PHI,DEV_ETA,DEV_PHI,AVE_THETA
C
      INTEGER CLASS,NEXT,CATW
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'       ! Link area
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C
      REAL ETEM, ETASUM, ETA2SU, PHISUM, PHI2SU,PHIJ
      INTEGER POINT, NREP, II
      REAL E(4), PHITMP, THETA, ETATMP, ETAWID, PHIWID
C
      LOGICAL FIRST,LOOP
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Define statement functions class,next
C
      CATW(J)  = LCMAP + (J-1)*IREPEAT
      CLASS(J) = CATW(J)+ICLASS
      NEXT(J)  = CATW(J)+INEXT
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
      ENDIF
C
      NJETS = 0
C
C ****  GET LINK TO START OF CACL BANKs.
C
      LCACL = GZCACL()
      IF ( LCACL .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'NO CACL BANK','W')
        GOTO 999
      END IF
C
C ****  Get address of CMAP
C
      LCMAP = GZCMAP()
      IF ( LCMAP .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'NO CMAP BANK','W')
        GOTO 999
      ENDIF
C
C ****  Get number of pre-clusters etc.
C
      CALL GTCMAP_TOTAL (IVERS,NCACL,IREPEAT,ICLASS,INEXT,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &    'Problem in GTCMAP; NO JETS produced','W')
        GOTO 999
      ENDIF
C
C ****  LOOP OVER ALL CACL BANKS IN CMAP
C
      DO 100 ICACL = 1, NCACL
C
C ****  Loop over class starting at the element with next equal to
C       class.  Leave inner loop when that element is re-encountered.
C
        IF( IQ(CLASS(ICACL)) .NE. IQ(NEXT(ICACL)) ) GOTO 100
        JCACL = ICACL
C
C ****   NEW JET
C ****   Book JETS bank and drop it if JET is below ETMIN threshold
C
        NJETS = NJETS + 1
        CALL BKJETS(LJETS)
C
C ****  Zero words for summing
C
        CALL VZERO(Q(LJETS+2),9)          ! Zero words for summing.
        NJPTS   = 0
        NCL     = 0
C
C *************************************
C ****  LOOP OVER a class of CACL banks
C *************************************
C
        LOOP = .TRUE.
        DO WHILE ( LOOP )
C
C ****  Get address of CACL bank JCACL from CMAP reference link
C
          CALL GTCMAP (JCACL,LCLASS,LNEXT,LCACL,IER)
C
          IF ( LCACL .LE. 0 ) THEN
            CALL ERRMSG('CALORIMETER','CACL_TO_JETS',
     &        'GTCMAP Returns LCACL = 0','W')
            GOTO 100
          END IF
C
C ****  ADD EX,EY,EZ,E TO BUILD JET
C
          DO I = 1, 4
            Q(LJETS+1+I) = Q(LJETS+1+I) + Q(LCACL+ 3 + I)
          END DO
C
C ****  Copy CAEP POINTERS FROM CACH TO temporary
C ****  array for JPTS bank.
C
          LCACH = LQ(LCACL-1)                   !CACH BANK ADDRESS
          DO ICELL =  NJPTS + 1 ,NJPTS + IQ ( LCACH + 2 )
            CELLS(ICELL)  =  IQ(LCACH + 2 + ICELL - NJPTS)
          END DO
          NJPTS = NJPTS + IQ ( LCACH + 2 )
C
C ****  GO BACK TO FIND NEXT CACL in current CLASS
C
          JCACL = IQ(NEXT(JCACL))
          LOOP  = IQ(NEXT(JCACL)) .NE. IQ(CLASS(JCACL))
          NCL = NCL + 1
        ENDDO
C
C *************************************
C ****  COMPUTE Et, theta, phi, eta of JET
C *************************************
C
        ETSUM = SQRT(Q(LJETS+2)*Q(LJETS+2)+Q(LJETS+3)*Q(LJETS+3))
        CALL ETOETA (Q(LJETS+2),AVE_PHI,AVE_THETA,AVE_ETA)
        Q(LJETS+6) = ETSUM
        Q(LJETS+7) = AVE_THETA
        Q(LJETS+8) = AVE_PHI
        Q(LJETS+9) = AVE_ETA
C
C ****  APPLY ETMIN CUT TO JET
C
        IF ( ETSUM.LT.ETMIN ) THEN
          CALL MZDROP(IXCOM,LJETS,' ')    ! Mark it for dropping
          NJETS = NJETS - 1
          GOTO 100
        END IF
C
C ****  FILL JPTS WITH CELLS IN JET
C
        CALL BKJPTS(LJETS,NJPTS,LJPTS)
        IQ(LJPTS+2) = NJPTS
        CALL UCOPY (CELLS(1),IQ(LJPTS + 3),NJPTS)
C
C ****  COMPUTE SOME SHAPE QUANTITIES BY LOOPING OVER
C ****  CELLS IN JET
C
        ETSUM = 0.
        ETEM = 0.
        ETASUM = 0.
        ETA2SU = 0.
        PHISUM = 0.
        PHI2SU = 0.
C
        LCAEH = GZCAEH()
        IF ( LCAEH .LE. 0 ) THEN
          CALL ERRMSG('NO_CAEH','CACL_TO_JETS',' LCAEH = 0','W')
          GOTO 100
        END IF
C
        Q(LJETS+10) = 0.0   !SigEx**2
        Q(LJETS+11) = 0.0   !SigEy**2
        Q(LJETS+22) = 0.0   !SigEz**2
        Q(LJETS+23) = 0.0   !<dExdEy>
        Q(LJETS+24) = 0.0   !<dExdEz>
        Q(LJETS+25) = 0.0   !<dEydEz>
C
        IVERS = IQ(LCAEH+1)  !Get Version number
        NREP  = IQ(LCAEH+2)
        DO 310 I = 1 , NJPTS
          POINT = LCAEH + NREP*(IQ(LJPTS+I+2)-1)
          DO 300 II = 1 ,4
            E(II) = Q(POINT+3+II)
  300     CONTINUE
          CALL ETOETA(E,PHITMP,THETA,ETATMP)
          PHIJ = AVE_PHI
          IF (ABS(PHITMP-PHIJ).GT.(TWOPI-ABS(PHITMP-PHIJ))) THEN
            IF(PHIJ.LT.PHITMP) THEN
              PHITMP = PHITMP - TWOPI
            ELSE
              PHITMP = PHITMP + TWOPI
            END IF
          END IF
          ETSUM  = Q(POINT+8) + ETSUM
          ETASUM = ETATMP*Q(POINT+8) + ETASUM
          PHISUM = PHITMP*Q(POINT+8) + PHISUM
          ETA2SU = ETATMP*ETATMP*Q(POINT+8) + ETA2SU
          PHI2SU = PHITMP*PHITMP*Q(POINT+8) + PHI2SU
          IF(IQ(POINT+14).LE.7) ETEM = ETEM + Q(POINT+8)
C
C ****  Compute full error matrix
C
          Q(LJETS+10) = Q(LJETS+10) + Q(POINT+9)    !sig(Ex)**2
          Q(LJETS+11) = Q(LJETS+11) + Q(POINT+10)   !sig(Ey)**2
C
C ****  Check version of CAEH bank
C
          IF ( IVERS .GE. 3 ) THEN
            Q(LJETS+22) = Q(LJETS+22) + Q(POINT+17)   !sig(Ez)**2
            Q(LJETS+23) = Q(LJETS+23) + Q(POINT+18)   !<dExdEy>
            Q(LJETS+24) = Q(LJETS+24) + Q(POINT+19)   !<dExdEz>
            Q(LJETS+25) = Q(LJETS+25) + Q(POINT+20)   !<dEydEz>
          ENDIF
C
  310   CONTINUE
C
        ETAWID = ETA2SU/ETSUM
        PHIWID = PHI2SU/ETSUM
        ETAWID = ETAWID-(ETASUM/ETSUM)**2
        ETAWID = SQRT(ABS(ETAWID))
        PHIWID = PHIWID-(PHISUM/ETSUM)**2
        PHIWID = SQRT(ABS(PHIWID))
C
C ****  Fill JETS instead of JTSH
C
        Q(LJETS+12) = ETAWID
        Q(LJETS+13) = PHIWID
        Q(LJETS+14) = ETEM/ETSUM
        IQ(LJETS+15)= NCL
C
  100 CONTINUE
C
      CALL CAPHFL_INT(K_CLUSTERS,NCACL)
      CALL CAPHFL_INT(K_JETS,NJETS)
C
C ****  ENERGY CORRECTION STATUS FLAG
C
      IQ(LJETS+26) = 0
  999 RETURN
      END
