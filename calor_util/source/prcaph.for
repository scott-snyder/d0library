      SUBROUTINE PRCAPH(PRUNIT,LCAPHI,NCAPHI,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump the contents of the CAPH bank. Use
C-   SET_CAPH to select a particular CAPH bank. For example,
C-
C-      CALL SET_CAPH('ELECTRON',0,IER)
C-
C-              :       :
C-
C-      CALL RESET_CAPH
C-
C-   Inputs  : PRUNIT   [I]     Unit number for printout
C-             LCAPHI   [I]     Bank address
C-             NCAPHI   [I]     Bank number
C-   Controls: CFL      [C*]    Flag to control printout
C-                              'HED' header only
C-             IFL      [I]     Not used
C-
C-   Created   2-MAY-1989   Serban D. Protopopescu
C-   Updated  18-JAN-1990   Harrison B. Prosper
C-      Add printout of new words
C-   Updated  10-OCT-1990   Chip Stewart, Boaz Klima   
C-      Added algorithm dependent print
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
C
      CHARACTER*4  PATH
      CHARACTER*10 ALGORITHM
      INTEGER PRUNIT,LCAPH,NCAPH,IFL,LCAPHI,NCAPHI,GZCAPH
      INTEGER LPROC,GZPROC
      INTEGER J,K,NFULL,NEMPTY
      CHARACTER CFL*(*)
      INTEGER NBANKS
      PARAMETER (NBANKS=2)
      CHARACTER*4 BLIST(NBANKS),EMPTY(NBANKS),FULL(NBANKS)
      DATA BLIST/'CACL','JETS'/
C--------------------------------------------------------------------
C
C--   check if bank exists
C
      LCAPH = LCAPHI
      IF ( LCAPH.EQ.0  ) THEN
        LCAPH = GZCAPH()
        IF ( LCAPH.LE.0 ) GOTO 99
      ENDIF
      IF ( CFL .EQ. 'ALL' ) THEN
        LPROC = GZPROC()
        LCAPH = LQ(LPROC-IZCAPH)
        IF ( LCAPH.LE.0 ) GOTO 99
      ENDIF
C
C--   Print header
C
      WRITE(PRUNIT,102)
C
      IF(CFL.EQ.'HED') GOTO 98
C
C     Give list of empty banks and banks with data
C
   11 CONTINUE
      NEMPTY=0
      NFULL=0
      DO 1 K=1,NBANKS
        IF(LQ(LCAPH-K).EQ.0) THEN
          NEMPTY=NEMPTY+1
          EMPTY(NEMPTY)=BLIST(K)
        ELSE
          NFULL=NFULL+1
          FULL(NFULL)=BLIST(K)
        ENDIF
  1   CONTINUE
C
      CALL PATHGT(PATH)                 ! Get Path
      WRITE(PRUNIT,121) PATH
      IF(NEMPTY.GT.0) WRITE(PRUNIT,103) (EMPTY(K),K=1,NEMPTY)
      IF(NFULL.GT.0)  WRITE(PRUNIT,104) (FULL(K),K=1,NFULL)
C
      WRITE(PRUNIT,105) IQ(LCAPH+K_BANK_VERSION)
C
      K = IQ(LCAPH+K_ALGORITHM)
      IF ( (K.GE.1) .AND. (K.LE.A_MAX) ) THEN
        ALGORITHM = A_NAME(K)
      ELSE
        ALGORITHM = '   UNKNOWN'
      ENDIF
C
      WRITE(PRUNIT,106)
     &  IQ(LCAPH+K_CLUSTERS),
     &  IQ(LCAPH+K_JETS),
     &  IQ(LCAPH+K_ALGORITHM),
     &  ALGORITHM,
     &  IQ(LCAPH+K_VERSION)
C
C ****  ALGORITHM DEPENDENT WORDS
C
      IF (K.EQ.2) THEN  ! CONE JET ALG
        WRITE(PRUNIT,108) (Q(LCAPH+K_BASE+J),J=1,7)
      ELSE IF (K.EQ.3) THEN  ! NN JET ALG
        WRITE(PRUNIT,110) (Q(LCAPH+K_BASE+J),J=1,10)
      END IF
C
      IF( CFL .EQ. 'ONE' ) THEN
        GOTO 98
      ELSE 
        LCAPH = LQ( LCAPH )
        IF( LCAPH .NE. 0 ) GOTO 11
      ENDIF
  98  RETURN
  99  WRITE(PRUNIT,100)
      WRITE(PRUNIT,101) LCAPH,NCAPH
      RETURN
C
C
 100  FORMAT(//1X,57('*')/)
 101  FORMAT(/,' Wrong Address for a CAPH bank: LCAPH =',I8
     +,' NCAPH =',I8/)
 102  FORMAT(/,
     +'  ========================================================='/
     +'       CAPH: Calorimeter hits bank                         '/
     +'  ========================================================='/)
 121  FORMAT('  This CAPH bank is under the ',A4,' path')
 103  FORMAT('  Bank ',A4,' is EMPTY')
 104  FORMAT('  Bank ',A4,' has DATA')
 105  FORMAT('  Version number of the bank CAPH :',I10)
 106  FORMAT('  Number of Clusters (CACL banks) :',I10,/,
     &       '  Number of Jets     (JETS banks) :',I10,/
     &       '  Algorithm Number/Name           :',I10,' / ',A10,/,
     &       '  Algorithm Version               :',I10,/)
  108 FORMAT('     RADIUS_OF_CONE               :', F10.3,
     &       '     MIN_JET_ET  (GeV)            :', F10.3,/,
     &       '     ET_SPLIT_FRAC                :', E10.2,
     &       '     MIN_CLUSTER_SEPARATION       :', E10.2,/,
     &       '     SEED_MIN_ET (GeV)            :', F10.3,
     &       '     CAND_MIN_ET (GeV)            :', F10.3,/,
     &       '     PRECLU_MIN_ET (GeV)          :', F10.3)
  110 FORMAT('     CACL E OR ET (-1.0=E,0.0=ET) :', F10.1,
     &       '     ETA_NEIGHBOR_LIMIT           :', F10.1,/,
     &       '     PHI_NEIGHBOR_LIMIT           :', F10.1,
     &       '     MINIMUM_TOWER_ET   (GeV)     :', E10.3,/,
     &       '     CACL_ENERGY_THRESHOLD (GeV)  :', F10.3,
     &       '     CACL_ET_THRESHOLD (GeV)      :', F10.3,/,
     &       '     CLUSTER_EM_RATIO_THRESHOLD   :', F10.3,
     &       '     JET_ET_THRESHOLD (GeV)       :', F10.3,/,
     &       '     POWER_FOR_SHOWER_CENTER      :', F10.3,
     &       '     MAXIMUM_CAL_CLUSTERS         :', F10.0)
C
  999 RETURN
      END
