      SUBROUTINE PRCACL(PRUNIT,CLNUM,IDUM,CFL,IDUM1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints CACL bank and associated CACH bank.
C-
C-   Inputs  :   PRUNIT= unit number for printout
C-               CLNUM = cluster number of cluster to be printed.
C-                       CLNUM = 0 all clusters will be printed
C-               CFL   = flag to control printout
C-                     = 'ALL'  prints CACL and CACH (detailed)as well
C-               IDUM,IDUM1   = Not used.(for compatibility with DMPANY)
C-   Outputs :
C-   Controls:
C-
C-   Created   7-MAY-1989   Rajendran Raja
C-   Modified  7-JUN-1991   James Kourlas    minor bugs - see below - dated
C-   Updated  18-SEP-1992   Rajendran Raja  ADDED printing of Center 
C-   dispersions 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER IDUM,IDUM1
      INTEGER PERLIN
      PARAMETER( PERLIN = 6 )           ! Number of reps per line
      INTEGER PRUNIT,CLNUM,NREP,PN,K1
      CHARACTER*(*) CFL
      CHARACTER*20 CHBF(PERLIN)
      CHARACTER*120 CHBF1
      CHARACTER*20 CIBF(PERLIN)
      CHARACTER*120 CIBF1
      EQUIVALENCE (CHBF,CHBF1)
      EQUIVALENCE (CIBF,CIBF1)
      INTEGER IOK
      INTEGER LCACL,GZCACL,LCACH,ICL,K,NDCACH,LCAEH,GZCAEH
      REAL    XX,YY,ZZ
C----------------------------------------------------------------------
      LCACL = GZCACL()
      IF(LCACL.EQ.0)THEN
        CALL ERRMSG('CALORIMETER','PRCACL',
     &    'NO CACL BANKS TO PRINT','W')
        RETURN
      ENDIF
C
      LCAEH = GZCAEH()
      NREP = IQ(LCAEH+2)                ! Repetition number
      CHBF1 = ' '
      CIBF1 = ' '
C
  100 CONTINUE
      ICL = IQ(LCACL+2)                       ! Cluster number -  7-JUN-1991
      IF(CLNUM.EQ.0.OR.ICL.EQ.CLNUM)THEN
C
C ****  Print stuff here
C
        WRITE(PRUNIT,150)
        WRITE(PRUNIT,200)IQ(LCACL+1),IQ(LCACL+2),IQ(LCACL+3),
     &    (Q(LCACL+K),K=4,19)
  150   FORMAT(/,
     +' ========================================================='/
     +'              CACL: Calorimeter Cluster bank              '/
     +' ========================================================='/)
  200   FORMAT(' Bank version number = ',I5,
     &         '      Cluster number = ',I5,
     &         '   EM/Hadronic flag  = ',I5,/,
     &  3X,' EX',5X,' EY',5X,' EZ',5X,' E',4X,' ET',
     &  5X,'Sig2(Ex)',2X,'Sig2(Ey)',1X,'THETA',2X,'PHI',3X,'ETA',
     &  3X,'XCENT',3X,'YCENT',3X,'ZCENT',3X,'ETOTL',3X,'ETALL',
     &  4X,'FH1',/,
     &  1X,3F8.3,2F7.3,2F10.3,3F6.3,3F8.3,3F8.3)
C
        WRITE(PRUNIT,199)(Q(LCACL+K),K=20,23)
  199   FORMAT(4X,'DISPX',3X,'DISPY',3X,'DISPZ',/,1X,3F8.3)
C
        LCACH = LQ(LCACL-1)
        NDCACH = IQ(LCACH-1)          ! Number of data words
        WRITE(PRUNIT,201)(IQ(LCACH+K),K=1,NDCACH-1)
  201   FORMAT(/,' CACH Bank Version number ',I5,
     &             ' Number of Cells in cluster ',I5,/,
     &             ' Cell  Pointers to CAEH/CAEP bank ',/,(12I8))
        IF(CFL.EQ.'ALL')THEN
          IF(LCAEH.EQ.0)THEN
            CALL ERRMSG('CALORIMETER','PRCACL',
     &        'NO CAEH BANK PRESENT. NO DETAILED PRINT POSSIBLE','W')
          ELSE
            WRITE(PRUNIT,205)
  205       FORMAT(/' Details of contributing cells ')
            DO 207 K1 = 1,PERLIN
              WRITE(CHBF(K1),206)
              WRITE(CIBF(K1),208)
  207       CONTINUE
  206       FORMAT(' Eta',' Phi',' Lyr','    Enr',1X)
  208       FORMAT('    XX','    YY','    ZZ',2X)
            WRITE(PRUNIT,204)CHBF1
            WRITE(PRUNIT,204)CIBF1
            K1 = 0
            CIBF1 = ' '
            CHBF1 = ' '
            DO 203 K = 3,NDCACH-1                       ! 7-JUN-1991 added -1
              PN = NREP*(IQ(LCACH+K)-1)+LCAEH
              K1 = K1+1
              WRITE(CHBF(K1),202)IQ(PN+12),IQ(PN+13),IQ(PN+14),Q(PN+7)
              CALL CELXYZ(IQ(PN+12),IQ(PN+13),IQ(PN+14),XX,YY,ZZ,IOK)
              IF(IOK.EQ.0)WRITE(CIBF(K1),209)XX,YY,ZZ
  202         FORMAT(3I4,F7.2,1X)
  209         FORMAT(3F6.1,2X)
              IF(K1.EQ.PERLIN)THEN
                WRITE(PRUNIT,204)CHBF1
                WRITE(PRUNIT,210)CIBF1
  204           FORMAT(1X,A120)
  210           FORMAT(1X,A120/)
                K1 = 0
                CHBF1 = ' '
                CIBF1 = ' '
              ENDIF
  203       CONTINUE
            IF(K1.NE.0)WRITE(PRUNIT,204)CHBF1
            IF(K1.NE.0)WRITE(PRUNIT,204)CIBF1
          ENDIF
        ENDIF
        IF(CLNUM.NE.0)GO TO 999   ! printed the cluster number of interest
      ENDIF
      LCACL = LQ(LCACL)
      IF(LCACL.NE.0)GO TO 100
  999 RETURN
      END
