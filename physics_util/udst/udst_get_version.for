      SUBROUTINE UDST_GET_VERSION(V)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get version numbers of all banks saved on micro DST
C-                         and pack them into array V. 4 bits are allocated
C-                         to each version number. Thus no version number is
C-                         allowed to be larger than 15.
C-
C-   Inputs  : DST banks
C-   Outputs : V
C-
C-   Created  31-JAN-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    V(3)
      INTEGER LVCOR,LGLOB,LZFIT,LVERT,LPMUO,LPELC,LPPHO,LHMTE,LHMTP,
     &  LPNUT,LPTAU,LCACL,LCASH,LJETS,LJNEP
      INTEGER GZVCOR,GZGLOB,GZZFIT,GZVERT,GZPMUO,GZPELC,GZPPHO,GZHMTE,
     &  GZHMTP,GZPNUT,GZPTAU,GZCACL,GZCASH,GZJETS,GZJNEP
      INTEGER IVCOR,IGLOB,IZFIT,IVERT,IPMUO,IPELC,IPPHO,IHMTE,IHMTP,
     &  IPNUT,IPTAU,ICACL,ICASH,IJETS,IJNEP
      INTEGER MAX,I,IER,NCONE
      PARAMETER( NCONE = 6,MAX = 15 )
      REAL    TEMPLATE(7,NCONE)
      DATA    TEMPLATE/ 1.,6.,0.7,0.,0.,0.,0.,      ! CONE R=0.7
     &                  1.,6.,0.5,0.,0.,0.,0.,      ! CONE R=0.5
     &                  1.,6.,0.3,0.,0.,0.,0.,      ! CONE R=0.3
     &                  1.,6.,1.0,0.,0.,0.,0.,      ! CONE R=1.0
     &                  3.,6.,0.7,8.,1.,9.,0.35,    ! CONE R=0.7  NOMERGE
     &                  1.,6.,-1.,0.,0.,0.,0./      ! NN
C----------------------------------------------------------------------
C... get all the version numbers
      LVCOR=GZVCOR()
      IF(LVCOR.GT.0)IVCOR=IQ(LVCOR+1)
      IF(IVCOR.GT.MAX)CALL ERRMSG('IVCOR>MAX','UDST_GET_VERSION',
     &  'version number of VCOR exceeds maximum','F')
      LGLOB=GZGLOB()
      IF(LGLOB.GT.0)IGLOB=IQ(LGLOB+1)
      IF(IGLOB.GT.MAX)CALL ERRMSG('IGLOB>MAX','UDST_GET_VERSION',
     &  'version number of VCOR exceeds maximum','F')
      LZFIT=GZZFIT(0)
      IF(LZFIT.GT.0)IZFIT=IQ(LZFIT+1)
      IF(IZFIT.GT.MAX)CALL ERRMSG('IZFIT>MAX','UDST_GET_VERSION',
     &  'version number of ZFIT exceeds maximum','F')
      LVERT=GZVERT(1)
      IF(LVERT.GT.0)IVERT=IQ(LVERT+1)
      IF(IVERT.GT.MAX)CALL ERRMSG('IVERT>MAX','UDST_GET_VERSION',
     &  'version number of VERT exceeds maximum','F')
      LPMUO=GZPMUO(0)
      IF(LPMUO.GT.0)IPMUO=IQ(LPMUO+1)
      IF(IPMUO.GT.MAX)CALL ERRMSG('IPMUO>MAX','UDST_GET_VERSION',
     &  'version number of PMUO exceeds maximum','F')
      LPELC=GZPELC()
      IF(LPELC.GT.0)THEN
        IPELC=IQ(LPELC+1)
        IF(IPELC.GT.MAX)CALL ERRMSG('IPELC>MAX','UDST_GET_VERSION',
     &    'version number of PELC exceeds maximum','F')
        LHMTE=GZHMTE()
        IF(LHMTE.GT.0)IHMTE=IQ(LHMTE+1)
        IF(IHMTE.GT.MAX)CALL ERRMSG('IHMTE>MAX','UDST_GET_VERSION',
     &    'version number of HMTE exceeds maximum','F')
        LCACL  = LQ(LPELC-2)
        IF(LCACL.GT.0)ICACL=IQ(LCACL+1)
        IF(ICACL.GT.MAX)CALL ERRMSG('ICACL>MAX','UDST_GET_VERSION',
     &    'version number of CACL exceeds maximum','F')
        LCASH  = LQ(LCACL-2)
        IF(LCASH.GT.0)ICASH=IQ(LCASH+1)
        IF(ICASH.GT.MAX)CALL ERRMSG('ICASH>MAX','UDST_GET_VERSION',
     &    'version number of CASH exceeds maximum','F')
      ENDIF
      LPPHO=GZPPHO()
      IF(LPPHO.GT.0)THEN
        IPPHO=IQ(LPPHO+1)
        IF(IPPHO.GT.MAX)CALL ERRMSG('IPPHO>MAX','UDST_GET_VERSION',
     &    'version number of PPHO exceeds maximum','F')
        LHMTP=GZHMTP()
        IF(LHMTP.GT.0)IHMTP=IQ(LHMTP+1)
        IF(IHMTP.GT.MAX)CALL ERRMSG('IHMTP>MAX','UDST_GET_VERSION',
     &    'version number of HMTP exceeds maximum','F')
        LCACL  = LQ(LPPHO-2)
        IF(LCACL.GT.0)ICACL=IQ(LCACL+1)
        IF(ICACL.GT.MAX)CALL ERRMSG('ICACL>MAX','UDST_GET_VERSION',
     &    'version number of CACL exceeds maximum','F')
        LCASH  = LQ(LCACL-2)
        IF(LCASH.GT.0)ICASH=IQ(LCASH+1)
        IF(ICASH.GT.MAX)CALL ERRMSG('ICASH>MAX','UDST_GET_VERSION',
     &    'version number of CASH exceeds maximum','F')
      ENDIF
      LPNUT=GZPNUT(0)
      IF(LPNUT.GT.0)IPNUT=IQ(LPNUT+1)
      IF(IPNUT.GT.MAX)CALL ERRMSG('IPNUT>MAX','UDST_GET_VERSION',
     &  'version number of PNUT exceeds maximum','F')
      LPTAU=GZPTAU()
      IF(LPTAU.GT.0)IPTAU=IQ(LPTAU+1)
      IF(IPTAU.GT.MAX)CALL ERRMSG('IPTAU>MAX','UDST_GET_VERSION',
     &  'version number of PTAU exceeds maximum','F')
      DO I = 1, NCONE
        IF(I.LT.NCONE)CALL SET_CAPH('CONE_JET',TEMPLATE(1,I),IER)
        IF(I.EQ.NCONE)CALL SET_CAPH('NN_JET',TEMPLATE(1,I),IER)
        IF(IER.EQ.0)THEN
          LJETS=GZJETS()
          IF(LJETS.GT.0)IJETS=IQ(LJETS+1)
          IF(IJETS.GT.MAX)CALL ERRMSG('IJETS>MAX','UDST_GET_VERSION', 
     &      'version number of JETS exceeds maximum','F')
          LJNEP=0
          DO WHILE (LJETS.NE.0.AND.LJNEP.EQ.0)
            LJNEP=LQ(LJETS-2)
            IF(LJNEP.GT.0)IJNEP=IQ(LJNEP+1)
            IF(IJNEP.GT.MAX)CALL ERRMSG('IJNEP>MAX','UDST_GET_VERSION',
     &        'version number of JNEP exceeds maximum','F')
            LJETS=LQ(LJETS)
          ENDDO
        ENDIF
        CALL RESET_CAPH
      ENDDO
C... pack version numbers into V
      V(1)=FLOAT(IVCOR+IGLOB*16+IZFIT*256+IVERT*4096+IPMUO*65536)
      V(2)=FLOAT(IPELC+IPPHO*16+IHMTE*256+IHMTP*4096+IPNUT*65536)
      V(3)=FLOAT(IPTAU+ICACL*16+ICASH*256+IJETS*4096+IJNEP*65536)
C----------------------------------------------------------------------
  999 RETURN
      END
