C----------------------------------------------------------------------
C-
C-   Created   2-JAN-1995   Jeffrey Bantly
C-
C----------------------------------------------------------------------
C
      INTEGER  IDW,IDZ,IDMU,IDNUM,IDELE,IDNUE,IDTAU,IDNUT
      INTEGER  IDS,IDC,IDU,IDD,IDB,IDT
      PARAMETER( IDW=80, IDZ=90 )
      PARAMETER( IDELE=12,IDNUE=11, IDMU=14,IDNUM=13, IDTAU=16,IDNUT=15)
      PARAMETER( IDD=1,IDU=2, IDS=3,IDC=4, IDB=5,IDT=6 )
C
C  Width and length of commons used to store event info.
C
      INTEGER NMCENT,NMCDAT
      PARAMETER(NMCENT=14,NMCDAT=120)
C
C  Pointers to info in MC array.
C
      INTEGER JMC_PAR,JMC_ND,JMC_ID0,JMC_ID,JMC_PX,JMC_PY,
     +  JMC_PZ,JMC_E,JMC_MASS,JMC_PHI,JMC_THETA,JMC_ETA,JMC_DR,
     +  JMC_PTREL
      PARAMETER(JMC_PAR=1,JMC_ND=2,JMC_ID0=3,JMC_ID=4,JMC_PX=5,JMC_PY=6,
     +  JMC_PZ=7,JMC_E=8,JMC_MASS=9,JMC_PHI=10,JMC_THETA=11,JMC_ETA=12,
     +  JMC_DR=13,JMC_PTREL=14)
C
      INTEGER NMC,IMCDAT(NMCENT,NMCDAT)
      REAL  RMCDAT(NMCENT,NMCDAT)
      EQUIVALENCE(IMCDAT,RMCDAT)
      COMMON/MYMCDATA/NMC,RMCDAT
