
C
C  ... ...........TRD DESCRIPTION...........
C   
C   -------COMMON,RADDSC: RADIATOR
C                         --------
      COMMON/RADDSC/NFOIL,FOIL,XFOIL,GAP,XGAP
      CHARACTER *3 FOIL,GAP
      INTEGER NFOIL
      REAL    XFOIL,XGAP
C   --------COMMON ,WINDESC :ENTRANCE WINDOW
C                            ---------------
      COMMON/WINDSC/SKIN,XSKIN,SAND,XSAND,MET,XMETAL,STUFF,XSTUFF
      CHARACTER *3  SKIN,      SAND,      MET,       STUFF
      REAL               XSKIN,     XSAND,    XMETAL,      XSTUFF
C   --------COMMON,TECDSC :T.E.C.
C                          ------
      COMMON/TECDSC/GAS,GAS2,GAS3,PERC,PERC2,PERC3,XGAS,
     +              XDER,XAMP,XD10,XA10,ATTACH,PED,NOISE
      CHARACTER *3  GAS,GAS2,GAS3
      REAL                        PERC,PERC2,PERC3,XGAS,
     +              XDER,XAMP,XD10,XA10,ATTACH,PED,NOISE
