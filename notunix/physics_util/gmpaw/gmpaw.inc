C----------------------------------------------------------------------
C-
C-   Created   3-JAN-1994   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      REAL    VIDN1,VIDN2,VIDN3,VIDN
      REAL
     +RUN     ,EVENT   ,TRIGGER ,OBJECT  ,COUNT   ,NUMBER  ,
     +PX      ,PY      ,PZ      ,E       ,ET      ,ETA     ,
     +PHI     ,DETA    ,QUALITY ,X1      ,X2      ,X3      ,
     +X4      ,X5      ,X6      ,X7      ,X8      ,X9      ,
     +X10     ,X11     ,X12     ,X13     ,X14     ,X15     ,
     +X16     ,X17     ,X18     ,X19     ,X20      
*
      INTEGER NCHEVT, ICHEVT, IDNEVT
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
*
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
*
      COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),
     +RUN     ,EVENT   ,TRIGGER ,OBJECT  ,COUNT   ,NUMBER  ,
     +PX      ,PY      ,PZ      ,E       ,ET      ,ETA     ,
     +PHI     ,DETA    ,QUALITY ,X1      ,X2      ,X3      ,
     +X4      ,X5      ,X6      ,X7      ,X8      ,X9      ,
     +X10     ,X11     ,X12     ,X13     ,X14     ,X15     ,
     +X16     ,X17     ,X18     ,X19     ,X20      
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      INTEGER IRUN, IEVENT, IOBJECT, INUMBER, IQUALITY, ITRIGGER, ICOUNT
      INTEGER ID_VERT, ID_PHOT, ID_ELEC
      INTEGER ID_MUON, ID_TAU,  ID_JET, ID_NU, ID_END
      INTEGER IPX,IPY,IPZ,IE,IET,IETA,IPHI,IDETA,IQUAL,IBASE,NSIZ,NOBJ
C----------------------------------------------------------------------
      EQUIVALENCE(QUALITY,IQUALITY)
      EQUIVALENCE(TRIGGER,ITRIGGER)
C----------------------------------------------------------------------
      PARAMETER ( IPX  = 1 )
      PARAMETER ( IPY  = 2 )
      PARAMETER ( IPZ  = 3 )
      PARAMETER ( IE   = 4 )
      PARAMETER ( IET  = 5 )
      PARAMETER ( IETA = 6 )
      PARAMETER ( IPHI = 7 )
      PARAMETER ( IDETA= 8 )
      PARAMETER ( IQUAL= 9 )
      PARAMETER ( IBASE= 9 )
      PARAMETER ( NSIZ =29 )
      PARAMETER ( NOBJ =20 )
C----------------------------------------------------------------------
      PARAMETER ( ID_VERT  = 0 )
      PARAMETER ( ID_PHOT  = 1 )
      PARAMETER ( ID_ELEC  = 2 )
      PARAMETER ( ID_MUON  = 3 )
      PARAMETER ( ID_TAU   = 4 )
      PARAMETER ( ID_JET   = 5 )
      PARAMETER ( ID_NU    = 6 )
      PARAMETER ( ID_END   =-1 )
C----------------------------------------------------------------------
      REAL VERTEX(NSIZ,NOBJ)
      REAL PHOTON(NSIZ,NOBJ)
      REAL ELECTRON(NSIZ,NOBJ)
      REAL MUON(NSIZ,NOBJ)
      REAL TAU(NSIZ,NOBJ)
      REAL JET(NSIZ,NOBJ)
      REAL NU(NSIZ,NOBJ)
C----------------------------------------------------------------------
      INTEGER NVERT
      INTEGER NPHOT
      INTEGER NELEC
      INTEGER NMUON
      INTEGER NTAU
      INTEGER NJET
      INTEGER NNU
C----------------------------------------------------------------------
