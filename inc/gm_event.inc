C----------------------------------------------------------------------
C-
C-   Created  11-JAN-1993   Harrison B. Prosper, Seung-chan Ahn
C-   Updated   8-MAR-1993   Harrison B. Prosper
C-    Add Partons
C-
C----------------------------------------------------------------------
C
C   CARBON COPY OF ELECTRON.DOC
C
      INTEGER ELECTRON_BANK_SIZE
      PARAMETER(ELECTRON_BANK_SIZE=29)
      STRUCTURE/ELECTRON_BANK/
        UNION
          MAP
            CHARACTER*(4*ELECTRON_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(ELECTRON_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(ELECTRON_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    VAREX
            REAL    VAREY
            REAL    EMOUT
            REAL    ECORE
            REAL    ECONE
            REAL    EMCORE
            REAL    EMCONE
            REAL    CHISQ
            REAL    CHISQ_T
            INTEGER NTRACKS
            REAL    DCLOSE
            REAL    CDCMIP
            REAL    FDCMIP
            REAL    VTXMIP
            REAL    TRDLIKE 
            INTEGER ECORFLAG
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/ELECTRON_BANK/ELECTRON
C
C   CARBON COPY OF EVENT.DOC
C
      INTEGER EVENT_BANK_SIZE
      PARAMETER(EVENT_BANK_SIZE=30)
      STRUCTURE/EVENT_BANK/
        UNION
          MAP
            CHARACTER*(4*EVENT_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(EVENT_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(EVENT_BANK_SIZE)
          ENDMAP
          MAP
            INTEGER RUN
            INTEGER EVENT
            INTEGER TRIGGER
            INTEGER OBJECT
            INTEGER COUNT
            INTEGER DATE_TIME(2)
            INTEGER INPUT_EVENT(2)
            INTEGER OUTPUT_EVENT
            INTEGER EVENT_TYPE
            INTEGER TRIGGER_BITS
            INTEGER CODE_VERSION
            INTEGER HEADER_VERSION
            INTEGER FILTER_BITS(4)
            INTEGER RECO_BITS(10)
            INTEGER ERROR_CODE
            INTEGER FLAG_WORD
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/EVENT_BANK/EVENT

C
C   CARBON COPY OF JET.DOC
C
      INTEGER JET_BANK_SIZE
      PARAMETER(JET_BANK_SIZE=29)
      STRUCTURE/JET_BANK/
        UNION
          MAP
            CHARACTER*(4*JET_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(JET_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(JET_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    VAREX
            REAL    VAREY
            REAL    RMSETA
            REAL    RMSPHI
            REAL    FEMET
            INTEGER FLAG
            INTEGER NCELLS
            REAL    FICDET
            REAL    FCHET
            REAL    RHOTTEST
            INTEGER NTRACKS
            INTEGER NTOWERS
            REAL    CONESIZE
            REAL    PHIDIFF
            REAL    X15
            INTEGER ECORFLAG
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/JET_BANK/JET
C
C   CARBON COPY OF ALLJET.DOC
C
      INTEGER ALLJET_BANK_SIZE
      PARAMETER(ALLJET_BANK_SIZE=29)
      STRUCTURE/ALLJET_BANK/
        UNION
          MAP
            CHARACTER*(4*ALLJET_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(ALLJET_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(ALLJET_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    VAREX
            REAL    VAREY
            REAL    RMSETA
            REAL    RMSPHI
            REAL    FEMET
            INTEGER FLAG
            INTEGER NCELLS
            REAL    FICDET
            REAL    FCHET
            REAL    RHOTTEST
            INTEGER NTRACKS
            INTEGER NTOWERS
            REAL    PHIDIFF
            REAL    CONESIZE
            REAL    X15
            INTEGER ECORFLAG
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/ALLJET_BANK/ALLJET
C
C   CARBON COPY OF ETMISS.DOC
C
      INTEGER ETMISS_BANK_SIZE
      PARAMETER(ETMISS_BANK_SIZE=29)
      STRUCTURE/ETMISS_BANK/
        UNION
          MAP
            CHARACTER*(4*ETMISS_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(ETMISS_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(ETMISS_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    VAREX
            REAL    VAREY
            REAL    SIGET
            REAL    ETSCALAR
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/ETMISS_BANK/ETMISS
C
C   CARBON COPY OF MUON.DOC
C
      INTEGER MUON_BANK_SIZE
      PARAMETER(MUON_BANK_SIZE=29)
      STRUCTURE/MUON_BANK/
        UNION
          MAP
            CHARACTER*(4*MUON_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(MUON_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(MUON_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    ISOL1
            REAL    ISOL2
            REAL    ISOL4
            INTEGER NCD
            REAL    ANGCD
            REAL    IMPT
            INTEGER IFW4
            INTEGER IFW1
            REAL    DBL
            REAL    DPHI
            REAL    DTHETA
            REAL    CAL_EN
            REAL    TRK_HITS
            REAL    FIT_HITS
            REAL    VAREX
            REAL    VAREY
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/MUON_BANK/MUON
C
C   CARBON COPY OF PHOTON.DOC
C
      INTEGER PHOTON_BANK_SIZE
      PARAMETER(PHOTON_BANK_SIZE=29)
      STRUCTURE/PHOTON_BANK/
        UNION
          MAP
            CHARACTER*(4*PHOTON_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(PHOTON_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(PHOTON_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    VAREX
            REAL    VAREY
            REAL    EMOUT
            REAL    ECORE
            REAL    ECONE
            REAL    EMCORE
            REAL    EMCONE
            REAL    CHISQ
            REAL    CHISQ_T
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/PHOTON_BANK/PHOTON
C
C   CARBON COPY OF TAU.DOC
C
      INTEGER TAU_BANK_SIZE
      PARAMETER(TAU_BANK_SIZE=29)
      STRUCTURE/TAU_BANK/
        UNION
          MAP
            CHARACTER*(4*TAU_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(TAU_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(TAU_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    RMSWIDTH
            REAL    X2
            REAL    X3
            REAL    X4
            REAL    X5
            REAL    X6
            REAL    X7
            REAL    X8
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    VAREX
            REAL    VAREY
            REAL    VAREZ
            REAL    DEXDEY
            REAL    DEXDEZ
            REAL    DEYDEZ
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/TAU_BANK/TAU
C
C   CARBON COPY OF VERTEX.DOC
C
      INTEGER VERTEX_BANK_SIZE
      PARAMETER(VERTEX_BANK_SIZE=29)
      STRUCTURE/VERTEX_BANK/
        UNION
          MAP
            CHARACTER*(4*VERTEX_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(VERTEX_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(VERTEX_BANK_SIZE)
          ENDMAP
          MAP
            REAL    X
            REAL    Y
            REAL    Z
            REAL    DX
            REAL    DY
            REAL    DZ
            REAL    WEIGHT
            REAL    FLAG
            INTEGER QUALITY
            REAL    X1
            REAL    X2
            REAL    X3
            REAL    X4
            REAL    X5
            REAL    X6
            REAL    X7
            REAL    X8
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/VERTEX_BANK/VERTEX
C
C
C   CARBON COPY OF GLOBAL.DOC
C
      INTEGER GLOBAL_BANK_SIZE
      PARAMETER(GLOBAL_BANK_SIZE=29)
      STRUCTURE/GLOBAL_BANK/
        UNION
          MAP
            CHARACTER*(4*GLOBAL_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(GLOBAL_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(GLOBAL_BANK_SIZE)
          ENDMAP
          MAP
            INTEGER VERSION
            INTEGER QUALITY
            INTEGER TOTRACK
            INTEGER NCEMFH
            REAL    CAL_SUMET
            REAL    EC_SUMET
            REAL    CC_SUMET
            REAL    CAL_SUME
            REAL    ECS_SUME
            REAL    CCUP_SUME
            REAL    CCDN_SUME
            REAL    ECN_SUME
            REAL    MR_SUME
            REAL    MR_SUMET
            REAL    HOT_SUME
            REAL    HOT_VSUMET
            REAL    X8
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/GLOBAL_BANK/GLOBAL
C
C
C   CARBON COPY OF PARTON.DOC
C
      INTEGER PARTON_BANK_SIZE
      PARAMETER(PARTON_BANK_SIZE=29)
      STRUCTURE/PARTON_BANK/
        UNION
          MAP
            CHARACTER*(4*PARTON_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(PARTON_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(PARTON_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            INTEGER ID
            INTEGER PN
            INTEGER PARENT
            INTEGER FN
            INTEGER FAMILY
            REAL    X6
            REAL    X7
            REAL    X8
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/PARTON_BANK/PARTON
C
C
C   CARBON COPY OF PETMISS.DOC
C
      INTEGER PETMISS_BANK_SIZE
      PARAMETER(PETMISS_BANK_SIZE=29)
      STRUCTURE/PETMISS_BANK/
        UNION
          MAP
            CHARACTER*(4*PETMISS_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(PETMISS_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(PETMISS_BANK_SIZE)
          ENDMAP
          MAP
            REAL    PX
            REAL    PY
            REAL    PZ
            REAL    E
            REAL    ET
            REAL    ETA
            REAL    PHI
            REAL    DETA
            INTEGER QUALITY
            REAL    X1
            REAL    X2
            REAL    X3
            REAL    X4
            REAL    X5
            REAL    X6
            REAL    X7
            REAL    X8
            REAL    X9
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/PETMISS_BANK/PETMISS
C
C ------------------------------------------------------------------
C             +1     I       event id(1)
C              2     I       event id(2)
C              3     I       event number
C              4     I       reaction type
C              5     I       number of primary jet banks
C              6     I         "    of stable parton banks (final+initial)
C              7     I         "    of PJET banks
C              8     I         "    of particle banks
C              9     I         "    of vertex banks
C             10     I         "    of lepton banks
C             11     F       cross section in microbarns
C             12     F       weight
C             13     F       effective q**2
C             14     F       hard scattering invariant s
C             15     F        "      "          "      t
C             16     F        "      "          "      u
C             17     D        Seed (part 1)
C             18     D        Seed (part 2)
C
C   CARBON COPY OF PEVENT.DOC
C
      INTEGER PEVENT_BANK_SIZE
      PARAMETER(PEVENT_BANK_SIZE=29)
      STRUCTURE/PEVENT_BANK/
        UNION
          MAP
            CHARACTER*(4*PEVENT_BANK_SIZE) CDUM
          ENDMAP
          MAP
            INTEGER IDUM(PEVENT_BANK_SIZE)
          ENDMAP
          MAP
            REAL RDUM(PEVENT_BANK_SIZE)
          ENDMAP
          MAP
            INTEGER ID(2)
            INTEGER NUMBER
            INTEGER TYPE
            INTEGER NPRIM
            INTEGER NSTAB
            INTEGER NPJET
            INTEGER NPART
            INTEGER NVERT
            INTEGER NLEPT
            REAL    XSECTION
            REAL    WEIGHT
            REAL    Q2
            REAL    S
            REAL    T
            REAL    U
            DOUBLE PRECISION SEED
            REAL    X10
            REAL    X11
            REAL    X12
            REAL    X13
            REAL    X14
            REAL    X15
            REAL    X16
            REAL    X17
            REAL    X18
            REAL    X19
            REAL    X20
          ENDMAP
        END UNION
      END STRUCTURE
      RECORD/PEVENT_BANK/PEVENT
C
      INTEGER MAXVERTEX,MAXPHOTON,MAXELECTRON,MAXJET,MAXPETMISS,
     &        MAXETMISS,MAXMUON,MAXTAU,MAXALLJET,MAXPARTON
C
      PARAMETER(MAXVERTEX=10)
      PARAMETER(MAXPHOTON=50)
      PARAMETER(MAXELECTRON=50)
      PARAMETER(MAXJET=200)
      PARAMETER(MAXALLJET=200)
      PARAMETER(MAXETMISS=10)
      PARAMETER(MAXMUON=50)
      PARAMETER(MAXTAU=50)
      PARAMETER(MAXPARTON=100)
      PARAMETER(MAXPETMISS=2 )
C
      INTEGER NVERTEX,NPHOTON,NALLJET,NPETMISS,
     &        NELECTRON,NJET,NETMISS,NMUON,NTAU,NPARTON,NGLOBAL
C
      COMMON /GM_EVENT/ NVERTEX,  VERTEX(MAXVERTEX),
     &                  NPARTON,  PARTON(MAXPARTON),
     &                  NPETMISS, PETMISS(MAXPETMISS),
     &                            PEVENT,
     &                  NPHOTON,  PHOTON(MAXPHOTON),
     &                  NELECTRON,ELECTRON(MAXELECTRON),
     &                  NMUON,    MUON(MAXMUON),
     &                  NTAU,     TAU(MAXTAU),
     &                  NJET,     JET(MAXJET),
     &                  NETMISS,  ETMISS(MAXETMISS),
     &                  NALLJET,  ALLJET(MAXJET),
     &                  EVENT, GLOBAL
