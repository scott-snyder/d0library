
C----------------------------------------------------------------------
C-
C-   Created  11-MAY-1989   A. Zylberstejn: Enegy deposit in the TRD
C-                                     for a given track
C-  TRETOT(I,J)  : Total energy deposited in layer I for track J
C-                 I= plane number 1 to 3 anodes,4 to 6 for cath.
C-                 J= Track number
C-  ETOTWI(I,J,K): I th Total energy deposited on wire in plane J for
C-                   track K
C-                 I= i th wire number hit in each plane
C-                 J= plane number
C-                 K= Track number
C-  NBHWIR(I,J)  : Nb. of hit wires in plane I for track J
C-  WIRNBH(I,J,K): I th wire hit in plane J for track K
C----------------------------------------------------------------------
      INTEGER TTRAKG
      INTEGER NBINFO
      PARAMETER( NBINFO =  10)
      PARAMETER( TTRAKG = 60 )
      COMMON /TRENER/ TRETOT(6,TTRAKG),ETOTWI(NBINFO,6,TTRAKG),
     &                EMAXCH(6,TTRAKG),NWIRMA(6,TTRAKG),
     &                NTWIRE(2,TTRAKG),
     &                ETOTAL(2,TTRAKG),ETRUNC(2,TTRAKG),
     &                LIKET(2,TTRAKG) ,LIKECL(2,TTRAKG),
     &                NBHWIR(6,TTRAKG),WIRNBH(NBINFO,6,TTRAKG),
     &                TMINIM(6,TTRAKG),PHITRA(3,TTRAKG),
     &                MULT_CELL(NBINFO,6,TTRAKG)
      REAL            TRETOT          ,ETOTWI            ,
     &                EMAXCH
      INTEGER                          NWIRMA,NTWIRE
      REAL            ETOTAL          ,ETRUNC
      REAL            LIKET           ,LIKECL
      INTEGER         NBHWIR          ,WIRNBH
      REAL            TMINIM          ,PHITRA
      INTEGER         MULT_CELL
