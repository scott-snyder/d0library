C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
*
      INTEGER IPAIR,ICOMP,IPHOT,IPFIS,IDRAY,IANNI,IBREM,IHADR,IMUNU
     +       ,IDCAY,ILOSS,IMULS,IRAYL
      REAL    SPAIR,SLPAIR,ZINTPA,STEPPA,SCOMP,SLCOMP,ZINTCO,STEPCO
     +       ,SPHOT,SLPHOT,ZINTPH,STEPPH,SPFIS,SLPFIS,ZINTPF,STEPPF
     +       ,SDRAY,SLDRAY,ZINTDR,STEPDR,SANNI,SLANNI,ZINTAN,STEPAN
     +       ,SBREM,SLBREM,ZINTBR,STEPBR,SHADR,SLHADR,ZINTHA,STEPHA
     +       ,SMUNU,SLMUNU,ZINTMU,STEPMU,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +       ,SLOSS,SOLOSS,STLOSS,DPHYS2,SMULS,SOMULS,STMULS,DPHYS3
     +       ,SRAYL,SLRAYL,ZINTRA,STEPRA
C