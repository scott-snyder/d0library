C----------------------------------------------------------------------
C-
C-   Created   2-NOV-1989   J.Fr. Glicenstein
C-   Updated   1-JUL-1991   A. Zylberstejn  : add COSMIC1 and MCDATA
C-                                            logical variables
C-   Updated   7-DEC-1993   A. Zylberstejn  :split common in 3 parts 
C-
C----------------------------------------------------------------------
      COMMON/TCNTRL1/RMIN,RMAX,ZMIN,ZMAX,TYPPRO,
     &              THMIN,THMAX,TPHIMI,TPHIMA,EPICOR(3),ETOTM,
     &              TVALFL(16),TVALIN(20),TROAD,TEXAMIN,APC(3)
      COMMON/TCNTRL2/ TRDPEDES,TRDGAIN,
     &              TVALLO(13),COSMIC1,MCDATA
      COMMON/TCNTRL3/ TVALCA(20),OPT
c
      REAL          RMIN,RMAX,ZMIN,ZMAX,EPICOR,APC,ETOTM
      REAL          THMIN,THMAX,TPHIMI,TPHIMA,TVALFL
      INTEGER       TVALIN,TYPPRO
      CHARACTER*3   OPT,TVALCA
      LOGICAL       TVALLO,COSMIC1,MCDATA, TROAD,TEXAMIN,TTRAKIN
      LOGICAL       TRDPEDES,TRDGAIN
