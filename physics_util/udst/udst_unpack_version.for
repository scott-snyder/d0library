      SUBROUTINE UDST_UNPACK_VERSION(V,IVCOR,IGLOB,IZFIT,IVERT,IPMUO,
     &  IPELC,IPPHO,IHMTE,IHMTP,IPNUT,IPTAU,ICACL,ICASH,IJETS,IJNEP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack bank version numbers from UDST
C-
C-   Inputs  : V
C-   Outputs : IVCOR,IGLOB,IZFIT,IVERT,IPMUO,IPELC,IPPHO,IHMTE,IHMTP,IPNUT
C-             IPTAU,ICACL,ICASH,IJETS,IJNEP
C-
C-   Created  31-JAN-1994   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    V(3)
      INTEGER IVCOR,IGLOB,IZFIT,IVERT,IPMUO,IPELC,IPPHO,IHMTE,IHMTP,
     &  IPNUT,IPTAU,ICACL,ICASH,IJETS,IJNEP
      INTEGER IV
C----------------------------------------------------------------------
      IV=INT(V(1))
      IPMUO=IV/65536
      IV=IV-IPMUO*65536
      IVERT=IV/4096
      IV=IV-IVERT*4096
      IZFIT=IV/256
      IV=IV-IZFIT*256
      IGLOB=IV/16
      IVCOR=IV-IGLOB*16
      IV=INT(V(2))
      IPNUT=IV/65536
      IV=IV-IPNUT*65536
      IHMTP=IV/4096
      IV=IV-IHMTP*4096
      IHMTE=IV/256
      IV=IV-IHMTE*256
      IPPHO=IV/16
      IPELC=IV-IPPHO*16
      IV=INT(V(3))
      IJNEP=IV/65536
      IV=IV-IJNEP*65536
      IJETS=IV/4096
      IV=IV-IJETS*4096
      ICASH=IV/256
      IV=IV-ICASH*256
      ICACL=IV/16
      IPTAU=IV-ICACL*16
C----------------------------------------------------------------------
  999 RETURN
      END
