C----------------------------------------------------------------------
C-   Created  24-JAN-1994   Chip Stewart   
C-   for JETNET V3.0 
C----------------------------------------------------------------------
C.../JNDAT2/:
C...TINV(I) (D=0.0)     inverse temperature of layer I (if 0 use PARJN(3))
C...
C...IGFN(I) (D=0)       sigmoid function for layer I (if 0 use MSTJN(3))
C...
C...ETAL(I) (D=0.0)     learning parameter in layer I (if 0 use PARJN(1))
C...
C...WIDL(I) (D=0.0)     initial width in layer I (if 0 use PARJN(4))
C...
C...SATM(I) (R)         saturation measure "S" for layer I.
C...      MSTJN(3)=1 -> S = sum[(1.-2.*O(J))**2]
C...      MSTJN(3)=2 -> S = sum[O(J)**2]
C...
C...End of description
      INTEGER IGFN(10)
      INTEGER TINV(10),ETAL(10),WIDL(10),SATM(10)
      COMMON /JNDAT2/ TINV,IGFN,ETAL,WIDL,SATM
