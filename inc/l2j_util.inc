C----------------------------------------------------------------------
C-
C-   Created  19-NOV-1990   Richard Astur
C-      Offline indice ieta-iphi lookup arrays
C----------------------------------------------------------------------
      INTEGER L2JBUF
      PARAMETER(L2JBUF = 30)
      INTEGER L1J_PHI(-L2JBUF+1:NPHIL1+L2JBUF)
      INTEGER L2J_PHI(-L2JBUF+1:NPHIL+L2JBUF)
      INTEGER L1J_ETA(-2*NETAL1-2*L2JBUF:2*NETAL1+2*L2JBUF)
      INTEGER L2J_ETA(-2*NETAL-2*L2JBUF:2*NETAL+2*L2JBUF)
C
      REAL L2J_SIN(-NETAL:NETAL)
      COMMON /L2J_UTIL/ L1J_PHI,L1J_ETA,L2J_PHI,L2J_ETA,L2J_SIN
