C----------------------------------------------------------------------
C-   Created   5-OCT-1992   Alexandre Zinchenko
C-   Updated  16-DEC-1992   A. Zinchenko - remove DATA statement
C----------------------------------------------------------------------
	COMMON /XYVCOM/ PAR(12), ZMEAN, ZSIG1, ZSIG2, XYBO(2), 
     &			XYBN(2)
	DOUBLE PRECISION PAR
	REAL ZMEAN, ZSIG1, ZSIG2, XYBO, XYBN
C-
C-  Common block for XYVERT-package.
C-  PAR(1) - peak value of first Gaussian
C-  PAR(2) - mean value 
C-  PAR(3) - standard deviation
C-  PAR(4) - PAR(6) - the same for second Gaussian
C-  PAR(7) - slope of first Gaussian in Z
C-  PAR(8) - peak value of Gaussian in Z
C-  PAR(9) - PAR(10) - the same for second Gaussian
C-  ZMEAN - mean Z of interaction region
C-  ZSIG1 - left sigma of Z
C-  ZSIG2 - right sigma of Z
C-  XYBO(1) - initial X-coordinate of the beam
C-  XYBO(2) - initial Y-coordinate of the beam
C-  XYBN(1) - new X-coordinate of the beam
C-  XYBN(2) - new Y-coordinate of the beam
C-
