C----------------------------------------------------------------------
C- MTC.INC part of the MTC (Muon Tracking in the Calorimeter) package
C-
C- Created  31-JAN-1994   Elizabeth Gallas
C-
C- Purpose:  A call to MTC_MUCALTRACK(VTX,DVTX,ETA,PHI) fills this block
C-      where VTX(3)  is the vertex point in x,y,z (cm)
C-            DVTX(3) is the uncertainty in the vertex coordinates
C-                    (actually, only the z component is used)
C-            ETA,PHI is the input direction eta,phi projected from the
C-                    input vertex at which you believe there is a muon.
C----------------------------------------------------------------------
      COMMON /MTC/
     &        IMTC_MAX, IMTC_HMAX, IMTC_GHMAX, IMTC_LYRMU,
     &        XMTC_DIRCOS(3), XMTC_POINT(3), XMTC_TRES,
     &        XMTC_FRACT, XMTC_HFRACT, XMTC_GHFRACT, XMTC_ECHI,
     &        XMTC_DIRCOS_V(3), XMTC_POINT_V(3), XMTC_TRES_V,
     &        XMTC_EN5, XMTC_EN3, XMTC_EFRACT_H(1:3),
     &        XMTC_ECHI_33, XMTC_FRACT_33, XMTC_ECHI_55, XMTC_FRACT_55,
     &        XMTC_FRACH_33, XMTC_FRACH_55,
     &        XMTC_ECHI2,
     &        XMTC_ETRACK

      REAL    XMTC_DIRCOS, XMTC_POINT, XMTC_TRES,
     &        XMTC_FRACT, XMTC_HFRACT, XMTC_GHFRACT, XMTC_ECHI
      INTEGER IMTC_MAX, IMTC_HMAX, IMTC_GHMAX
      REAL    XMTC_DIRCOS_V, XMTC_POINT_V, XMTC_TRES_V
      REAL    XMTC_EN5, XMTC_EN3, XMTC_EFRACT_H
      REAL    XMTC_ECHI_33, XMTC_FRACT_33, XMTC_ECHI_55, XMTC_FRACT_55,
     &        XMTC_FRACH_33, XMTC_FRACH_55
      INTEGER IMTC_LYRMU
      REAL    XMTC_ECHI2
      REAL    XMTC_ETRACK
C----------------------------------------------------------------------
C- TRACK FIT RESULTS AND THE NUMBER OF CAL CELLS USED TO GET THE FIT
C----------------------------------------------------------------------
C- XMTC_DIRCOS(3) - the direction cosines of the best fit line
C-                  (fit does not include the vertex)
C- XMTC_POINT(3)  - an arbitrary point on the best fit line
C-                  (the direction cosines and one point uniquely defines
C-                  the fit line)
C- XMTC_TRES      - the square root of the sum of the squares of the
C-                  residuals of the fit points to the 3-D fit line
C-                  divided by the number of points used in the fit
C-
C- For some purposes, the fit should include the vertex point:
C-
C- XMTC_DIRCOS_V(3)-the direction cosines of the best fit line
C-                  that includes the input vertex in the fit
C- XMTC_POINT_V(3)- an arbitrary point on the best fit line
C-                  that includes the input vertex in the fit
C- XMTC_TRES      - the square root of the sum of the squares of the
C-                  residuals of the fit points to the 3-D fit line
C-                  divided by the number of points used in the fit
C----------------------------------------------------------------------
C- IMTC_MAX       - the maximum  possible number of layers that can be
C-                  hit at the input eta,phi projected from the vertex
C- XMTC_FRACT     - the fraction of calorimeter layers utilized of the
C-                  maximum  possible (IMTC_MAX)
C- IMTC_HMAX      - the maximum possible number of hadronic layers that
C-                  can be hit (a subset of IMTC_MAX)
C- XMTC_HFRACT    - the fraction of hadronic calorimeter layers utilized of
C-                  the maximum possible (IMTC_HMAX)
C- IMTC_GHMAX     - the maximum possible number of 
C-                  massless gap/icd/hadronic layers that
C-                  can be hit (a subset of IMTC_MAX)
C- XMTC_GHFRACT   - the fraction of massless gap/icd/hadronic calorimeter 
C-                  layers utilized of the maximum possible (IMTC_GHMAX)
C-
C- XMTC_ETRACK    - the calorimeter energy associated with the best track
C-                  found - it is literally the sum of the energy of the 
C-                  hit calorimeter cells used to get the best cal track fit
C- XMTC_ECHI      - the energy chi squared - a measure of the
C-                  MIP-like-ness of the calorimeter track.
C-                  MIP-like particles have a low energy chi square (<2.5)
C-                  and a high fraction of layers hit (100% is ideal).
C-
C- In words:  The energy chi square is the
C- {sum over all hit layers  of
C- [energy seen in each cell - the expected MIP energy for that cell] squared
C- divided by the
C- [rms of the expected MIP energy dist for that particular cell] squared }
C- divided by the number of layers.
C-
C- --------------------------------------------------------------------
C-                     PATTERN RECOGNITION SECTION
C- --------------------------------------------------------------------
C- Calorimetric energy measurement of total energy and punchthough might be
C- XMTC_EN3       - the total calorimeter energy found
C-                  in a calorimeter tower centered on the
C-                  input eta,phi projected from the vertex.
C-                  The tower includes the cell at the central eta,phi
C-                  plus the 8 surrounding calorimeter cells
C-                  in each layer (a 3x3 array of cells in each layer).
C- XMTC_EN5       - the total calorimeter energy found the 5x5 tower
C- XMTC_EFRACT_H(1:3) the fraction of the total calorimeter energy
C-                  found in the last, last two, and last 3 layers
C-                  of the calorimeter
C-                  ie XMTC_EFRACT_H(1) - fraction of energy in last layer
C-                  XMTC_EFRACT_H(2) - fraction of energy in last 2 layers
C-                  XMTC_EFRACT_H(3) - fraction of energy in last 3 layers
C-
C- Is the muon isolated ?  This program looks in the calorimeter cells in
C- a tower centered on the input eta,phi (projected from the vertex). The
C- width of the tower extends .2 in eta and .2 in phi on both sides of the
C- central input eta,phi.  Therefore, the tower looks at up to 25
C- calorimeter  cells in each layer (a 5x5 array in each layer).
C- The following variables can indicate the presence of an isolated muon
C- in the entire calorimeter tower (with 5x5 calorimter cells in each layer),
C- or in the narrower "3x3" tower (using 3x3 calorimter cells in each layer
C- centered about the input eta,phi).
C- XMTC_ECHI_33   - again an energy chi square but this time formed
C-                  using the total energy deposited in each "3x3" layer
C-                  and the energy expected to be deposited by a single
C-                  isolated muon in that 3x3 layer.
C-                  Again, MIP-like particles will have a
C-                  low energy chi square (<2.5)
C-                  and a high fraction of layers hit (100% is ideal).
C- XMTC_FRACT_33  - The fraction of layers hit in the 3x3 tower.
C- XMTC_FRACH_33  - The fraction of hadronic layers hit in the 3x3 tower.
C-
C- XMTC_ECHI_55   - again an energy chi square but this time formed
C-                  using the total energy deposited in each "5x5" layer
C-                  and the energy expected to be deposited by a single
C-                  isolated muon in that 5x5 layer.
C-                  Again, MIP-like particles will have a
C-                  low energy chi square (<2.5)
C-                  and a high fraction of layers hit (100% is ideal).
C- XMTC_FRACT_55  - The fraction of layers hit in the 5x5 tower.
C- XMTC_FRACH_55  - The fraction of hadronic layers hit in the 5x5 tower.
C-
C- When does the muon emerge from the 'jet'?
C- IMTC_LYRMU     - layer number at which the muon seems to emerge from
C-                  a region of higher energy deposition (the 'jet')
C-                  This layer number is zero (0) if ALL calorimeter
C-                  cells used in the fit had an average MIP-like
C-                  energy signature.  It is 18 (calorimeter layer
C-                  numbers range from 1 to 17) if the last layer
C-                  utilized had an energy a bit more than one would
C-                  expect for a MIP-like particle.
C- XMTC_ECHI2     - an energy chi square of the track summed over
C-                  all layers after the muon emerges from the 'jet'
C- -----------------------------------------------------------------------
