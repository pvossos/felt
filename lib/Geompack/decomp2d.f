C
C     The following code was excerpted from: cvdec2.f
C
      SUBROUTINE CVDEC2(ANGSPC,ANGTOL,NVC,NPOLG,NVERT,MAXVC,MAXHV,MAXPV,
     $   MAXIW,MAXWK,VCL,REGNUM,HVL,PVL,IANG,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXHV,MAXIW,MAXPV,MAXVC,MAXWK,NPOLG,NVC,NVERT
      INTEGER HVL(MAXHV),IWK(MAXIW),PVL(4,MAXPV),REGNUM(MAXHV)
      DOUBLE PRECISION ANGSPC,ANGTOL,IANG(MAXPV),VCL(2,MAXVC),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Decompose general polygonal region (which is decomposed
C        into simple polygons on input) into convex polygons using
C        vertex coordinate list, head vertex list, and polygon vertex
C        list data structures.
C
C     Input parameters:
C        ANGSPC - angle spacing parameter in radians used in controlling
C              vertices to be considered as an endpoint of a separator
C        ANGTOL - angle tolerance parameter in radians used in accepting
C              separator(s)
C        NVC - number of vertex coordinates or positions used in VCL
C              array
C        NPOLG - number of polygonal subregions or positions used in
C              HVL array
C        NVERT - number of polygon vertices or positions used in PVL
C              array
C        MAXVC - maximum size available for VCL array, should be >=
C              number of vertex coordinates required for decomposition 
C        MAXHV - maximum size available for HVL, REGNUM arrays, should
C              be >= number of polygons required for decomposition
C        MAXPV - maximum size available for PVL, IANG arrays; should be
C              >= number of polygon vertices required for decomposition
C        MAXIW - maximum size available for IWK array; should be about
C              3 times maximum number of vertices in any polygon
C        MAXWK - maximum size available for WK array; should be about
C              5 times maximum number of vertices in any polygon
C        VCL(1:2,1:NVC) - vertex coordinate list
C        REGNUM(1:NPOLG) - region numbers
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles; see routine DSPGDC for more details
C        [Note: The data structures should be as output from routine
C              SPDEC2.]
C
C     Updated parameters:
C        NVC,NPOLG,NVERT,VCL,REGNUM,HVL,PVL,IANG
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 4, 5, 6, 7, 206, 207, 208, 209, 210, or 212
C
C     Routines called:
C        INSED2, RESVRT
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER V,W1,W2
      DOUBLE PRECISION PIPTOL
C
C     For each reflex vertex, resolve it with one or two separators
C     and update VCL, HVL, PVL, IANG.
C
      PIPTOL = PI + TOL
      V = 1
   10 CONTINUE
      IF (V .GT. NVERT) RETURN
	 IF (IANG(V) .GT. PIPTOL) THEN
            CALL RESVRT(V,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,
     $         MAXWK,VCL,PVL,IANG,W1,W2,IWK,WK)
	    IF (IERR .NE. 0) RETURN
	    CALL INSED2(V,W1,NPOLG,NVERT,MAXHV,MAXPV,VCL,REGNUM,HVL,
     $         PVL,IANG)
	    IF (IERR .NE. 0) RETURN
            IF (W2 .GT. 0) CALL INSED2(V,W2,NPOLG,NVERT,MAXHV,MAXPV,
     $         VCL,REGNUM,HVL,PVL,IANG)
	    IF (IERR .NE. 0) RETURN
         ENDIF
      V = V + 1
      GO TO 10
      END
C
C     The following code was excerpted from: dsmcpr.f
C
      SUBROUTINE DSMCPR(NHOLE,NVBC,VCL,MAXHV,MAXPV,MAXHO,NVC,NPOLG,
     $   NVERT,NHOLA,REGNUM,HVL,PVL,IANG,HOLV)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXHO,MAXHV,MAXPV,NHOLA,NHOLE,NPOLG,NVC,NVERT
      INTEGER HVL(NHOLE+1),HOLV(MAXHO),NVBC(NHOLE+1),PVL(4,MAXPV)
      INTEGER REGNUM(1)
      DOUBLE PRECISION IANG(MAXPV),VCL(2,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Initialize the polygonal decomposition data structure
C        given a multiply-connected polygonal region with 1 outer
C        boundary curve and 0 or more inner boundary curves of holes.
C
C     Input parameters:
C        NHOLE - number of holes in region
C        NVBC(1:NHOLE+1) - number of vertices per boundary curve; first
C              boundary curve is the outer boundary of the region
C        VCL(1:2,1:NVC) - vertex coordinates of boundary curves in CCW
C              order; NVC = NVBC(1) + ... + NVBC(NHOLE+1); positions 1
C              to NVBC(1) of VCL contain the vertex coordinates of the
C              outer boundary in CCW order; positions NVBC(1)+1 to
C              NVBC(1)+NVBC(2) contain the vertex coordinates of the
C              first hole boundary in CCW order, etc.
C        MAXHV - maximum size available for HVL, REGNUM arrays, should
C              be >= NHOLE + 1
C        MAXPV - maximum size available for PVL, IANG arrays; should be
C               >= NVC
C        MAXHO - maximum size available for HOLV array; should be
C               >= NHOLE*2
C
C     Output parameters:
C        NVC - number of vertex coordinates, set to sum of NVBC(I)
C        NPOLG - number of polygonal subregions, set to 1
C        NVERT - number of vertices in PVL, set to NVC
C        NHOLA - number of attached holes, set to 0
C        REGNUM(1:1) - region number of only subregion, set to 1
C        [Note: Above 4 parameters are for consistency with DSPGDC.]
C        HVL(1:NHOLE+1) - head vertex list; first entry is the head
C              vertex (index in PVL) of outer boundary curve; next
C              NHOLE entries contain the head vertex of a hole
C        PVL(1:4,1:NVC),IANG(1:NVC) - polygon vertex list and interior
C              angles; vertices of outer boundary curve are in CCW order
C              followed by vertices of each hole in CW hole; vertices
C              of each polygon are in a circular linked list; see
C              routine DSPGDC for more details of this data structure
C        HOLV(1:NHOLE*2) - indices in PVL of top and bottom vertices of
C              holes; first (last) NHOLE entries are for top (bottom)
C              vertices; top (bottom) vertices are sorted in decreasing
C              (increasing) lexicographic (y,x) order of coordinates
C
C     Abnormal return:
C        IERR is set to 2, 4, or 5
C
C     Routines called:
C        ANGLE, HOLVRT
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,IV,IVS,J,LV,LVP,LVS,NV,NVS
      DOUBLE PRECISION ANGLE
C
      NVC = 0
      DO 10 I = 1,NHOLE+1
	 NVC = NVC + NVBC(I)
   10 CONTINUE
      NPOLG = 1
      NVERT = NVC
      NHOLA = 0
      REGNUM(1) = 1
      IF (NHOLE + 1 .GT. MAXHV) THEN
	 IERR = 4
	 RETURN
      ELSE IF (NVC .GT. MAXPV) THEN
	 IERR = 5
	 RETURN
      ELSE IF (NHOLE + NHOLE .GT. MAXHO) THEN
	 IERR = 2
	 RETURN
      ENDIF
C
C     Initialize HVL, PVL arrays.
C
   20 CONTINUE
      HVL(1) = 1
      NV = NVBC(1)
      DO 30 I = 1,NV
	 PVL(LOC,I) = I
	 PVL(POLG,I) = 1
	 PVL(SUCC,I) = I + 1
	 PVL(EDGV,I) = 0
   30 CONTINUE
      PVL(SUCC,NV) = 1
      DO 50 J = 1,NHOLE
	 HVL(J+1) = NV + 1
	 NVS = NV + NVBC(J+1)
         DO 40 I = NV+1,NVS
	    PVL(LOC,I) = I
	    PVL(POLG,I) = 1
	    PVL(SUCC,I) = I - 1
	    PVL(EDGV,I) = 0
   40    CONTINUE
         PVL(SUCC,NV+1) = NVS
	 NV = NVS
   50 CONTINUE
C
C     Initialize IANG array.
C
      DO 70 I = 1,NHOLE+1
	 J = HVL(I)
	 LVP = PVL(LOC,J)
	 IV = PVL(SUCC,J)
	 LV = PVL(LOC,IV)
   60    CONTINUE
	    IVS = PVL(SUCC,IV)
	    LVS = PVL(LOC,IVS)
	    IANG(IV) = ANGLE(VCL(1,LVP),VCL(2,LVP),VCL(1,LV),VCL(2,LV),
     $         VCL(1,LVS),VCL(2,LVS))
	    IF (IV .EQ. J) GO TO 70
	    LVP = LV
	    IV = IVS
	    LV = LVS
	 GO TO 60
   70 CONTINUE
C
C     Initialize HOLV array.
C
      IF (NHOLE .GT. 0) CALL HOLVRT(NHOLE,VCL,HVL(2),PVL,HOLV)
      END
C
C     The following code was excerpted from: edght.f
C
      SUBROUTINE EDGHT(A,B,V,N,HTSIZ,MAXEDG,HDFREE,LAST,HT,EDGE,W)
      IMPLICIT LOGICAL (A-Z)
      INTEGER A,B,HDFREE,HTSIZ,LAST,MAXEDG,N,V,W
      INTEGER EDGE(4,MAXEDG),HT(0:HTSIZ-1)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Search in hash table HT for record in EDGE containing
C        key (A,B).
C
C     Input parameters:
C        A,B - vertex indices, > 0, of edge (also key of hash table)
C        V - value associated with edge
C        N - upper bound on A, B
C        HTSIZ - size of hash table HT
C        MAXEDG - maximum size available for EDGE array
C        HDFREE - head pointer to linked list of free entries of EDGE
C              array due to deletions
C        LAST - index of last entry used in EDGE array
C        HT(0:HTSIZ-1) - hash table of head pointers (direct chaining
C              with ordered lists is used)
C        EDGE(1:4,1:MAXEDG) - entries of hash table records;
C              EDGE(1,I) = MIN(A,B); EDGE(2,I) = MAX(A,B);
C              EDGE(3,I) = V; EDGE(4,I) = link
C        [Note: Before first call to this routine, HDFREE, LAST, and
C              entries of HT should be set to 0.]
C
C     Updated parameters:
C        HDFREE,LAST - at least one of these is updated
C        HT,EDGE - if key with A,B is found then this record is deleted
C              from hash table, else record is inserted in hash table
C
C     Output parameters:
C        W - EDGE(3,INDEX), where INDEX is index of record, if found;
C              else 0
C
C     Abnormal return:
C        IERR is set to 1
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER AA,BB,BPTR,K,NEWP,PTR
C
      IF (A .LT. B) THEN
         AA = A
         BB = B
      ELSE
         AA = B
         BB = A
      ENDIF
      K = MOD(AA*N + BB, HTSIZ)
      BPTR = -1
      PTR = HT(K)
   10 CONTINUE
      IF (PTR .NE. 0) THEN
	 IF (EDGE(1,PTR) .GT. AA) THEN
	    GO TO 20
	 ELSE IF (EDGE(1,PTR) .EQ. AA) THEN
	    IF (EDGE(2,PTR) .GT. BB) THEN
	       GO TO 20
	    ELSE IF (EDGE(2,PTR) .EQ. BB) THEN
	       IF (BPTR .EQ. -1) THEN
		  HT(K) = EDGE(4,PTR)
	       ELSE
		  EDGE(4,BPTR) = EDGE(4,PTR)
	       ENDIF
	       EDGE(4,PTR) = HDFREE
	       HDFREE = PTR
	       W = EDGE(3,PTR)
	       RETURN
	    ENDIF
	 ENDIF
	 BPTR = PTR
	 PTR = EDGE(4,PTR)
	 GO TO 10
      ENDIF
   20 CONTINUE
      IF (HDFREE .GT. 0) THEN
	 NEWP = HDFREE
	 HDFREE = EDGE(4,HDFREE)
      ELSE
	 LAST = LAST + 1
	 NEWP = LAST
	 IF (LAST .GT. MAXEDG) THEN
	    IERR = 1
	    RETURN
	 ENDIF
      ENDIF
      IF (BPTR .EQ. -1) THEN
	 HT(K) = NEWP
      ELSE
	 EDGE(4,BPTR) = NEWP
      ENDIF
      EDGE(1,NEWP) = AA
      EDGE(2,NEWP) = BB
      EDGE(3,NEWP) = V
      EDGE(4,NEWP) = PTR
      W = 0
      END
C
C     The following code was excerpted from: fndsep.f
C
      SUBROUTINE FNDSEP(ANGAC1,XR,YR,NVRT,XC,YC,IVIS,THETA,NV,IV,
     $   VCL,PVL,IANG,ANGSEP,I1,I2,WKANG)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,NV,NVRT
      INTEGER IV(0:NV),IVIS(0:NVRT),PVL(4,*)
      DOUBLE PRECISION ANGAC1,ANGSEP,XR,YR,IANG(*),THETA(0:NVRT)
      DOUBLE PRECISION VCL(2,*),WKANG(0:NV),XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Find 1 or 2 separators which can resolve reflex vertex
C        (XR,YR) using a max-min angle criterion from list of vertices
C        in increasing polar angle w.r.t. reflex vertex. Preference
C        is given to 1 separator.
C
C     Input parameters:
C        ANGAC1 - angle tolerance parameter used for preference
C              in accepting one separator
C        XR,YR - coordinates of reflex vertex
C        NVRT - (number of vertices) - 1
C        XC(0:NVRT), YC(0:NVRT) - vertex coordinates of possible
C              endpoints of a separator
C        IVIS(0:NVRT) - contains information about the vertices of
C              XC, YC arrays w.r.t. the polygon vertex list; if
C              IVIS(I) > 0 then vertex (XC(I),YC(I)) has index IVIS(I)
C              in PVL; if IVIS(I) < 0 then vertex (XC(I),YC(I)) is on
C              the edge joining vertices with indices -IVIS(I) and
C              SUCC(-IVIS(I)) in PVL
C        THETA(0:NVRT) - polar angles of vertices in increasing order;
C              THETA(NVRT) is the interior angle of reflex vertex;
C              THETA(I), I >= 0, is the polar angle of (XC(I),YC(I))
C              w.r.t. reflex vertex
C        NV - (number of vertices to be considered as endpoint of a
C              separator) - 1
C        IV(0:NV) - indices of vertices in XC, YC arrays to be
C              considered as endpoint of a separator; angle between
C              consecutive vertices is assumed to be < 180 degrees
C        VCL(1:2,1:*) - vertex coordinate list
C        PVL(1:4,1:*),IANG(1:*) - polygon vertex list, interior angles
C
C     Output parameters:
C        ANGSEP - minimum of the 4 or 7 angles at the boundary
C              resulting from 1 or 2 separators, respectively
C        I1,I2 - indices of endpoints of separators in XC, YC arrays;
C              I2 = -1 if there is only one separator, else I1 < I2
C
C     Working parameters:
C        WKANG(0:NV) - working array for angles
C
C     Routines called:
C        MINANG
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER I,II,K,L,M,NL,NR,P,Q,R
      DOUBLE PRECISION ANG,ANGSP2,MINANG,PHI
C
C     Determine the vertices in the inner cone - indices P to Q.
C
      I = 0
      P = -1
      PHI = THETA(NVRT) - PI + TOL
   10 CONTINUE
      IF (P .GE. 0) GO TO 20
	 IF (THETA(IV(I)) .GE. PHI) THEN
	    P = I
	 ELSE
	    I = I + 1
	 ENDIF
      GO TO 10
   20 CONTINUE
      I = NV
      Q = -1
      PHI = PI - TOL
   30 CONTINUE
      IF (Q .GE. 0) GO TO 40
	 IF (THETA(IV(I)) .LE. PHI) THEN
	    Q = I
	 ELSE
	    I = I - 1
	 ENDIF
      GO TO 30
   40 CONTINUE
C
C     Use the max-min angle criterion to find the best separator
C     in inner cone.
C
      ANGSEP = 0.0D0
      DO 50 I = P,Q
	 K = IV(I)
	 ANG = MINANG(XR,YR,XC(K),YC(K),IVIS(K),THETA(K),THETA(NVRT),
     $      VCL,PVL,IANG)
	 IF (ANG .GT. ANGSEP) THEN
	    ANGSEP = ANG
	    II = IV(I)
	 ENDIF
   50 CONTINUE
      ANGSP2 = ANGSEP
      IF (ANGSEP .GE. ANGAC1) GO TO 110
C
C     If the best separator in inner cone is not 'good' enough,
C     use max-min angle criterion to try to find a better pair
C     of separators from the right and left cones.
C
      NR = 0
      NL = 0
      DO 60 R = 0,P-1
         WKANG(R) = 0.0D0
         IF (THETA(IV(R)) .GT. ANGSEP) THEN
	    K = IV(R)
	    ANG = MINANG(XR,YR,XC(K),YC(K),IVIS(K),THETA(K),THETA(NVRT),
     $         VCL,PVL,IANG)
	    IF (ANG .GT. ANGSEP) THEN
	       NR = NR + 1
	       WKANG(R) = ANG
	    ENDIF
         ENDIF
   60 CONTINUE
      IF (NR .EQ. 0) GO TO 110
      PHI = THETA(NVRT) - ANGSEP
      DO 70 L = Q+1,NV
         WKANG(L) = 0.0D0
         IF (THETA(IV(L)) .LT. PHI) THEN
	    K = IV(L)
	    ANG = MINANG(XR,YR,XC(K),YC(K),IVIS(K),THETA(K),THETA(NVRT),
     $         VCL,PVL,IANG)
	    IF (ANG .GT. ANGSEP) THEN
	       NL = NL + 1
	       WKANG(L) = ANG
	    ENDIF
         ENDIF
   70 CONTINUE
      IF (NL .EQ. 0) GO TO 110
C
C     Check all possible pairs for the best pair of separators
C     in the right and left cones.
C
      M = NV
      DO 100 R = P-1,0,-1
         IF (M .GT. Q .AND. WKANG(R) .GT. ANGSP2) THEN
	    PHI = THETA(IV(R))
   80       CONTINUE
	    IF (M .GT. Q .AND. (WKANG(M) .LE. ANGSP2 .OR.
     $         THETA(IV(M)) - PHI .GT. PI - TOL)) THEN
	       M = M - 1
	       GO TO 80
	    ENDIF
	    DO 90 L = Q+1,M
	       IF (WKANG(L) .GT. ANGSP2) THEN
	          ANG = MIN(THETA(IV(L)) - PHI, WKANG(R), WKANG(L))
	          IF (ANG .GT. ANGSP2) THEN
		     ANGSP2 = ANG
		     I1 = IV(R)
		     I2 = IV(L)
	          ENDIF
	       ENDIF
   90       CONTINUE
         ENDIF
  100 CONTINUE
C
C     Choose 1 or 2 separators based on max-min angle criterion or
C     ANGAC1 parameter.
C
  110 CONTINUE
      IF (ANGSP2 .LE. ANGSEP) THEN
	 I1 = II
	 I2 = -1
      ELSE
	 ANGSEP = ANGSP2
      ENDIF
      END
C
C     The following code was excerpted from: holvrt.f
C
      SUBROUTINE HOLVRT(NHOLE,VCL,HVL,PVL,HOLV)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NHOLE
      INTEGER HOLV(NHOLE*2),HVL(NHOLE),PVL(4,*)
      DOUBLE PRECISION VCL(2,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine top and bottom vertices of holes in polygonal
C        region(s), and sort top vertices in decreasing (y,x) order
C        and bottom vertices in increasing (y,x) order.
C
C     Input parameters:
C        NHOLE - number of holes in region(s)
C        VCL(1:2,1:*) - vertex coordinate list
C        HVL(1:NHOLE) - head vertex list; HVL(I) is index in PVL of
C              head vertex of Ith hole
C        PVL(1:4,1:*) - polygon vertex list; see routine DSPGDC
C
C     Output parameters:
C        HOLV(1:NHOLE*2) - indices in PVL of top and bottom vertices of
C              holes; first (last) NHOLE entries are for top (bottom)
C              vertices; top (bottom) vertices are sorted in decreasing
C              (increasing) lexicographic (y,x) order of coordinates
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER HV,I,IMAX,IMIN,IV,J,LV,NHP1
      DOUBLE PRECISION X,XMAX,XMIN,Y,YMAX,YMIN
C
C     Determine top and bottom vertices of holes.
C
      IMIN = HVL(1)
      IMAX = HVL(1)
      XMIN = VCL(1,PVL(LOC,HVL(1)))
      YMIN = VCL(2,PVL(LOC,HVL(1)))
      XMAX = XMIN
      YMAX = YMIN
      DO 20 I = 1,NHOLE
	 HV = HVL(I)
	 IV = HV
   10    CONTINUE
	    LV = PVL(LOC,IV)
	    IF (IV .EQ. HV) THEN
	       IMIN = IV
	       IMAX = IV
	       XMIN = VCL(1,LV)
	       YMIN = VCL(2,LV)
	       XMAX = XMIN
	       YMAX = YMIN
	    ELSE
	       X = VCL(1,LV)
	       Y = VCL(2,LV)
	       IF (Y .LT. YMIN .OR. Y .EQ. YMIN .AND. X .LT. XMIN) THEN
		  IMIN = IV
		  XMIN = X
		  YMIN = Y
	       ELSE IF (Y .GT. YMAX .OR. Y .EQ. YMAX .AND. X .GT. XMAX)
     $         THEN
		  IMAX = IV
		  XMAX = X
		  YMAX = Y
	       ENDIF
	    ENDIF
	    IV = PVL(SUCC,IV)
	 IF (IV .NE. HV) GO TO 10
	 HOLV(I) = IMAX
	 HOLV(I+NHOLE) = IMIN
   20 CONTINUE
C
C     Use linear insertion sort to sort the top vertices of holes
C     in decreasing (y,x) order, then bottom vertices in increasing
C     (y,x) order. It is assumed NHOLE is small.
C
      DO 40 I = 2,NHOLE
	 HV = HOLV(I)
	 LV = PVL(LOC,HV)
	 X = VCL(1,LV)
	 Y = VCL(2,LV)
	 J = I
   30    CONTINUE
	    IV = HOLV(J-1)
	    LV = PVL(LOC,IV)
	    IF (Y .GT. VCL(2,LV) .OR. Y .EQ. VCL(2,LV) .AND.
     $         X .GT. VCL(1,LV)) THEN
	       HOLV(J) = IV
	       J = J - 1
	       IF (J .GT. 1) GO TO 30
	    ENDIF
	 HOLV(J) = HV
   40 CONTINUE
C
      NHP1 = NHOLE + 1
      DO 60 I = NHP1+1,NHOLE+NHOLE
	 HV = HOLV(I)
	 LV = PVL(LOC,HV)
	 X = VCL(1,LV)
	 Y = VCL(2,LV)
	 J = I
   50    CONTINUE
	    IV = HOLV(J-1)
	    LV = PVL(LOC,IV)
	    IF (Y .LT. VCL(2,LV) .OR. Y .EQ. VCL(2,LV) .AND.
     $         X .LT. VCL(1,LV)) THEN
	       HOLV(J) = IV
	       J = J - 1
	       IF (J .GT. NHP1) GO TO 50
	    ENDIF
	 HOLV(J) = HV
   60 CONTINUE
      END
C
C     The following code was excerpted from: insed2.f
C
      SUBROUTINE INSED2(V,W,NPOLG,NVERT,MAXHV,MAXPV,VCL,REGNUM,HVL,
     $   PVL,IANG)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXHV,MAXPV,NPOLG,NVERT,V,W
      INTEGER HVL(MAXHV),PVL(4,MAXPV)
      INTEGER REGNUM(MAXHV)
      DOUBLE PRECISION IANG(MAXPV),VCL(2,*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Insert edge joining vertices V, W into head vertex
C        list and polygon vertex list data structures.
C
C     Input parameters:
C        V,W - indices in PVL of vertices which are the endpoints
C              of an edge to be added to decomposition
C        NPOLG - number of positions used in HVL array
C        NVERT - number of positions used in PVL array
C        MAXHV - maximum size available for HVL array
C        MAXPV - maximum size available for PVL array
C        VCL(1:2,1:*) - vertex coordinate list
C        REGNUM(1:NPOLG) - region numbers
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles
C
C     Updated parameters:
C        REGNUM,HVL,PVL,IANG
C
C     Abnormal return:
C        IERR is set to 4 or 5
C
C     Routines called:
C        ANGLE
C
      INTEGER IERR,IPRT,MSGLVL
      COMMON /GERROR/ IERR
      COMMON /GPRINT/ IPRT,MSGLVL
      SAVE /GERROR/,/GPRINT/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,L,LV,LW,VV,WW
      DOUBLE PRECISION ANGLE
C
      IF (NPOLG .GE. MAXHV) THEN
	 IERR = 4
	 RETURN
      ELSE IF (NVERT+2 .GT. MAXPV) THEN
	 IERR = 5
	 RETURN
      ENDIF
C
C     Split linked list of vertices of the polygon containing vertices
C     V and W into two linked list of vertices of polygons with common
C     edge joining V and W.
C
      NVERT = NVERT + 2
      VV = NVERT - 1
      WW = NVERT
      LV = PVL(LOC,V)
      LW = PVL(LOC,W)
      PVL(LOC,VV) = LV
      PVL(LOC,WW) = LW
      PVL(POLG,WW) = PVL(POLG,V)
      PVL(SUCC,VV) = PVL(SUCC,V)
      PVL(SUCC,WW) = PVL(SUCC,W)
      PVL(SUCC,V) = WW
      PVL(SUCC,W) = VV
      PVL(EDGV,VV) = PVL(EDGV,V)
      PVL(EDGV,WW) = PVL(EDGV,W)
      PVL(EDGV,V) = W
      PVL(EDGV,W) = V
      IF (PVL(EDGV,VV) .GT. 0) PVL(EDGV,PVL(EDGV,VV)) = VV
      IF (PVL(EDGV,WW) .GT. 0) PVL(EDGV,PVL(EDGV,WW)) = WW
      L = PVL(LOC,PVL(SUCC,VV))
      IANG(VV) = ANGLE(VCL(1,LW),VCL(2,LW),VCL(1,LV),VCL(2,LV),
     $   VCL(1,L),VCL(2,L))
      IANG(V) = IANG(V) - IANG(VV)
      L = PVL(LOC,PVL(SUCC,WW))
      IANG(WW) = ANGLE(VCL(1,LV),VCL(2,LV),VCL(1,LW),VCL(2,LW),
     $   VCL(1,L),VCL(2,L))
      IANG(W) = IANG(W) - IANG(WW)
      NPOLG = NPOLG + 1
      I = VV
   10 CONTINUE
	 PVL(POLG,I) = NPOLG
	 I = PVL(SUCC,I)
      IF (I .NE. VV) GO TO 10
      HVL(PVL(POLG,V)) = V
      HVL(NPOLG) = VV
      REGNUM(NPOLG) = REGNUM(PVL(POLG,V))
C
      END
C
C     The following code was excerpted from: insvr2.f
C
      SUBROUTINE INSVR2(XI,YI,WP,NVC,NVERT,MAXVC,MAXPV,VCL,PVL,IANG,W)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXPV,MAXVC,NVC,NVERT,PVL(4,MAXPV),W,WP
      DOUBLE PRECISION IANG(MAXPV),VCL(2,MAXVC),XI,YI
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Insert point (XI,YI) into vertex coordinate list and
C        polygon vertex list data structures.
C
C     Input parameters:
C        XI,YI - coordinates of point to be inserted
C        WP - index of vertex in PVL which is to be the predecessor
C              vertex of the inserted vertex
C        NVC - number of positions used in VCL array
C        NVERT - number of positions used in PVL array
C        MAXVC - maximum size available for VCL array
C        MAXPV - maximum size available for PVL array
C        VCL(1:2,1:NVC) - vertex coordinate list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles
C
C     Updated parameters:
C        NVC,NVERT,VCL,PVL,IANG
C
C     Output parameter:
C        W - index of inserted vertex in PVL
C
C     Abnormal return:
C        IERR is set to 3 or 5
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER WS,WW,WWP,WWS
C
      IF (NVC .GE. MAXVC) THEN
	 IERR = 3
	 RETURN
      ELSE IF (NVERT+2 .GT. MAXPV) THEN
	 IERR = 5
	 RETURN
      ENDIF
C
C     Update linked list of vertices of polygon containing vertex WP.
C
      NVC = NVC + 1
      VCL(1,NVC) = XI
      VCL(2,NVC) = YI
      WS = PVL(SUCC,WP)
      NVERT = NVERT + 1
      W = NVERT
      PVL(LOC,W) = NVC
      PVL(POLG,W) = PVL(POLG,WP)
      PVL(SUCC,WP) = W
      PVL(SUCC,W) = WS
      IANG(W) = PI
      PVL(EDGV,W) = PVL(EDGV,WP)
C
C     If edge containing (XI,YI) is shared by another polygon,
C     then also update linked list of vertices of that polygon.
C
      IF (PVL(EDGV,WP) .GT. 0) THEN
	 WWS = PVL(EDGV,WP)
         WWP = PVL(SUCC,WWS)
         NVERT = NVERT + 1
         WW = NVERT
         PVL(LOC,WW) = NVC
         PVL(POLG,WW) = PVL(POLG,WWS)
         PVL(SUCC,WWS) = WW
         PVL(SUCC,WW) = WWP
         IANG(WW) = PI
         PVL(EDGV,WP) = WW
	 PVL(EDGV,WW) = WP
	 PVL(EDGV,WWS) = W
      ENDIF
      END
C
C     The following code was excerpted from: jnhole.f
C
      SUBROUTINE JNHOLE(ITOPHV,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,
     $   MAXIW,MAXWK,VCL,HVL,PVL,IANG,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER ITOPHV,MAXIW,MAXPV,MAXVC,MAXWK,NVC,NVERT
      INTEGER HVL(*),IWK(MAXIW),PVL(4,MAXPV)
      DOUBLE PRECISION ANGSPC,ANGTOL,IANG(MAXPV),VCL(2,MAXVC),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Join hole boundary to boundary of polygon containing hole
C        by finding a cut edge originating from the top vertex of hole
C        to a point on outer polygon boundary above it.
C
C     Input parameters:
C        ITOPHV - index in PVL of top vertex of hole
C        ANGSPC - angle spacing parameter used in controlling the
C              vertices to be considered as an endpoint of a separator
C        ANGTOL - angle tolerance parameter used in accepting
C              separator(s)
C        NVC - number of positions used in VCL array
C        NVERT - number of positions used in PVL array
C        MAXVC - maximum size available for VCL array
C        MAXPV - maximum size available for PVL array
C        MAXIW - maximum size available for IWK array; should be about
C              3 times number of vertices in outer polygon
C        MAXWK - maximum size available for WK array; should be about
C              5 times number of vertices in outer polygon
C        VCL(1:2,1:NVC) - vertex coordinate list
C        HVL(1:*) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles
C
C     Updated parameters:
C        NVC,NVERT,VCL,PVL,IANG
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 5, 6, 7, 206 to 210, 212, 218, or 219
C
C     Routines called:
C        ANGLE, RESVRT
C
      INTEGER IERR,IPRT,MSGLVL
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      COMMON /GPRINT/ IPRT,MSGLVL
      SAVE /GERROR/,/GCONST/,/GPRINT/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER HV,ILFT,IPOLY,IRGT,IV,IVS,L,LV,LW,SUCCIL,SUCCIR
      INTEGER VP,VR,VS,VV,W,WW
      DOUBLE PRECISION ANGLE,DY,S,SLFT,SRGT,XINT,XLFT,XRGT,XT,XV,XVS
      DOUBLE PRECISION YLFT,YRGT,YT,YTMTOL,YTPTOL,YV,YVS
C
      IF (NVC+3 .GT. MAXVC) THEN
	 IERR = 3
	 RETURN
      ELSE IF (NVERT+5 .GT. MAXPV) THEN
	 IERR = 5
	 RETURN
      ENDIF
C
C     Determine 'closest' vertices on outer boundary which are to the
C     left and right of top vertex of hole and on the horizontal line
C     through top vertex. The two closest vertices must be on edges
C     which intersect the horizontal line and are partially above the
C     line. Ties are broken (in the case of a vertex on a cut edge)
C     by choosing the vertex on the edge of maximum or minimum dx/dy
C     slope depending on whether the vertex is to the left or right
C     of top vertex, respectively.
C
      IPOLY = PVL(POLG,ITOPHV)
      LV = PVL(LOC,ITOPHV)
      XT = VCL(1,LV)
      YT = VCL(2,LV)
      DY = 0.0D0
      HV = HVL(IPOLY)
      IV = HV
      YV = VCL(2,PVL(LOC,IV))
      SLFT = 0.0
      SRGT = 0.0
   10 CONTINUE
	 IV = PVL(SUCC,IV)
	 YVS = VCL(2,PVL(LOC,IV))
	 DY = MAX(DY,ABS(YVS-YV))
	 YV = YVS
      IF (IV .NE. HV) GO TO 10
      YTMTOL = YT - TOL*DY
      YTPTOL = YT + TOL*DY
      ILFT = 0
      IRGT = 0
      XLFT = 0.0D0
      XRGT = 0.0D0
      HV = HVL(IPOLY)
      IV = HV
      LV = PVL(LOC,IV)
      XV = VCL(1,LV)
      YV = VCL(2,LV)
   20 CONTINUE
	 IVS = PVL(SUCC,IV)
	 LV = PVL(LOC,IVS)
	 XVS = VCL(1,LV)
	 YVS = VCL(2,LV)
	 IF (YV .LE. YTPTOL .AND. YVS .GT. YTPTOL) THEN
	    IF (YV .GE. YTMTOL) THEN
	       IF (XV .GT. XT) THEN
		  IF (XV .LT. XRGT .OR. IRGT .EQ. 0) THEN
		     IRGT = IV
		     XRGT = XV
		     YRGT = YV
		     SRGT = (XVS - XV)/(YVS - YV)
		  ELSE IF (XV .EQ. XRGT) THEN
		     S = (XVS - XV)/(YVS - YV)
		     IF (S .LT. SRGT) THEN
			IRGT = IV
		        YRGT = YV
			SRGT = S
		     ENDIF
		  ENDIF
	       ENDIF
	    ELSE
	       XINT = (YT - YV)*(XVS - XV)/(YVS - YV) + XV
	       IF (XINT .GT. XT) THEN
		  IF (XINT .LT. XRGT .OR. IRGT .EQ. 0) THEN
		     IRGT = IV
		     XRGT = XINT
		     YRGT = YT
		  ENDIF
	       ENDIF
	    ENDIF
	 ELSE IF (YV .GT. YTPTOL .AND. YVS .LE. YTPTOL) THEN
	    IF (YVS .GE. YTMTOL) THEN
	       IF (XVS .LT. XT) THEN
		  IF (XVS .GT. XLFT .OR. ILFT .EQ. 0) THEN
		     ILFT = IV
		     XLFT = XVS
		     YLFT = YVS
		     SLFT = (XVS - XV)/(YVS - YV)
		  ELSE IF (XVS .EQ. XLFT) THEN
		     S = (XVS - XV)/(YVS - YV)
		     IF (S .GT. SLFT) THEN
			ILFT = IV
		        YLFT = YVS
			SLFT = S
		     ENDIF
		  ENDIF
	       ENDIF
	    ELSE
	       XINT = (YT - YV)*(XVS - XV)/(YVS - YV) + XV
	       IF (XINT .LT. XT) THEN
		  IF (XINT .GT. XLFT .OR. ILFT .EQ. 0) THEN
		     ILFT = IV
		     XLFT = XINT
		     YLFT = YT
		  ENDIF
	       ENDIF
	    ENDIF
	 ENDIF
	 IV = IVS
	 XV = XVS
	 YV = YVS
      IF (IV .NE. HV) GO TO 20
C
      IF (ILFT .EQ. 0 .OR. IRGT .EQ. 0) THEN
	 IERR = 218
	 RETURN
      ENDIF
C
C     Temporarily modify PVL to pass the subregion 'above' top vertex
C     of hole to routine RESVRT. The top vertex is the reflex vertex
C     passed to RESVRT (in the temporary subregion, it has interior
C     angle PI). This causes one separator to be chosen by RESVRT
C     and its other endpoint is above the top vertex.
C
      SUCCIL = PVL(SUCC,ILFT)
      SUCCIR = PVL(SUCC,IRGT)
      VCL(1,NVC+2) = XLFT
      VCL(2,NVC+2) = YLFT
      VCL(1,NVC+3) = XRGT
      VCL(2,NVC+3) = YRGT
      VP = NVERT + 3
      VR = NVERT + 4
      VS = NVERT + 5
      IANG(VR) = ANGLE(XLFT,YLFT,XT,YT,XRGT,YRGT)
      IF (IANG(VR) .LT. PI - TOL .OR. IANG(VR) .GT. PI + TOL) THEN
	 IERR = 219
	 RETURN
      ENDIF
      PVL(LOC,VP) = NVC + 2
      PVL(POLG,VP) = IPOLY
      PVL(SUCC,VP) = VR
      PVL(EDGV,VP) = 0
      PVL(LOC,VR) = PVL(LOC,ITOPHV)
      PVL(POLG,VR) = IPOLY
      PVL(SUCC,VR) = VS
      PVL(EDGV,VR) = 0
      PVL(LOC,VS) = NVC + 3
      PVL(POLG,VS) = IPOLY
      PVL(SUCC,VS) = SUCCIR
      PVL(EDGV,VS) = PVL(EDGV,IRGT)
      PVL(SUCC,ILFT) = VP
      LV = PVL(LOC,ILFT)
      IANG(VP) = ANGLE(VCL(1,LV),VCL(2,LV),XLFT,YLFT,XT,YT)
      LV = PVL(LOC,SUCCIR)
      IANG(VS) = ANGLE(XT,YT,XRGT,YRGT,VCL(1,LV),VCL(2,LV))
      W = 0
      CALL RESVRT(VR,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,MAXWK,
     $   VCL,PVL,IANG,W,WW,IWK,WK)
C
C     Remove temporary modification to PVL. There are three cases
C     depending on where the endpoint of separator is located:
C     successor of closest vertex to the right of top vertex,
C     predecessor of closest vertex to the left of top vertex,
C     or neither of these.
C
      IF (PVL(SUCC,VS) .EQ. W) THEN
	 PVL(SUCC,ILFT) = SUCCIL
	 PVL(SUCC,IRGT) = W
	 PVL(EDGV,IRGT) = PVL(EDGV,VS)
         IF (PVL(EDGV,IRGT) .GT. 0) PVL(EDGV,PVL(EDGV,IRGT)) = IRGT
      ELSE IF (PVL(SUCC,ILFT) .EQ. W) THEN
	 PVL(SUCC,W) = SUCCIL
      ELSE
	 PVL(SUCC,ILFT) = SUCCIL
      ENDIF
      IF (IERR .NE. 0) RETURN
C
C     Update PVL with cut edge, i.e. join linked lists of vertices
C     of the hole polygon and the outer boundary polygon into one
C     linked list of vertices by adding the cut edge from the top
C     vertex of hole to the vertex on the outer boundary.
C
      NVERT = NVERT + 2
      VV = NVERT - 1
      WW = NVERT
      LV = PVL(LOC,ITOPHV)
      LW = PVL(LOC,W)
      PVL(LOC,VV) = LV
      PVL(LOC,WW) = LW
      PVL(POLG,VV) = IPOLY
      PVL(POLG,WW) = IPOLY
      PVL(SUCC,VV) = PVL(SUCC,ITOPHV)
      PVL(SUCC,WW) = PVL(SUCC,W)
      PVL(SUCC,ITOPHV) = WW
      PVL(SUCC,W) = VV
      PVL(EDGV,VV) = PVL(EDGV,ITOPHV)
      PVL(EDGV,WW) = PVL(EDGV,W)
      PVL(EDGV,ITOPHV) = W
      PVL(EDGV,W) = ITOPHV
      IF (PVL(EDGV,VV) .GT. 0) PVL(EDGV,PVL(EDGV,VV)) = VV
      IF (PVL(EDGV,WW) .GT. 0) PVL(EDGV,PVL(EDGV,WW)) = WW
      L = PVL(LOC,PVL(SUCC,VV))
      IANG(VV) = ANGLE(VCL(1,LW),VCL(2,LW),VCL(1,LV),VCL(2,LV),
     $   VCL(1,L),VCL(2,L))
      IANG(ITOPHV) = IANG(ITOPHV) - IANG(VV)
      L = PVL(LOC,PVL(SUCC,WW))
      IANG(WW) = ANGLE(VCL(1,LV),VCL(2,LV),VCL(1,LW),VCL(2,LW),
     $   VCL(1,L),VCL(2,L))
      IANG(W) = IANG(W) - IANG(WW)
C
      END
C
C     The following code was excerpted from: minang.f
C
      DOUBLE PRECISION FUNCTION MINANG(XR,YR,XS,YS,IND,ALPHA,THETA,
     $   VCL,PVL,IANG)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IND,PVL(4,*)
      DOUBLE PRECISION ALPHA,IANG(*),THETA,VCL(2,*),XR,XS,YR,YS
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine the minimum of the 4 angles at the boundary
C        resulting from using edge joining vertices (XR,YR) and
C        (XS,YS) as a separator.
C
C     Input parameters:
C        XR,YR - coordinates of reflex vertex
C        XS,YS - coordinates of other endpoint of possible separator
C        IND - if positive then (XS,YS) has index IND in PVL; else
C              (XS,YS) is on edge joining vertices with indices -IND
C              and SUCC(-IND) in PVL
C        ALPHA - polar angle of (XS,YS) w.r.t. (XR,YR)
C        THETA - interior angle at reflex vertex
C        VCL(1:2,1:*) - vertex coordinate list
C        PVL(1:4,1:*),IANG(1:*) - polygon vertex list, interior angles
C
C     Returned function value:
C        MINANG - minimum of the 4 angles in radians
C
C     Routines called:
C        ANGLE
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER J,L
      DOUBLE PRECISION ANG,ANGLE,BETA1
C
      IF (IND .GT. 0) THEN
	 J = PVL(SUCC,IND)
	 ANG = IANG(IND)
      ELSE
	 J = PVL(SUCC,-IND)
	 ANG = PI
      ENDIF
      L = PVL(LOC,J)
      BETA1 = ANGLE(XR,YR,XS,YS,VCL(1,L),VCL(2,L))
      MINANG = MIN(ALPHA, THETA - ALPHA, ANG - BETA1, BETA1)
      END
C
C     The following code was excerpted from: resvrt.f
C
      SUBROUTINE RESVRT(VR,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,
     $   MAXWK,VCL,PVL,IANG,W1,W2,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXIW,MAXPV,MAXVC,MAXWK,NVC,NVERT,VR,W1,W2
      INTEGER IWK(MAXIW),PVL(4,MAXPV)
      DOUBLE PRECISION ANGSPC,ANGTOL,IANG(MAXPV),VCL(2,MAXVC),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Resolve reflex vertex of simply connected polygon with
C        one or two separators. The reflex vertex must be a 'simple'
C        vertex of the polygon.
C
C     Input parameters:
C        VR - index in PVL of reflex vertex
C        ANGSPC - angle spacing parameter used in controlling the
C              vertices to be considered as an endpoint of a separator
C        ANGTOL - angle tolerance parameter used in accepting
C              separator(s)
C        NVC - number of positions used in VCL array
C        NVERT - number of positions used in PVL array
C        MAXVC - maximum size available for VCL array
C        MAXPV - maximum size available for PVL array
C        MAXIW - maximum size available for IWK array; should be about
C              3 times number of vertices in polygon
C        MAXWK - maximum size available for WK array; should be about
C              5 times number of vertices in polygon
C        VCL(1:2,1:NVC) - vertex coordinate list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles
C
C     Updated parameters:
C        NVC,NVERT,VCL,PVL,IANG - are updated if INSVR2 is called
C
C     Output parameters:
C        W1 - index in PVL of vertex which is the endpoint of separator
C              in inner cone or right cone w.r.t. reflex vertex
C        W2 - 0 if there is only one separator; else index in PVL of
C              vertex which is endpoint of 2nd separator in left cone
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 5, 6, 7, 206, 207, 208, 209, 210, or 212
C
C     Routines called:
C        FNDSEP, INSVR2, VISPOL, VISVRT, VORNBR
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,I1,I2,IVIS,IVOR,IVRT,L,MAXN,NVIS,NVOR,NVRT,NVSVRT
      INTEGER THETA,V,WKANG,XC,XVOR,YC,YVOR
      DOUBLE PRECISION ANGSEP,XR,YR
C
C     Determine number of vertices in polygon containing reflex vertex.
C
      NVRT = 0
      V = VR
   10 CONTINUE
         V = PVL(SUCC,V)
         IF (V .NE. VR) THEN
	    NVRT = NVRT + 1
	    GO TO 10
         ENDIF
      MAXN = NVRT + INT(IANG(VR)/ANGSPC)
      L = PVL(LOC,VR)
      XR = VCL(1,L)
      YR = VCL(2,L)
C
C     Set up work arrays for routine VISPOL, and determine whether there
C     is enough workspace. XC, YC are d.p. arrays of length NVRT in WK,
C     used for the coordinates of the polygon containing the reflex
C     vertex. MAXN positions are reserved for XC, YC since this is the
C     maximum space required by routine VISVRT. IVIS is an integer array
C     of length MAXN in IWK. IVRT is an integer array of length NVRT in
C     IWK used temporarily for storing indices of vertices in PVL.
C
      IF (MAXN + NVRT .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ELSE IF (MAXN + MAXN .GT. MAXWK) THEN
	 IERR = 7
	 RETURN
      ENDIF
      IVIS = 1
      IVRT = IVIS + MAXN
      XC = 1
      YC = XC + MAXN
      V = PVL(SUCC,VR)
      DO 20 I = 0,NVRT-1
	 L = PVL(LOC,V)
	 WK(XC+I) = VCL(1,L)
	 WK(YC+I) = VCL(2,L)
	 IWK(IVRT+I) = V
	 V = PVL(SUCC,V)
   20 CONTINUE
      CALL VISPOL(XR,YR,NVRT-1,WK(XC),WK(YC),NVIS,IWK(IVIS))
      IF (IERR .NE. 0) RETURN
C
C     XC, YC now contain visibility polygon coordinates. Update MAXN
C     and set up d.p. array THETA of length MAXN in WK for routine
C     VISVRT. Elements of IVIS are changed to indices of PVL after call.
C
      MAXN = MAXN - NVRT + NVIS + 1
      THETA = YC + MAXN
      IF (THETA + MAXN - 1 .GT. MAXWK) THEN
	 IERR = 7
	 RETURN
      ENDIF
      CALL VISVRT(ANGSPC,XR,YR,NVIS,WK(XC),WK(YC),IWK(IVIS),MAXN-1,
     $   NVSVRT,WK(THETA))
      WK(THETA+NVSVRT) = IANG(VR)
      DO 30 I = IVIS,IVIS+NVSVRT
	 L = IWK(I)
	 IF (L .GE. 0) THEN
	    IWK(I) = IWK(IVRT+L)
	 ELSE
	    IWK(I) = -IWK(IVRT-L-1)
	 ENDIF
   30 CONTINUE
C
C     XC, YC now contain coord. of visible vertices to be considered
C     as an endpoint of a separator. Set up work arrays for routine
C     VORNBR. Integer array IVOR and d.p. arrays XVOR, YVOR, each of
C     length NVSVRT+1, are added at the end of IWK and WK arrays.
C
      IVOR = IVIS + NVSVRT + 1
      XVOR = THETA + NVSVRT + 1
      YVOR = XVOR + NVSVRT + 1
      IF (IVOR + NVSVRT .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ELSE IF (YVOR + NVSVRT .GT. MAXWK) THEN
	 IERR = 7
	 RETURN
      ENDIF
      CALL VORNBR(XR,YR,NVSVRT,WK(XC),WK(YC),NVOR,IWK(IVOR),WK(XVOR),
     $   WK(YVOR))
      IF (IERR .NE. 0) RETURN
C
C     Set up d.p. array WKANG of length NVOR+1 <= NVSVRT+1 in WK for
C     routine FNDSEP. Only Voronoi neighbours are considered as an
C     endpoint of a separator in the first call to FNDSEP. If the
C     minimum angle created at the boundary by the separator(s) is too
C     small, then a second call is made to FNDSEP in which all visible
C     vertices are considered as an endpoint of a separator.
C
      WKANG = XVOR
      IF (IWK(IVOR+NVOR) .EQ. NVSVRT) NVOR = NVOR - 1
      IF (IWK(IVOR) .EQ. 0) THEN
	 IVOR = IVOR + 1
	 NVOR = NVOR - 1
      ENDIF
      CALL FNDSEP(ANGTOL+ANGTOL,XR,YR,NVSVRT,WK(XC),WK(YC),IWK(IVIS),
     $   WK(THETA),NVOR,IWK(IVOR),VCL,PVL,IANG,ANGSEP,I1,I2,WK(WKANG))
      IF (ANGSEP .LT. ANGTOL) THEN
	 IVRT = IVIS + NVSVRT + 1
	 DO 40 I = 1,NVSVRT-1
	    IWK(IVRT+I-1) = I
   40    CONTINUE
         CALL FNDSEP(ANGTOL+ANGTOL,XR,YR,NVSVRT,WK(XC),WK(YC),IWK(IVIS),
     $      WK(THETA),NVSVRT-2,IWK(IVRT),VCL,PVL,IANG,ANGSEP,I1,I2,
     $      WK(WKANG))
      ENDIF
C
C     Insert endpoint(s) of separator(s) in vertex coordinate list and
C     polygon vertex list data structures, if they are not yet there.
C
      IF (I2 .EQ. -1) THEN
	 W2 = 0
      ELSE IF (IWK(IVIS+I2) .LT. 0) THEN
	 CALL INSVR2(WK(XC+I2),WK(YC+I2),-IWK(IVIS+I2),NVC,NVERT,MAXVC,
     $      MAXPV,VCL,PVL,IANG,W2)
	 IF (IERR .NE. 0) RETURN
      ELSE
	 W2 = IWK(IVIS+I2)
      ENDIF
      IF (IWK(IVIS+I1) .LT. 0) THEN
	 CALL INSVR2(WK(XC+I1),WK(YC+I1),-IWK(IVIS+I1),NVC,NVERT,MAXVC,
     $      MAXPV,VCL,PVL,IANG,W1)
	 IF (IERR .NE. 0) RETURN
      ELSE
	 W1 = IWK(IVIS+I1)
      ENDIF
      END
C
C     The following code was excerpted from: spdec2.f
C
      SUBROUTINE SPDEC2(ANGSPC,ANGTOL,NVC,NPOLG,NVERT,NHOLE,NHOLA,MAXVC,
     $   MAXHV,MAXPV,MAXIW,MAXWK,HOLV,VCL,REGNUM,HVL,PVL,IANG,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER MAXHV,MAXIW,MAXPV,MAXVC,MAXWK,NHOLA,NHOLE,NPOLG,NVC,NVERT
      INTEGER HOLV(*),HVL(MAXHV),IWK(MAXIW),PVL(4,MAXPV),REGNUM(MAXHV)
      DOUBLE PRECISION ANGSPC,ANGTOL,IANG(MAXPV),VCL(2,MAXVC),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Decompose general polygonal region with interfaces and
C        holes into simple polygons using vertex coordinate list,
C        head vertex list, and polygon vertex list data structures.
C
C     Input parameters:
C        ANGSPC - angle spacing parameter in radians used in controlling
C              vertices to be considered as an endpoint of a separator
C        ANGTOL - angle tolerance parameter in radians used in accepting
C              separator(s)
C        NVC - number of vertex coordinates or positions used in VCL
C              array
C        NPOLG - number of polygonal subregions or positions used in
C              HVL array
C        NVERT - number of polygon vertices or positions used in PVL
C              array
C        NHOLE - number of holes and hole interfaces
C        NHOLA - number of 'attached' holes; these holes are attached
C              to the outer boundary of a subregion through vertices
C              or cut interfaces and have their edges in consecutive
C              order on the boundary
C        MAXVC - maximum size available for VCL array, should be >=
C              number of vertex coordinates required for decomposition
C        MAXHV - maximum size available for HVL, REGNUM arrays, should
C              be >= number of polygons required for decomposition
C        MAXPV - maximum size available for PVL, IANG arrays; should be
C              >= number of polygon vertices required for decomposition
C        MAXIW - maximum size available for IWK array; should be about
C              3 times maximum number of vertices in any polygon
C        MAXWK - maximum size available for WK array; should be about
C              5 times maximum number of vertices in any polygon
C        HOLV(1:NHOLE*2+NHOLA) - indices in PVL of bottom or top vertex
C              of holes; first (next) NHOLE entries are for top (bottom)
C              vertices of holes and hole interfaces, with top (bottom)
C              vertices sorted in decreasing (increasing) lexicographic
C              (y,x) order of coord; last NHOLA entries are for attached
C              holes; if bottom vertex of attached hole is a simple
C              vertex of boundary curve containing the hole then entry
C              contains index of bottom vertex otherwise entry contains
C              index of top vertex (which is simple)
C        VCL(1:2,1:NVC) - vertex coordinate list
C        REGNUM(1:NPOLG) - region numbers
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles; see routine DSPGDC for more details
C        [Note: The data structures should be as output from routines
C              DSMCPR or DSPGDC.]
C
C     Updated parameters:
C        NVC,NPOLG,NVERT,VCL,REGNUM,HVL,PVL,IANG
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 4, 5, 6, 7, 206 to 210, 212, 218, or 219
C
C     Routines called:
C        INSED2, JNHOLE, RESVRT
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,J,P,VR,W1,W2
      DOUBLE PRECISION PIPTOL
      LOGICAL CI,CJ
C
C     For each simple hole, find cut edge from top vertex of hole to
C     a point on the outer boundary above top vertex, and update
C     VCL, HVL, PVL, IANG.
C
      PIPTOL = PI + TOL
      DO 10 I = 1,NHOLE
	 CALL JNHOLE(HOLV(I),ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,
     $      MAXWK,VCL,HVL,PVL,IANG,IWK,WK)
	 IF (IERR .NE. 0) RETURN
   10 CONTINUE
C
C     Resolve remaining vertices in HOLV array if they are reflex
C     vertices. These vertices may no longer be reflex if they are the
C     endpoint of a cut edge from the top vertex of another hole or
C     of a previous separator.
C
      DO 20 I = NHOLE+1,NHOLE+NHOLE+NHOLA
	 VR = HOLV(I)
	 IF (IANG(VR) .GT. PIPTOL) THEN
            CALL RESVRT(VR,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,
     $         MAXWK,VCL,PVL,IANG,W1,W2,IWK,WK)
	    IF (IERR .NE. 0) RETURN
	    CALL INSED2(VR,W1,NPOLG,NVERT,MAXHV,MAXPV,VCL,REGNUM,HVL,
     $         PVL,IANG)
	    IF (IERR .NE. 0) RETURN
            IF (W2 .GT. 0) CALL INSED2(VR,W2,NPOLG,NVERT,MAXHV,MAXPV,
     $         VCL,REGNUM,HVL,PVL,IANG)
	    IF (IERR .NE. 0) RETURN
         ENDIF
   20 CONTINUE
      IF (NHOLA .EQ. 0) RETURN
C
C     Check that polygons are simple. If polygon is simply-connected and
C     not simple then find a simple reflex vertex in polygon to resolve.
C
      P = 1
   30 CONTINUE
      IF (P .GT. NPOLG) RETURN
	 I = HVL(P)
   40    CONTINUE
	    IF (PVL(POLG,PVL(EDGV,I)) .EQ. P) GO TO 50
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(P)) GO TO 40
	 P = P + 1
	 GO TO 30
   50    CONTINUE
	 CI = .TRUE.
   60    CONTINUE
	    J = PVL(SUCC,I)
	    CJ = (PVL(POLG,PVL(EDGV,J)) .EQ. P)
	 IF (CI .OR. CJ .OR. IANG(J) .LE. PIPTOL) THEN
	    I = J
	    CI = CJ
	    GO TO 60
	 ENDIF
	 VR = J
         CALL RESVRT(VR,ANGSPC,ANGTOL,NVC,NVERT,MAXVC,MAXPV,MAXIW,
     $      MAXWK,VCL,PVL,IANG,W1,W2,IWK,WK)
	 IF (IERR .NE. 0) RETURN
	 CALL INSED2(VR,W1,NPOLG,NVERT,MAXHV,MAXPV,VCL,REGNUM,HVL,
     $      PVL,IANG)
	 IF (IERR .NE. 0) RETURN
         IF (W2 .GT. 0) CALL INSED2(VR,W2,NPOLG,NVERT,MAXHV,MAXPV,
     $      VCL,REGNUM,HVL,PVL,IANG)
	 IF (IERR .NE. 0) RETURN
      GO TO 30
      END
