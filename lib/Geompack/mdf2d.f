C
C     The following code was excerpted from: dsmdf2.f
C
      SUBROUTINE DSMDF2(HFLAG,NVC,NPOLG,MAXWK,VCL,HVL,PVL,IANG,IVRT,
     $   XIVRT,WIDSQ,EDGVAL,VRTVAL,AREA,WK)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL HFLAG
      INTEGER MAXWK,NPOLG,NVC
      INTEGER HVL(NPOLG),IVRT(*),PVL(4,*),XIVRT(NPOLG+1)
      DOUBLE PRECISION AREA(NPOLG),EDGVAL(*),IANG(*)
      DOUBLE PRECISION VCL(2,NVC),VRTVAL(NVC),WIDSQ(NPOLG),WK(MAXWK)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Set up data structure for heuristic mesh distribution
C        function from data structure for convex polygon decomposition
C        if HFLAG is .TRUE., else set up only IVRT and XIVRT.
C        Also compute areas of convex polygons.
C
C     Input parameters:
C	 HFLAG - .TRUE. if data structure is to be constructed,
C              .FALSE. if only IVRT, XIVRT, AREA are to be computed
C        NVC - number of vertex coordinates in VCL array
C        NPOLG - number of polygonal subregions in HVL array
C        MAXWK - maximum size available for WK array; should be
C              2 times maximum number of vertices in any polygon
C	 VCL(1:2,1:NVC) - vertex coordinate list
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:*),IANG(1:*) - polygon vertex list, interior angles
C
C     Output parameters:
C        IVRT(1:*) - indices of polygon vertices in VCL, ordered by
C              polygon; same size as PVL
C        XIVRT(1:NPOLG+1) - pointer to first vertex of each polygon
C              in IVRT; vertices of polygon K are IVRT(I) for I from
C              XIVRT(K) to XIVRT(K+1)-1
C        WIDSQ(1:NPOLG) - square of width of convex polygons
C        EDGVAL(1:*) - value associated with each edge of decomp.;
C              same size as PVL
C        VRTVAL(1:NVC) - value associated with each vertex of decomp.
C        [Note: Above 5 arrays are for heuristic mdf data structure.]
C        AREA(1:NPOLG) - area of convex polygons
C
C     Working parameters:
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 7 or 201
C
C     Routines called:
C        AREAPG, WIDTH2
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
      INTEGER I,IL,J,JL,K,L,M,NVRT,XC,YC
      DOUBLE PRECISION AREAPG,PIMTOL,S
C
C     Compute area and square of width of polygons.
C
      PIMTOL = PI - TOL
      DO 30 K = 1,NPOLG
	 NVRT = 0
	 I = HVL(K)
   10    CONTINUE
	    IF (IANG(I) .LT. PIMTOL) NVRT = NVRT + 1
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(K)) GO TO 10
	 IF (NVRT + NVRT .GT. MAXWK) THEN
	    IERR = 7
	    RETURN
	 ENDIF
	 XC = 0
   20    CONTINUE
	    IF (IANG(I) .LT. PIMTOL) THEN
	       J = PVL(LOC,I)
	       XC = XC + 1
	       WK(XC) = VCL(1,J)
	       WK(XC+NVRT) = VCL(2,J)
	    ENDIF
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(K)) GO TO 20
	 XC = 1
	 YC = XC + NVRT
	 AREA(K) = AREAPG(NVRT,WK(XC),WK(YC))*0.5D0
	 IF (HFLAG) THEN
	    CALL WIDTH2(NVRT,WK(XC),WK(YC),I,J,WIDSQ(K))
	    IF (IERR .NE. 0) RETURN
	 ENDIF
   30 CONTINUE
C
C     Set up IVRT, XIVRT, EDGVAL, VRTVAL arrays.
C
      L = 1
      DO 50 K = 1,NPOLG
	 XIVRT(K) = L
	 I = HVL(K)
	 IL = PVL(LOC,I)
   40    CONTINUE
	    IVRT(L) = IL
	    J = PVL(SUCC,I)
	    JL = PVL(LOC,J)
	    IF (HFLAG) THEN
	       S = MIN((VCL(1,JL)-VCL(1,IL))**2 + (VCL(2,JL)-VCL(2,IL))
     $            **2, WIDSQ(K))
	       M = PVL(EDGV,I)
	       IF (M .GT. 0) S = MIN(S,WIDSQ(PVL(POLG,M)))
	       EDGVAL(L) = S
	    ENDIF
	    L = L + 1
	    I = J
	    IL = JL
	 IF (I .NE. HVL(K)) GO TO 40
   50 CONTINUE
      XIVRT(NPOLG+1) = L
      IF (.NOT. HFLAG) RETURN
      DO 60 I = 1,NVC
	 VRTVAL(I) = 0.0D0
   60 CONTINUE
      DO 80 K = 1,NPOLG
	 J = XIVRT(K+1) - 1
	 L = J
	 DO 70 I = XIVRT(K),L
	    IL = IVRT(I)
	    IF (VRTVAL(IL) .EQ. 0.0D0) THEN
	       VRTVAL(IL) = MIN(EDGVAL(I),EDGVAL(J))
	    ELSE
	       VRTVAL(IL) = MIN(VRTVAL(IL),EDGVAL(I),EDGVAL(J))
	    ENDIF
	    J = I
   70    CONTINUE
   80 CONTINUE
      END
C
C     The following code was excerpted from: eqdis2.f
C
      SUBROUTINE EQDIS2(HFLAG,UMDF,KAPPA,ANGSPC,ANGTOL,DMIN,NMIN,NTRID,
     $   NVC,NPOLG,NVERT,MAXVC,MAXHV,MAXPV,MAXIW,MAXWK,VCL,REGNUM,HVL,
     $   PVL,IANG,AREA,PSI,H,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL HFLAG
      INTEGER MAXHV,MAXIW,MAXPV,MAXVC,MAXWK,NMIN,NPOLG,NTRID,NVC,NVERT
      INTEGER HVL(MAXHV),IWK(MAXIW),PVL(4,MAXPV),REGNUM(MAXHV)
      DOUBLE PRECISION ANGSPC,ANGTOL,DMIN,KAPPA,UMDF
      DOUBLE PRECISION AREA(MAXHV),H(MAXHV),IANG(MAXPV),PSI(MAXHV)
      DOUBLE PRECISION VCL(2,MAXVC),WK(MAXWK)
      EXTERNAL UMDF
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Further subdivide convex polygons so that an approx.
C        equidistributing triangular mesh can be constructed with
C        respect to heuristic or user-supplied mesh distribution
C        function, and determine triangle size for each polygon of
C        decomposition.
C
C     Input parameters:
C        HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf
C        UMDF(X,Y) - d.p user-supplied mdf with d.p arguments
C        KAPPA - mesh smoothness parameter in interval [0.0,1.0]
C        ANGSPC - angle spacing parameter in radians used to determine
C              extra points as possible endpoints of separators
C        ANGTOL - angle tolerance parameter in radians used in
C              accepting separators
C        DMIN - parameter used to determine if variation of mdf in
C              polygon is 'sufficiently high'
C        NMIN - parameter used to determine if 'sufficiently large'
C              number of triangles in polygon
C        NTRID - desired number of triangles in mesh
C        NVC - number of vertex coordinates or positions used in VCL
C              array
C        NPOLG - number of polygonal subregions or positions used in
C              HVL array
C        NVERT - number of polygon vertices or positions used in PVL
C              array
C        MAXVC - maximum size available for VCL array, should be >=
C              number of vertex coordinates required for decomposition
C              (approx NVC + 2*NS where NS is expected number of new
C              separators)
C        MAXHV - maximum size available for HVL, REGNUM, AREA, PSI, H
C              arrays; should be >= number of polygons required for
C              decomposition (approx NPOLG + NS)
C        MAXPV - maximum size available for PVL, IANG arrays; should be
C              >= number of polygon vertices required for decomposition
C              (approx NVERT + 5*NS)
C        MAXIW - maximum size available for IWK array; should be >=
C              MAX(2*NP, NVERT + NPOLG + 3*NVRT + INT(2*PI/ANGSPC))
C              where NVRT is maximum number of vertices in a convex
C              polygon of the (input) decomposition, NP is expected
C              value of NPOLG on output
C        MAXWK - maximum size available for WK array; should be >=
C              NVC + NVERT + 2*NPOLG + 3*(NVRT + INT(2*PI/ANGSPC))
C        VCL(1:2,1:NVC) - vertex coordinate list
C        REGNUM(1:NPOLG) - region numbers
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles; see routine DSPGDC for more details
C        [Note: The data structures should be as output from routine
C              CVDEC2.]
C
C     Updated parameters:
C        NVC,NPOLG,NVERT,VCL,REGNUM,HVL,PVL,IANG
C
C     Output parameters:
C        AREA(1:NPOLG) - area of convex polygons in decomposition
C        PSI(1:NPOLG) - smoothed mean mdf values in the convex polygons
C        H(1:NPOLG) - triangle size for convex polygons
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precsion work array
C
C     Abnormal return:
C        IERR is set to 3, 4, 5, 6, 7, 200, 201, or 222
C
C     Routines called:
C        DSMDF2, MFDEC2, TRISIZ
C
      INTEGER IERR
      COMMON /GERROR/ IERR
      SAVE /GERROR/
C
      INTEGER EDGVAL,IVRT,M,N,VRTVAL,WIDSQ,XIVRT
C
      IVRT = 1
      XIVRT = IVRT + NVERT
      M = XIVRT + NPOLG
      IF (M .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ENDIF
      WIDSQ = 1
      IF (HFLAG) THEN
         EDGVAL = WIDSQ + NPOLG
         VRTVAL = EDGVAL + NVERT
	 N = NPOLG + NVERT + NVC
         IF (N .GT. MAXWK) THEN
	    IERR = 7
	    RETURN
         ENDIF
      ELSE
	 EDGVAL = 1
	 VRTVAL = 1
         N = 0
      ENDIF
      CALL DSMDF2(HFLAG,NVC,NPOLG,MAXWK-N,VCL,HVL,PVL,IANG,IWK(IVRT),
     $   IWK(XIVRT),WK(WIDSQ),WK(EDGVAL),WK(VRTVAL),AREA,WK(N+1))
      IF (IERR .NE. 0) RETURN
      CALL MFDEC2(HFLAG,UMDF,KAPPA,ANGSPC,ANGTOL,DMIN,NMIN,NTRID,NVC,
     $   NPOLG,NVERT,MAXVC,MAXHV,MAXPV,MAXIW-M,MAXWK-N,VCL,REGNUM,HVL,
     $   PVL,IANG,IWK(IVRT),IWK(XIVRT),WK(WIDSQ),WK(EDGVAL),WK(VRTVAL),
     $   AREA,PSI,IWK(M+1),WK(N+1))
      IF (IERR .NE. 0) RETURN
      IF (2*NPOLG .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ENDIF
      CALL TRISIZ(NTRID,NPOLG,HVL,PVL,AREA,PSI,H,IWK,IWK(NPOLG+1))
      END
C
C     The following code was excerpted from: intpg.f
C
      SUBROUTINE INTPG(NVRT,XC,YC,CTRX,CTRY,ARPOLY,HFLAG,UMDF,WSQ,NEV,
     $   IFV,LISTEV,IVRT,EDGVAL,VRTVAL,VCL,MDFINT,MEAN,STDV,MDFTR)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL HFLAG
      INTEGER IFV,NEV,NVRT
      INTEGER IVRT(*),LISTEV(NEV)
      DOUBLE PRECISION ARPOLY,CTRX,CTRY,MDFINT,MEAN,STDV,UMDF,WSQ
      DOUBLE PRECISION EDGVAL(*),MDFTR(0:NVRT-1),VCL(2,*),VRTVAL(*)
      DOUBLE PRECISION XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Compute integral of MDF2(X,Y) [heuristic mdf] or
C        UMDF(X,Y) [user-supplied mdf] in convex polygon.
C
C     Input parameters:
C        NVRT - number of vertices in polygon
C        XC(0:NVRT),YC(0:NVRT) - coordinates of polygon vertices in
C              CCW order, translated so that centroid is at origin;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        CTRX, CTRY - coordinates of centroid before translation
C        ARPOLY - area of polygon
C        HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf
C        UMDF(X,Y) - d.p user-supplied mdf with d.p arguments
C        WSQ - square of width of original polygon of decomposition
C        NEV,IFV,LISTEV(1:NEV) - output from routine PRMDF2
C        IVRT(1:*),EDGVAL(1:*),VRTVAL(1:*) - arrays output from DSMDF2;
C              if .NOT. HFLAG then only first array exists
C        VCL(1:2,1:*) - vertex coordinate list
C
C     Output parameters:
C        MDFINT - integral of mdf in polygon
C        MEAN - mean mdf value in polygon
C        STDV - standard deviation of mdf in polygon
C        MDFTR(0:NVRT-1) - mean mdf value in each triangle of polygon;
C              triangles are determined by polygon vertices and centroid
C
C     Routines called:
C        UMDF
C
      INTEGER NQPT
      PARAMETER (NQPT = 3)
C
      EXTERNAL UMDF
      INTEGER I,J,K,KP1,L,M
      DOUBLE PRECISION AREATR,D,MDFSQI,S,SUM1,SUM2,TEMP,VAL
      DOUBLE PRECISION X,X0,X1,XX,Y,Y0,Y1,YY
C
      DOUBLE PRECISION WT(NQPT),QC(3,NQPT)
      DATA WT/0.3333333333333333D0,0.3333333333333333D0,
     $   0.3333333333333333D0/
      DATA QC/0.6666666666666666D0,0.1666666666666667D0,
     $   0.1666666666666667D0,0.1666666666666667D0,
     $   0.6666666666666666D0,0.1666666666666667D0,
     $   0.1666666666666667D0,0.1666666666666667D0,
     $   0.6666666666666666D0/
      SAVE WT,QC
C
C     NQPT is number of quad pts for numerical integration in triangle
C     WT(I) is weight of Ith quadrature point
C     QC(1:3,I) are barycentric coordinates of Ith quadrature point
C
      MDFINT = 0.0D0
      MDFSQI = 0.0D0
      DO 30 L = 0,NVRT-1
	 AREATR = 0.5D0*(XC(L)*YC(L+1) - XC(L+1)*YC(L))
	 SUM1 = 0.0D0
	 SUM2 = 0.0D0
	 DO 20 M = 1,NQPT
	    XX = QC(1,M)*XC(L) + QC(2,M)*XC(L+1)
	    YY = QC(1,M)*YC(L) + QC(2,M)*YC(L+1)
	    IF (HFLAG) THEN
C	       VAL = MDF2(XX+CTRX,YY+CTRY,WSQ,NEV,IFV,LISTEV,IVRT,
C    $            EDGVAL,VRTVAL,VCL)
C              Insert code for function MDF2 to reduce number of calls.
C
	       X = XX + CTRX
	       Y = YY + CTRY
               S = WSQ
               DO 10 I = 1,NEV
	          K = LISTEV(I)
	          IF (K .LT. 0) THEN
	             K = -K
	             D = (VCL(1,K) - X)**2 + (VCL(2,K) - Y)**2
	             D = MAX(0.25D0*D,VRTVAL(K))
	             S = MIN(S,D)
	          ELSE
	             KP1 = K + 1
	             IF (I .EQ. NEV .AND. IFV .GT. 0) KP1 = IFV
	             J = IVRT(KP1)
	             X0 = X - VCL(1,J)
	             Y0 = Y - VCL(2,J)
	             X1 = VCL(1,IVRT(K)) - VCL(1,J)
	             Y1 = VCL(2,IVRT(K)) - VCL(2,J)
	             IF (X0*X1 + Y0*Y1 .LE. 0.0D0) THEN
	                D = X0**2 + Y0**2
	             ELSE
	                X0 = X0 - X1
	                Y0 = Y0 - Y1
	                IF (X0*X1 + Y0*Y1 .GE. 0.0D0) THEN
	                   D = X0**2 + Y0**2
	                ELSE
		           D = (X1*Y0 - Y1*X0)**2/(X1**2 + Y1**2)
	                ENDIF
	             ENDIF
	             D = MAX(0.25D0*D,EDGVAL(K))
	             S = MIN(S,D)
	          ENDIF
   10          CONTINUE
               VAL = 1.0D0/S
	    ELSE
 	       VAL = UMDF(XX+CTRX,YY+CTRY)
	    ENDIF
	    TEMP = WT(M)*VAL
	    SUM1 = SUM1 + TEMP
	    SUM2 = SUM2 + TEMP*VAL
   20    CONTINUE
	 MDFTR(L) = SUM1
	 MDFINT = MDFINT + SUM1*AREATR
	 MDFSQI = MDFSQI + SUM2*AREATR
   30 CONTINUE
      MEAN = MDFINT/ARPOLY
      STDV = MDFSQI/ARPOLY - MEAN**2
      STDV = SQRT(MAX(STDV,0.0D0))
      END
C
C     The following code was excerpted from: mfdec2.f
C
      SUBROUTINE MFDEC2(HFLAG,UMDF,KAPPA,ANGSPC,ANGTOL,DMIN,NMIN,NTRID,
     $   NVC,NPOLG,NVERT,MAXVC,MAXHV,MAXPV,MAXIW,MAXWK,VCL,REGNUM,HVL,
     $   PVL,IANG,IVRT,XIVRT,WIDSQ,EDGVAL,VRTVAL,AREA,PSI,IWK,WK)
      IMPLICIT LOGICAL (A-Z)
      LOGICAL HFLAG
      INTEGER MAXHV,MAXIW,MAXPV,MAXVC,MAXWK,NMIN,NPOLG,NTRID,NVC,NVERT
      INTEGER HVL(MAXHV),IVRT(NVERT),IWK(MAXIW),PVL(4,MAXPV)
      INTEGER REGNUM(MAXHV),XIVRT(NPOLG+1)
      DOUBLE PRECISION ANGSPC,ANGTOL,DMIN,KAPPA,UMDF
      DOUBLE PRECISION AREA(MAXHV),EDGVAL(NVERT),IANG(MAXPV),PSI(MAXHV)
      DOUBLE PRECISION VCL(2,MAXVC),VRTVAL(NVC),WIDSQ(NPOLG),WK(MAXWK)
      EXTERNAL UMDF
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Further subdivide convex polygons so that the variation
C        of heuristic or user-supplied mesh distribution function in
C        each polygon is limited.
C
C     Input parameters:
C        HFLAG - .TRUE. if heuristic mdf, .FALSE. if user-supplied mdf
C        UMDF(X,Y) - d.p user-supplied mdf with d.p arguments
C        KAPPA - mesh smoothness parameter in interval [0.0,1.0]
C        ANGSPC - angle spacing parameter in radians used to determine
C              extra points as possible endpoints of separators
C        ANGTOL - angle tolerance parameter in radians used in
C              accepting separators
C        DMIN - parameter used to determine if variation of mdf in
C              polygon is 'sufficiently high'
C        NMIN - parameter used to determine if 'sufficiently large'
C              number of triangles in polygon
C        NTRID - desired number of triangles in mesh
C        NVC - number of vertex coordinates or positions used in VCL
C              array
C        NPOLG - number of polygonal subregions or positions used in
C              HVL array
C        NVERT - number of polygon vertices or positions used in PVL
C              array
C        MAXVC - maximum size available for VCL array
C        MAXHV - maximum size available for HVL,REGNUM,AREA,PSI arrays
C        MAXPV - maximum size available for PVL, IANG arrays
C        MAXIW - maximum size available for IWK array; should be about
C              3*NVRT + INT(2*PI/ANGSPC) where NVRT is maximum number of
C              vertices in a convex polygon of the (input) decomposition
C        MAXWK - maximum size available for WK array; should be about
C              NPOLG + 3*(NVRT + INT(2*PI/ANGSPC)) + 2
C        VCL(1:2,1:NVC) - vertex coordinate list
C        REGNUM(1:NPOLG) - region numbers
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:NVERT),IANG(1:NVERT) - polygon vertex list and
C              interior angles
C        IVRT(1:NVERT),XIVRT(1:NPOLG+1),WIDSQ(1:NPOLG),EDGVAL(1:NVERT),
C              VRTVAL(1:NVC) - arrays output from routine DSMDF2;
C              if .NOT. HFLAG then only first two arrays exist
C        AREA(1:NPOLG) - area of convex polygons in decomposition
C
C     Updated parameters:
C        NVC,NPOLG,NVERT,VCL,REGNUM,HVL,PVL,IANG,AREA
C
C     Output parameters:
C        PSI(1:NPOLG) - mean mdf values in the convex polygons
C
C     Working parameters:
C        IWK(1:MAXIW) - integer work array
C        WK(1:MAXWK) - double precision work array
C
C     Abnormal return:
C        IERR is set to 3, 4, 5, 6, 7, 200, or 222
C
C     Routines called:
C        AREAPG, INSED2, INSVR2, INTPG, PRMDF2, SEPMDF, SEPSHP
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
      INTEGER I,I1,I2,IFV,II,INC,INDPVL,J,K,L,LISTEV,M,MAXN,MDFTR,NEV
      INTEGER NP,NVRT,P,V,W,XC,YC
      DOUBLE PRECISION ALPHA,ANGSP2,AREAPG,AREARG,C1,C2,COSALP,CTRX
      DOUBLE PRECISION CTRY,DELTA,DX,DY,INTREG,MDFINT,MEAN,NUMER,NWAREA
      DOUBLE PRECISION PI2,R,SINALP,STDV,SUMX,SUMY,THETA1,THETA2,WSQ
      DOUBLE PRECISION X1,X2,Y1,Y2
C
C     WK(1:NPOLG) is used for mdf standard deviation in polygons.
C     Compute AREARG = area of region and INTREG = estimated integral
C     of MDF2(X,Y) or UMDF(X,Y).
C
      NVRT = 0
      DO 10 I = 1,NPOLG
         NVRT = MAX(NVRT,XIVRT(I+1)-XIVRT(I))
   10 CONTINUE
      IF (HFLAG .AND. 2*NVRT .GT. MAXIW) THEN
	 IERR = 6
	 RETURN
      ELSE IF (NPOLG + 3*NVRT + 2 .GT. MAXWK) THEN
	 IERR = 7
	 RETURN
      ENDIF
      LISTEV = 1
      XC = NPOLG + 1
      YC = XC + NVRT + 1
      MDFTR = YC + NVRT + 1
      AREARG = 0.0D0
      INTREG = 0.0D0
      NEV = -1
      DO 40 I = 1,NPOLG
	 IF (HFLAG) THEN
	    WSQ = WIDSQ(I)
	    CALL PRMDF2(I,WSQ,IVRT,XIVRT,EDGVAL,VRTVAL,NEV,IFV,
     $         IWK(LISTEV))
	 ENDIF
	 IF (NEV .EQ. 0) THEN
	    PSI(I) = 1.0D0/WSQ
	    WK(I) = 0.0D0
	    MDFINT = PSI(I)*AREA(I)
	 ELSE
	    NVRT = XIVRT(I+1) - XIVRT(I)
	    K = XIVRT(I)
	    SUMX = 0.0D0
	    SUMY = 0.0D0
	    DO 20 J = 0,NVRT-1
	       L = IVRT(K)
	       WK(XC+J) = VCL(1,L)
	       WK(YC+J) = VCL(2,L)
	       SUMX = SUMX + WK(XC+J)
	       SUMY = SUMY + WK(YC+J)
	       K = K + 1
   20       CONTINUE
	    CTRX = SUMX/DBLE(NVRT)
	    CTRY = SUMY/DBLE(NVRT)
	    DO 30 J = 0,NVRT-1
	       WK(XC+J) = WK(XC+J) - CTRX
	       WK(YC+J) = WK(YC+J) - CTRY
   30       CONTINUE
	    WK(XC+NVRT) = WK(XC)
	    WK(YC+NVRT) = WK(YC)
	    CALL INTPG(NVRT,WK(XC),WK(YC),CTRX,CTRY,AREA(I),HFLAG,UMDF,
     $         WSQ,NEV,IFV,IWK(LISTEV),IVRT,EDGVAL,VRTVAL,VCL,MDFINT,
     $         PSI(I),WK(I),WK(MDFTR))
	 ENDIF
	 AREARG = AREARG + AREA(I)
	 INTREG = INTREG + MDFINT
   40 CONTINUE
C
C     If HFLAG, compute mean mdf values from KAPPA, etc. Scale PSI(I)'s
C     so that integral in region is 1. Determine which polygons need to
C     be further subdivided (indicated by negative PSI(I) value).
C
      IF (HFLAG) THEN
	 C1 = (1.0D0 - KAPPA)/INTREG
	 C2 = KAPPA/AREARG
      ELSE
	 C1 = 1.0D0/INTREG
	 C2 = 0.0D0
      ENDIF
      DO 50 I = 1,NPOLG
	 PSI(I) = PSI(I)*C1 + C2
	 IF (C1*WK(I) .GT. PSI(I)*DMIN) THEN
	    IF (NTRID*PSI(I)*AREA(I) .GT. NMIN) PSI(I) = -PSI(I)
	 ENDIF
   50 CONTINUE
C
C     Further subdivide polygons for which STDV/MEAN > DMIN and
C     (estimated number of triangles) > NMIN.
C
      ANGSP2 = 2.0D0*ANGSPC
      PI2 = 2.0D0*PI
      INC = INT(PI2/ANGSPC)
      NEV = 0
      NP = NPOLG
      XC = 1
      DO 140 I = 1,NP
	 IF (PSI(I) .LT. 0.0D0) THEN
	    IF (HFLAG) THEN
	       WSQ = WIDSQ(I)
	       CALL PRMDF2(I,WSQ,IVRT,XIVRT,EDGVAL,VRTVAL,NEV,IFV,
     $            IWK(LISTEV))
	    ENDIF
	    L = NPOLG + 1
	    K = I
   60       CONTINUE
	    IF (K .GT. NPOLG) GO TO 130
   70          CONTINUE
	       IF (PSI(K) .GE. 0.0D0) GO TO 120
		  NVRT = 0
		  SUMX = 0.0D0
		  SUMY = 0.0D0
		  J = HVL(K)
   80             CONTINUE
		     NVRT = NVRT + 1
		     M = PVL(LOC,J)
		     SUMX = SUMX + VCL(1,M)
		     SUMY = SUMY + VCL(2,M)
		     J = PVL(SUCC,J)
		  IF (J .NE. HVL(K)) GO TO 80
		  CTRX = SUMX/DBLE(NVRT)
		  CTRY = SUMY/DBLE(NVRT)
		  MAXN = NVRT + INC
		  IF (NEV + MAXN + 1 .GT. MAXIW) THEN
		     IERR = 6
		     RETURN
		  ELSE IF (3*MAXN + 2 .GT. MAXWK) THEN
		     IERR = 7
		     RETURN
		  ENDIF
		  YC = XC + MAXN + 1
		  MDFTR = YC + MAXN + 1
		  INDPVL = LISTEV + NEV
		  NVRT = 0
		  M = PVL(LOC,J)
		  X1 = VCL(1,M) - CTRX
		  Y1 = VCL(2,M) - CTRY
		  WK(XC) = X1
		  WK(YC) = Y1
		  THETA1 = ATAN2(Y1,X1)
		  P = J
		  IWK(INDPVL) = J
   90             CONTINUE
		     J = PVL(SUCC,J)
		     M = PVL(LOC,J)
		     X2 = VCL(1,M) - CTRX
		     Y2 = VCL(2,M) - CTRY
		     THETA2 = ATAN2(Y2,X2)
		     IF (THETA2 .LT. THETA1) THETA2 = THETA2 + PI2
    		     DELTA = THETA2 - THETA1
		     IF (DELTA .GE. ANGSP2) THEN
			M = INT(DELTA/ANGSPC)
			DELTA = DELTA/DBLE(M)
			DX = X2 - X1
			DY = Y2 - Y1
			NUMER = X1*DY - Y1*DX
			ALPHA = THETA1
			DO 100 II = 1,M-1
			   ALPHA = ALPHA + DELTA
			   COSALP = COS(ALPHA)
			   SINALP = SIN(ALPHA)
			   R = NUMER/(DY*COSALP - DX*SINALP)
			   NVRT = NVRT + 1
			   WK(XC+NVRT) = R*COSALP
			   WK(YC+NVRT) = R*SINALP
			   IWK(INDPVL+NVRT) = -P
  100                   CONTINUE
		     ENDIF
		     NVRT = NVRT + 1
		     WK(XC+NVRT) = X2
		     WK(YC+NVRT) = Y2
		     X1 = X2
		     Y1 = Y2
		     THETA1 = THETA2
		     P = J
		     IWK(INDPVL+NVRT) = J
		  IF (J .NE. HVL(K)) GO TO 90
	          CALL INTPG(NVRT,WK(XC),WK(YC),CTRX,CTRY,AREA(K),HFLAG,
     $               UMDF,WSQ,NEV,IFV,IWK(LISTEV),IVRT,EDGVAL,VRTVAL,
     $               VCL,MDFINT,MEAN,STDV,WK(MDFTR))
		  PSI(K) = MEAN*C1 + C2
	          IF (C1*STDV .GT. PSI(K)*DMIN) THEN
	             IF (NTRID*PSI(K)*AREA(K) .GT. NMIN) THEN
			CALL SEPMDF(ANGTOL,NVRT,WK(XC),WK(YC),AREA(K),
     $                     MEAN,WK(MDFTR),IWK(INDPVL),IANG,I1,I2)
			IF (I1 .LT. 0) THEN
			   IF (YC + 3*NVRT .GT. MAXWK) THEN
			      IERR = 7
			      RETURN
			   ENDIF
			   CALL SEPSHP(ANGTOL,NVRT,WK(XC),WK(YC),
     $                        IWK(INDPVL),IANG,I1,I2,WK(YC+NVRT+1))
			   IF (IERR .NE. 0) RETURN
			ENDIF
			IF (I1 .LT. 0) THEN
			   IERR = 222
			   RETURN
			ENDIF
			V = IWK(INDPVL+I1)
			IF (V .LT. 0) THEN
			   CALL INSVR2(WK(XC+I1)+CTRX,WK(YC+I1)+CTRY,-V,
     $                        NVC,NVERT,MAXVC,MAXPV,VCL,PVL,IANG,V)
			   IF (IERR .NE. 0) RETURN
			ENDIF
			W = IWK(INDPVL+I2)
			IF (W .LT. 0) THEN
			   CALL INSVR2(WK(XC+I2)+CTRX,WK(YC+I2)+CTRY,-W,
     $                        NVC,NVERT,MAXVC,MAXPV,VCL,PVL,IANG,W)
			   IF (IERR .NE. 0) RETURN
			ENDIF
			CALL INSED2(V,W,NPOLG,NVERT,MAXHV,MAXPV,VCL,
     $                     REGNUM,HVL,PVL,IANG)
			IF (IERR .NE. 0) RETURN
		        NVRT = 0
		        J = HVL(K)
  110                   CONTINUE
		           M = PVL(LOC,J)
		           WK(XC+NVRT) = VCL(1,M)
		           WK(YC+NVRT) = VCL(2,M)
		           NVRT = NVRT + 1
		           J = PVL(SUCC,J)
		        IF (J .NE. HVL(K)) GO TO 110
			NWAREA = AREAPG(NVRT,WK(XC),WK(YC))*0.5D0
			AREA(NPOLG) = AREA(K) - NWAREA
			AREA(K) = NWAREA
			PSI(K) = -PSI(K)
			PSI(NPOLG) = PSI(K)
		     ENDIF
	          ENDIF
	       GO TO 70
  120          CONTINUE
	       IF (K .EQ. I) THEN
		  K = L
	       ELSE
		  K = K + 1
	       ENDIF
	    GO TO 60
  130       CONTINUE
	 ENDIF
  140 CONTINUE
      END
C
C     The following code was excerpted from: mmasep.f
C
      SUBROUTINE MMASEP(ANGTOL,XC,YC,INDPVL,IANG,V,W,I1,I2)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,INDPVL(0:*),V(2),W(2)
      DOUBLE PRECISION ANGTOL,IANG(*),XC(0:*),YC(0:*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Find best of four possible separators according to
C        max-min angle criterion.
C
C     Input parameters:
C        ANGTOL - angle tolerance parameter (in radians) for accepting
C              separator
C        XC(0:NVRT),YC(0:NVRT) - coordinates of polygon vertices in
C              CCW order where NVRT is number of vertices;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
C              (XC(I),YC(I)) is extra vertex inserted on edge from
C              K to PVL(SUCC,K)
C        IANG(1:*) - interior angle array
C        V(1:2),W(1:2) - indices in XC, YC in range 0 to NVRT-1; four
C              possible separators are V(I),W(J), I,J = 1,2
C
C     Output parameters:
C        I1,I2 - indices in range 0 to NVRT-1 of best separator
C              according to max-min angle criterion; I1 = -1
C              if no satisfactory separator is found
C
C     Routines called:
C        ANGLE
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER I,J,K,L,M
      DOUBLE PRECISION ALPHA,ANGLE,ANGMAX,ANGMIN,BETA,DELTA,GAMMA
C
      ANGMAX = 0.0D0
      DO 20 I = 1,2
	 L = V(I)
	 K = INDPVL(L)
	 IF (K .GT. 0) THEN
	    ALPHA = IANG(K)
	 ELSE
	    ALPHA = PI
	 ENDIF
	 DO 10 J = 1,2
	    M = W(J)
	    IF (L .EQ. M) GO TO 10
	    K = INDPVL(M)
	    IF (K .GT. 0) THEN
	       BETA = IANG(K)
	    ELSE
	       BETA = PI
	    ENDIF
	    GAMMA = ANGLE(XC(M),YC(M),XC(L),YC(L),XC(L+1),YC(L+1))
	    DELTA = ANGLE(XC(L),YC(L),XC(M),YC(M),XC(M+1),YC(M+1))
	    ANGMIN = MIN(GAMMA,ALPHA-GAMMA,DELTA,BETA-DELTA)
	    IF (ANGMIN .GT. ANGMAX) THEN
	       ANGMAX = ANGMIN
	       I1 = L
	       I2 = M
	    ENDIF
   10    CONTINUE
   20 CONTINUE
      IF (ANGMAX .LT. ANGTOL) I1 = -1
      END
C
C     The following code was excerpted from: prmdf2.f
C
      SUBROUTINE PRMDF2(IPOLY,WSQ,IVRT,XIVRT,EDGVAL,VRTVAL,NEV,IFV,
     $   LISTEV)
      IMPLICIT LOGICAL (A-Z)
      INTEGER IFV,IPOLY,IVRT(*),LISTEV(*),NEV,XIVRT(*)
      DOUBLE PRECISION EDGVAL(*),VRTVAL(*),WSQ
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Preprocessing step for evaluating mesh distribution
C        function in polygon IPOLY - the edges and vertices for
C        which distances must be computed are determined.
C
C     Input parameters:
C        IPOLY - index of polygon
C        WSQ - square of width of polygon IPOLY
C        IVRT(1:*) - indices of polygon vertices in VCL, ordered by
C              polygon
C        XIVRT(1:*) - pointer to first vertex of each polygon in IVRT;
C              vertices of polygon IPOLY are IVRT(I) for I from
C              XIVRT(IPOLY) to XIVRT(IPOLY+1)-1
C        EDGVAL(1:*) - value associated with each edge of decomp.
C        VRTVAL(1:*) - value associated with each vertex of decomp.
C
C     Output parameters:
C        NEV - number of edges and vertices for which distances must
C              be evaluated
C        IFV - index of first vertex XIVRT(IPOLY) if LISTEV(NEV)
C              = XIVRT(IPOLY+1) - 1; 0 otherwise
C        LISTEV(1:*) - array of length <= [XIVRT(IPOLY+1)-XIVRT(IPOLY)]
C              *2 containing indices of edges and vertices mentioned
C              above; indices of vertices are negated
C
      INTEGER I,IM1,J,L
C
      IFV = 0
      NEV = 0
      IM1 = XIVRT(IPOLY+1) - 1
      L = IM1
      DO 10 I = XIVRT(IPOLY),L
	 J = IVRT(I)
	 IF (VRTVAL(J) .LT. MIN(EDGVAL(I),EDGVAL(IM1))) THEN
	    NEV = NEV + 1
	    LISTEV(NEV) = -J
	 ENDIF
	 IF (EDGVAL(I) .LT. WSQ) THEN
	    NEV = NEV + 1
	    LISTEV(NEV) = I
	 ENDIF
	 IM1 = I
   10 CONTINUE
      IF (NEV .GT. 0) THEN
	 IF (LISTEV(NEV) .EQ. L) IFV = XIVRT(IPOLY)
      ENDIF
      END
C
C     The following code was excerpted from: sepmdf.f
C
      SUBROUTINE SEPMDF(ANGTOL,NVRT,XC,YC,ARPOLY,MEAN,MDFTR,INDPVL,
     $   IANG,I1,I2)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,NVRT
      INTEGER INDPVL(0:NVRT)
      DOUBLE PRECISION ANGTOL,ARPOLY,MEAN
      DOUBLE PRECISION IANG(*),MDFTR(0:NVRT-1),XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine separator to split convex polygon into two
C        parts based on mesh distribution function.
C
C     Input parameters:
C        ANGTOL - angle tolerance parameter (in radians)
C        NVRT - number of vertices in polygon
C        XC(0:NVRT),YC(0:NVRT) - coordinates of polygon vertices in
C              CCW order, translated so that centroid is at origin;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        ARPOLY - area of polygon
C        MEAN - mean mdf value in polygon
C        MDFTR(0:NVRT-1) - mean mdf value in each triangle of polygon;
C              triangles are determined by polygon vertices and centroid
C        INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
C              (XC(I),YC(I)) is extra vertex inserted on edge from
C              K to PVL(SUCC,K)
C        IANG(1:*) - interior angle array
C
C     Output parameters:
C        I1,I2 - indices in range 0 to NVRT-1 of best separator
C              according to mdf and max-min angle criterion; I1 = -1
C              if no satisfactory separator is found
C
C     Routines called:
C        ANGLE, MMASEP
C
      DOUBLE PRECISION PI,TOL
      COMMON /GCONST/ PI,TOL
      SAVE /GCONST/
C
      INTEGER HI,I,L,M,V(2),W(2)
      DOUBLE PRECISION ANGLE,AREATR,SUM
C
C     Determine triangle with highest mean mesh density; then determine
C     triangles adjacent to this triangle with mesh density >= MEAN
C     such that the area of these triangles is <= ARPOLY/2.
C     Note that twice the triangle area is computed.
C
      HI = 0
      DO 10 I = 1,NVRT-1
	 IF (MDFTR(I) .GT. MDFTR(HI)) HI = I
   10 CONTINUE
      SUM = XC(HI)*YC(HI+1) - XC(HI+1)*YC(HI)
      L = HI - 1
      IF (L .LT. 0) L = NVRT - 1
      M = HI + 1
      IF (M .GE. NVRT) M = 0
   20 CONTINUE
	 IF (MDFTR(L) .GE. MDFTR(M)) THEN
	    I = L
	 ELSE
	    I = M
	 ENDIF
	 IF (MDFTR(I) .LT. MEAN) GO TO 30
	 AREATR = XC(I)*YC(I+1) - XC(I+1)*YC(I)
	 SUM = SUM + AREATR
	 IF (SUM .GT. ARPOLY) GO TO 30
	 IF (I .EQ. L) THEN
	    L = L - 1
	    IF (L .LT. 0) L = NVRT - 1
	 ELSE
	    M = M + 1
	    IF (M .GE. NVRT) M = 0
	 ENDIF
      GO TO 20
   30 CONTINUE
      L = L + 1
      IF (L .GE. NVRT) L = 0
C
C     Interchange role of L and M depending on angle determined by
C     (XC(M),YC(M)), (0,0), and (XC(L),YC(L)).
C     Possible separators are L,M; L,M+1; L+1,M; L+1,M+1.
C
      IF (ANGLE(XC(M),YC(M),0.0D0,0.0D0,XC(L),YC(L)) .GT. PI) THEN
	 I = L
	 L = M
	 M = I
      ENDIF
      V(1) = L
      V(2) = L - 1
      IF (V(2) .LT. 0) V(2) = NVRT - 1
      W(1) = M
      W(2) = M + 1
      IF (W(2) .GE. NVRT) W(2) = 0
      CALL MMASEP(ANGTOL,XC,YC,INDPVL,IANG,V,W,I1,I2)
      END
C
C     The following code was excerpted from: sepshp.f
C
      SUBROUTINE SEPSHP(ANGTOL,NVRT,XC,YC,INDPVL,IANG,I1,I2,WK)
      IMPLICIT LOGICAL (A-Z)
      INTEGER I1,I2,NVRT
      INTEGER INDPVL(0:NVRT)
      DOUBLE PRECISION ANGTOL,IANG(*),WK(2*NVRT),XC(0:NVRT),YC(0:NVRT)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Determine separator to split convex polygon into two
C        parts based on shape (diameter) of polygon.
C
C     Input parameters:
C        ANGTOL - angle tolerance parameter (in radians)
C        NVRT - number of vertices in polygon
C        XC(0:NVRT), YC(0:NVRT) - coordinates of polygon vertices in
C              CCW order, translated so that centroid is at origin;
C              (XC(0),YC(0)) = (XC(NVRT),YC(NVRT))
C        INDPVL(0:NVRT) - indices in PVL of vertices; INDPVL(I) = -K if
C              (XC(I),YC(I)) is extra vertex inserted on edge from
C              K to PVL(SUCC,K)
C        IANG(1:*) - interior angle array
C
C     Output parameters:
C        I1,I2 - indices in range 0 to NVRT-1 of best separator
C              according to shape and max-min angle criterion; I1 = -1
C              if no satisfactory separator is found
C
C     Working parameters:
C        WK(1:2*NVRT) - space for two working arrays of length NVRT
C
C     Abnormal return:
C        IERR is set to 200
C
C     Routines called:
C        DIAM2, MMASEP
C
      INTEGER IERR
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      SAVE /GERROR/,/GCONST/
C
      INTEGER I,K,N,V(2),W(2)
      DOUBLE PRECISION DIST,DX,DY,PIMTOL,XA,YA
C
C     Determine diameter of polygon. Possible separators endpoints (two
C     on each side of polygon) are nearest to perpendicular bisector of
C     diameter. (XA,YA) and (XA+DX,YA+DY) are on bisector. Distance to
C     bisector is proportional to two times triangle area.
C
      PIMTOL = PI - TOL
      N = 0
      DO 10 I = 0,NVRT-1
	 K = INDPVL(I)
	 IF (K .GT. 0) THEN
	    IF (IANG(K) .LT. PIMTOL) THEN
	       N = N + 1
	       WK(N) = XC(I)
	       WK(N+NVRT) = YC(I)
	    ENDIF
	 ENDIF
   10 CONTINUE
      CALL DIAM2(N,WK,WK(NVRT+1),I1,I2,DIST)
      IF (IERR .NE. 0) RETURN
      IF (I1 .GT. I2) THEN
	 I = I1
	 I1 = I2
	 I2 = I
      ENDIF
      DX = WK(I2+NVRT) - WK(I1+NVRT)
      DY = WK(I1) - WK(I2)
      XA = 0.5D0*(WK(I1) + WK(I2) - DX)
      YA = 0.5D0*(WK(I1+NVRT) + WK(I2+NVRT) - DY)
C
      I = I1 - 1
   20 CONTINUE
      IF (XC(I) .EQ. WK(I1) .AND. YC(I) .EQ. WK(I1+NVRT)) THEN
	 I1 = I
      ELSE
	 I = I + 1
	 GO TO 20
      ENDIF
      I = MAX(I2-1,I1+1)
   30 CONTINUE
      IF (XC(I) .EQ. WK(I2) .AND. YC(I) .EQ. WK(I2+NVRT)) THEN
	 I2 = I
      ELSE
	 I = I + 1
	 GO TO 30
      ENDIF
      I = I1 + 1
   40 CONTINUE
      DIST = DX*(YC(I) - YA) - DY*(XC(I) - XA)
      IF (DIST .GE. 0.0D0) THEN
	 V(1) = I - 1
	 V(2) = I
      ELSE
	 I = I + 1
	 GO TO 40
      ENDIF
      I = I2 + 1
   50 CONTINUE
      IF (I .GE. NVRT) I = 0
      DIST = DX*(YC(I) - YA) - DY*(XC(I) - XA)
      IF (DIST .LE. 0.0D0) THEN
	 W(1) = I - 1
	 W(2) = I
	 IF (I .LE. 0) W(1) = NVRT - 1
      ELSE
	 I = I + 1
	 GO TO 50
      ENDIF
      CALL MMASEP(ANGTOL,XC,YC,INDPVL,IANG,V,W,I1,I2)
      END
C
C     The following code was excerpted from: sfdwmf.f
C
      SUBROUTINE SFDWMF(L,R,PSI,INDP,LOCH)
      IMPLICIT LOGICAL (A-Z)
      INTEGER L,R
      INTEGER INDP(R),LOCH(*)
      DOUBLE PRECISION PSI(*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Sift PSI(INDP(L)) down heap which has maximum PSI value
C        at root of heap and is maintained by pointers in INDP.
C
C     Input parameters:
C	 L - element of heap to be sifted down
C        R - upper bound of heap
C	 PSI(1:*) - key values for heap
C        INDP(1:R) - indices of PSI which are maintained in heap
C        LOCH(1:*) - location of indices in heap (inverse of INDP)
C
C     Updated parameters:
C        INDP,LOCH
C
      INTEGER I,J,K
      DOUBLE PRECISION T
C
      I = L
      J = 2*I
      K = INDP(I)
      T = PSI(K)
   10 CONTINUE
      IF (J .GT. R) GO TO 20
	 IF (J .LT. R) THEN
	    IF (PSI(INDP(J)) .LT. PSI(INDP(J+1))) J = J + 1
	 ENDIF
	 IF (T .GE. PSI(INDP(J))) GO TO 20
	 INDP(I) = INDP(J)
	 LOCH(INDP(I)) = I
	 I = J
	 J = 2*I
      GO TO 10
   20 CONTINUE
      INDP(I) = K
      LOCH(K) = I
      END
C
C     The following code was excerpted from: sfupmf.f
C
      SUBROUTINE SFUPMF(R,PSI,INDP,LOCH)
      IMPLICIT LOGICAL (A-Z)
      INTEGER R
      INTEGER INDP(R),LOCH(*)
      DOUBLE PRECISION PSI(*)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Sift PSI(INDP(R)) up heap which has maximum PSI value
C        at root of heap and is maintained by pointers in INDP.
C
C     Input parameters:
C	 R - element of heap to be sifted up
C	 PSI(1:*) - key values for heap
C        INDP(1:R) - indices of PSI which are maintained in heap
C        LOCH(1:*) - location of indices in heap (inverse of INDP)
C
C     Updated parameters:
C        INDP,LOCH
C
      INTEGER I,J,K
      DOUBLE PRECISION T
C
      I = R
      J = INT(I/2)
      K = INDP(I)
      T = PSI(K)
   10 CONTINUE
      IF (I .LE. 1) GO TO 20
	 IF (T .LE. PSI(INDP(J))) GO TO 20
	 INDP(I) = INDP(J)
	 LOCH(INDP(I)) = I
	 I = J
	 J = INT(I/2)
      GO TO 10
   20 CONTINUE
      INDP(I) = K
      LOCH(K) = I
      END
C
C     The following code was excerpted from: trisiz.f
C
      SUBROUTINE TRISIZ(NTRID,NPOLG,HVL,PVL,AREA,PSI,H,INDP,LOCH)
      IMPLICIT LOGICAL (A-Z)
      INTEGER NPOLG,NTRID
      INTEGER HVL(NPOLG),INDP(NPOLG),LOCH(NPOLG),PVL(4,*)
      DOUBLE PRECISION AREA(NPOLG),H(NPOLG),PSI(NPOLG)
C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Purpose: Smooth PSI (mean mesh distribution function) values using
C        heap so that they differ by a factor of at most 4 in adjacent
C        polygons and then compute triangle sizes for each polygon.
C
C     Input parameters:
C	 NTRID - desired number of triangles in mesh
C        NPOLG - number of polygons or positions used in HVL array
C        HVL(1:NPOLG) - head vertex list
C        PVL(1:4,1:*) - polygon vertex list
C        AREA(1:NPOLG) - area of convex polygons in decomposition
C        PSI(1:NPOLG) - mean mdf values in the convex polygons
C
C     Updated parameters:
C	 PSI(1:NPOLG) - values are 'smoothed' on output
C
C     Output parameters:
C        H(1:NPOLG) - triangle size for convex polygons
C
C     Working parameters:
C        INDP(1:NPOLG) - indices of polygon or PSI which are maintained
C              in heap according to PSI values
C        LOCH(1:NPOLG) - location of polygon indices in heap
C
C     Routines called:
C        SFDWMF, SFUPMF
C
      INTEGER EDGV,LOC,POLG,SUCC
      PARAMETER (LOC = 1, POLG = 2, SUCC = 3, EDGV = 4)
C
      INTEGER I,J,K,L,R
      DOUBLE PRECISION FACTOR,SUM
C
      FACTOR = 0.25D0
      DO 10 I = 1,NPOLG
	 INDP(I) = I
	 LOCH(I) = I
   10 CONTINUE
      K = INT(NPOLG/2)
      DO 20 L = K,1,-1
	 CALL SFDWMF(L,NPOLG,PSI,INDP,LOCH)
   20 CONTINUE
      DO 40 R = NPOLG,2,-1
	 J = INDP(1)
	 INDP(1) = INDP(R)
	 LOCH(INDP(1)) = 1
	 CALL SFDWMF(1,R-1,PSI,INDP,LOCH)
	 I = HVL(J)
   30    CONTINUE
	    K = PVL(EDGV,I)
	    IF (K .GT. 0) THEN
	       K = PVL(POLG,K)
	       IF (PSI(K) .LT. PSI(J)*FACTOR) THEN
		  PSI(K) = PSI(J)*FACTOR
		  CALL SFUPMF(LOCH(K),PSI,INDP,LOCH)
	       ENDIF
	    ENDIF
	    I = PVL(SUCC,I)
	 IF (I .NE. HVL(J)) GO TO 30
   40 CONTINUE
C
      SUM = 0.0D0
      DO 50 I = 1,NPOLG
	 SUM = SUM + PSI(I)*AREA(I)
   50 CONTINUE
      FACTOR = 2.0D0/DBLE(NTRID)
      DO 60 I = 1,NPOLG
	 PSI(I) = PSI(I)/SUM
	 H(I) = SQRT(FACTOR/PSI(I))
   60 CONTINUE
      END
C
C     The following code was excerpted from: umdf2.f
C
      DOUBLE PRECISION FUNCTION UMDF2(X,Y)
      IMPLICIT LOGICAL (A-Z)
      DOUBLE PRECISION X,Y
C
C     Purpose: Dummy user-supplied mesh distribution function which
C        is provided if heuristic mesh distribution function is used.
C
C     Input parameters:
C        X,Y - coordinates of 2-D point
C
C     Returned function value:
C        UMDF2 - mesh distribution function value at (X,Y)
C
      UMDF2 = 1.0D0
      END
