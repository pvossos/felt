C
C     Written and copyright by:
C        Barry Joe, Dept. of Computing Science, Univ. of Alberta
C        Edmonton, Alberta, Canada  T6G 2H1
C        Phone: (403) 492-5757      Email: barry@cs.ualberta.ca
C
C     Modifications by Jason Gobat (jgobat@ucsd.edu) to turn what
C     was a main program into a subroutine to interface with the
C     FElt system (this is the only Fortran routine that FElt calls).
C
      SUBROUTINE GEOMPK(TOLIN,ANGSPC,ANGTOL,KAPPA,DMIN,NMIN,NTRID,NVC,
     $                  NCUR,NVBC,VCL,TIL,NTRI,ISTAT)
      IMPLICIT LOGICAL (A-Z)
C
C     Driver routine for interfacing the Geompack library
C
      INTEGER ISTAT 
      INTEGER IERR,IPRT,MSGLVL
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      COMMON /GPRINT/ IPRT,MSGLVL
C
      INTEGER MAXHV,MAXIW,MAXNC,MAXPV,MAXTI,MAXVC,MAXWK
      PARAMETER (MAXHV = 350)
      PARAMETER (MAXIW = 900)
      PARAMETER (MAXNC = 30)
      PARAMETER (MAXPV = 2000)
      PARAMETER (MAXTI = 8000)
      PARAMETER (MAXVC = 5000)
      PARAMETER (MAXWK = 1500)
C
      INTEGER HVL(MAXHV),IWK(MAXIW)
      INTEGER NVBC(MAXNC),PVL(4,MAXPV),REGNUM(MAXHV),TIL(3,MAXTI)
      INTEGER TSTART(MAXHV),VNUM(MAXPV),VSTART(MAXPV)
      INTEGER IMEAS,IRDR,NCUR,NHOLA,NHOLE,NH
      INTEGER NMIN,NPOLG,NTRI,NTRID,NVC,NVERT
C
      DOUBLE PRECISION AREA(MAXHV),H(MAXHV),IANG(MAXPV),PSI(MAXHV)
      DOUBLE PRECISION VCL(2,MAXVC),WK(MAXWK)
      DOUBLE PRECISION ANGSPC,ANGTOL
      DOUBLE PRECISION DMIN,KAPPA,TOLIN,UMDF2
      LOGICAL HFLAG
      EXTERNAL UMDF2
C
C     Read in vertices of general polygonal region.
C     CASE = 1 : simple polygon or multiply connected polygonal region
C
      IRDR = 5
      IMEAS = 7
      CALL INITCB(TOLIN)
      ANGSPC = ANGSPC*PI/180.0D0
      ANGTOL = ANGTOL*PI/180.0D0
      HFLAG = (KAPPA .GE. 0.0D0 .AND. KAPPA .LE. 1.0D0)
      IF (NVC .GT. MAXVC) THEN
	 ISTAT = 24
	 RETURN
      ELSE IF (NCUR .GT. MAXNC) THEN
	 ISTAT = 24
	 RETURN
      ENDIF
C
C     Call routine DSMCPR or DSPGDC to set data structures in arrays
C     REGNUM, HVL, PVL, IANG, HOLV = IWK.
C
      NHOLE = NCUR - 1
      CALL DSMCPR(NHOLE,NVBC,VCL,MAXHV,MAXPV,MAXIW,NVC,NPOLG,NVERT,
     $            NHOLA,REGNUM,HVL,PVL,IANG,IWK)
      IF (IERR .NE. 0) THEN
         ISTAT = IERR
	 RETURN
      ENDIF

      NH = NHOLE*2 + NHOLA
C
C     Obtain simple and convex polygon decompositions, and print
C     measurements.
C
      CALL SPDEC2(ANGSPC,ANGTOL,NVC,NPOLG,NVERT,NHOLE,NHOLA,MAXVC,MAXHV,
     $   MAXPV,MAXIW-NH,MAXWK,IWK,VCL,REGNUM,HVL,PVL,IANG,IWK(NH+1),WK)
      CALL CVDEC2(ANGSPC,ANGTOL,NVC,NPOLG,NVERT,MAXVC,MAXHV,MAXPV,MAXIW,
     $   MAXWK,VCL,REGNUM,HVL,PVL,IANG,IWK,WK)
      IF (IERR .NE. 0) THEN
         ISTAT = IERR
	 RETURN
      ENDIF
C
C     Obtain further convex polygon decomposition based on mesh
C     distribution function, and triangle sizes for the polygons.
C     Then print measurements.
C
      CALL EQDIS2(HFLAG,UMDF2,KAPPA,ANGSPC,ANGTOL,DMIN,NMIN,NTRID,NVC,
     $   NPOLG,NVERT,MAXVC,MAXHV,MAXPV,MAXIW,MAXWK,VCL,REGNUM,HVL,PVL,
     $   IANG,AREA,PSI,H,IWK,WK)
      IF (IERR .NE. 0) THEN
         ISTAT = IERR
	 RETURN
      ENDIF
C
C     Triangulate each convex polygon in decomposition according to
C     mesh spacings in H array.
C
      CALL TRIPR2(NVC,NPOLG,NVERT,MAXVC,MAXTI,MAXIW,MAXWK,H,VCL,HVL,PVL,
     $   IANG,NTRI,TIL,VSTART,VNUM,TSTART,IWK,WK)
      IF (IERR .NE. 0) THEN
         ISTAT = IERR
	 RETURN
      ENDIF
C
C     Check for errors, if none we succeeded
C
      IF (IERR .NE. 0) THEN
         ISTAT = IERR
	 RETURN
      ENDIF

      ISTAT = 0
  610 FORMAT (1X,'*** ',A,' must be increased to',I8)

      END

      SUBROUTINE GEOMPK_(TOLIN,ANGSPC,ANGTOL,KAPPA,DMIN,NMIN,NTRID,NVC,
     $                  NCUR,NVBC,VCL,TIL,NTRI,ISTAT)
      IMPLICIT LOGICAL (A-Z)
C
C     Driver routine for interfacing the Geompack library
C
      INTEGER ISTAT 
      INTEGER IERR,IPRT,MSGLVL
      DOUBLE PRECISION PI,TOL
      COMMON /GERROR/ IERR
      COMMON /GCONST/ PI,TOL
      COMMON /GPRINT/ IPRT,MSGLVL
C
      INTEGER MAXHV,MAXIW,MAXNC,MAXPV,MAXTI,MAXVC,MAXWK
      PARAMETER (MAXHV = 350)
      PARAMETER (MAXIW = 900)
      PARAMETER (MAXNC = 30)
      PARAMETER (MAXPV = 2000)
      PARAMETER (MAXTI = 8000)
      PARAMETER (MAXVC = 5000)
      PARAMETER (MAXWK = 1500)
C
      INTEGER NVBC(MAXNC),TIL(3,MAXTI)
      INTEGER NCUR
      INTEGER NMIN,NTRI,NTRID,NVC
C
      DOUBLE PRECISION VCL(2,MAXVC)
      DOUBLE PRECISION ANGSPC,ANGTOL
      DOUBLE PRECISION DMIN,KAPPA,TOLIN
C
      CALL GEOMPK(TOLIN,ANGSPC,ANGTOL,KAPPA,DMIN,NMIN,NTRID,NVC,
     $                  NCUR,NVBC,VCL,TIL,NTRI,ISTAT)
      END
