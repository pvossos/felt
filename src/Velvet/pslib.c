/* 
 * this code taken basically verbatim (changes and deletions only where
 * necessary) from the xmgr package by Paul J. Turner.  It looks like he
 * got if from somewhere else too, so credit to those folks as well.
 */

/* 
 *
 * driver for postscript printer
 *
 * courtesy of:
 *
 * Jim Hudgens
 * hudgens@ray.met.fsu.edu
 *
 * Further modifications by,
 * Ole Holm Nielsen
 * ohnielse@ltf.dth.dk
 *
 */

#include <stdio.h>
#include <ctype.h>
#include "version.h"

#define MAX_BUF_LEN 128

static void putps();
static int isoneof();
static void stripspecial ();

/* postscript page at scale = 0.25 */

/*
 * the following defines are tuned for our HP LaserJet IV
 * and may need adjustment for other printers
 */

#define PSXMIN 100
#define PSXMAX 2350
#define PSYMIN 100
#define PSYMAX 3045
#define DXPS 2250
#define DYPS 2945
#define CHARS 1.8

#define MINCOLOR 0
#define PSMAXPAT 30
#define MAXLINEWIDTH 9


static int psxmin = PSXMIN;
static int psxmax = PSXMAX;
static int psymin = PSYMIN;
static int psymax = PSYMAX;
static int psdx = DXPS;
static int psdy = DYPS;
static int pscolor = -1;
static int pslinewidth = -1;
static int psdmode;
static int pspattern = 0;
static int psfont = 0;
static double pscharsize = 1.7;
static int pslinestyle;
static char *fname;

/* reduced from 1000 to 800 PJT */

#define MAXPATHLEN 800		/* MAXPATHLEN points in a path between
				 * strokes */
static int   pathlength = 0;
static FILE *psout;

static int   prevx = 99999, 
             prevy = 99999, 
	     prevmode;

static void stroke()
{
    if (pathlength) {
	 fprintf(psout, "stroke\n");
	prevx = 99999;
	prevy = 99999;
	pathlength = 0;
    }
}

int pssetmode(mode, ps_fp)
    int    mode;
    FILE   *ps_fp;
{
    if (mode % 2) {
       psout = ps_fp;
       if (psout == NULL) 
           return 0;
    }

    switch (mode) {

    case 3:			/* EPS   portrait */
	pscharsize = CHARS;
        psxmin = PSXMIN;
	psxmax = PSXMAX;
	psymin = PSYMIN;
	psymax = PSYMAX;
	psdx = DXPS;
	psdy = DYPS;
	
	break;

    case 1:			/* EPS landscape */
	pscharsize = CHARS;
	psxmin = PSYMIN;
	psxmax = PSYMAX;
	psymin = PSXMIN;
	psymax = PSXMAX;
	psdx = DYPS;
	psdy = DXPS;
	
	break;

    case 2:
    case 4:
        stroke();
        fprintf(psout, "showpage\n");
        fprintf(psout, "%%%%Trailer\n");
        fclose(psout);
 
        break;
    }

    return mode;
}

void drawps(x2, y2, mode)
    int x2, y2, mode;
{
    int xtmp, ytmp;

    if (x2 < 0 || y2 < 0) { 	/* Eliminate garbage on output */
	return;
    } 
    xtmp = x2;
    ytmp = y2;

    if (mode) {
	if (prevmode && xtmp == prevx && ytmp == prevy) {
	    return;		/* previous mode was draw and points are the
				 * same */
	}
	fprintf(psout, "%d %d l\n", xtmp, ytmp);	/* lineto */
    } else {
	/* Avoid excessive moveto */
	if (xtmp == prevx && ytmp == prevy) {
	    return;
	}
	fprintf(psout, "%d %d m\n", xtmp, ytmp);	/* moveto */
    }
    pathlength++;
    prevx = xtmp;
    prevy = ytmp;

    /*
     * Printers have some maximum number of points in a path. See PostScript
     * Language Reference Manual (Red book), p. 261. Hence the fix that
     * follows 
     */

    prevmode = mode;
    if (pathlength > MAXPATHLEN) {
	stroke();
	prevmode = 0;
	fprintf(psout, "%d %d m\n", xtmp, ytmp);	/* moveto */
    }
}

int xconvps(x)
    double x;
{
    return ((int) (psxmin + x));
}

int yconvps(y)
    double y;
{
    return ((int) (psymin + y));
}

int pssetcolor(c)
    int c;
{
    static int		   first_time = 1;
    static unsigned char   red[16], 
		 	   green[16], 
			   blue[16];

    if (first_time) {
       first_time = 0;

    /* white  */ red[0] = 255;  green[0] = 255; blue[0] = 255; 
    /* black  */ red[1] = 0;    green[1] = 0;   blue[1] = 0;
    /* red    */ red[2] = 255;  green[2] = 0;   blue[2] = 0;
    /* green  */ red[3] = 0;    green[3] = 255; blue[3] = 0;
    /* blue   */ red[4] = 0;    green[4] = 0;   blue[4] = 255;
    /* yellow */ red[5] = 255;  green[5] = 255; blue[5] = 0;
    /* brown  */ red[6] = 188;  green[6] = 143; blue[6] = 143;
    /* gray   */ red[7] = 220;  green[7] = 220; blue[7] = 220;
    /* violet */ red[8] = 148;  green[8] = 0;   blue[8] = 211;
    /* cyan   */ red[9] = 0;    green[9] = 255; blue[9] = 255;
    /* magenta*/ red[10] = 255; green[10] = 0;  blue[10] = 211;
    /* orange */ red[11] = 255; green[11] = 138; blue[11] = 0;
    /* b violet*/ red[12] = 114; green[12] = 33; blue[12] = 188;
    /* maroon */ red[13] = 103; green[13] = 7;   blue[13] = 72;
    /* turq   */ red[14] = 72;  green[14] = 209; blue[14] = 204;
    /* f green*/ red[15] = 85;  green[15] = 192; blue[15] = 52;

    }
    
    stroke();
    if (c != pscolor) {
	if (c >= 0) {
	    fprintf(psout, "%f %f %f setrgbcolor\n", red[c] / 255.0, 
                                                     green[c] / 255.0, 
					             blue[c] / 255.0);
	    pscolor = 1;
	}
    }
    pscolor = c;
    return c;
}

int pssetlinewidth(c)
    int c;
{
    stroke();
    if (c != pslinewidth) {
	c = c % (MAXLINEWIDTH + 1);
	if (c == 1)
	    fprintf(psout, "1 setlinewidth\n");
	else
	    fprintf(psout, "%d setlinewidth\n", (int) (3.5 * c + 0.51));
    }
    pslinewidth = c;
    return c;
}

int pssetlinestyle(style)
    int style;
{
    stroke();
    if (style == pslinestyle) {
	return (pslinestyle);
    }
    switch (style) {
    case 1:			/* solid */
	fprintf(psout, "[] 0 setdash\n");
	break;
    case 2:			/* dotted */
	fprintf(psout, "[4 8] 0 setdash\n");
	break;
    case 3:			/* long dash */
	fprintf(psout, "[20 20] 0 setdash\n");
	break;
    case 4:			/* short dash */
	fprintf(psout, "[40 20] 0 setdash\n");
	break;
    case 5:			/* dot-dashed */
	fprintf(psout, "[40 20 12 20] 0 setdash\n");
	break;
    }
    return (pslinestyle = style);
}

char pscurfont[MAX_BUF_LEN] = "/Times-Roman findfont \n60 scalefont\n setfont";
int psfontsize = 60;

void pssetfont(n)
    int n;
{
    if (psfont == n) {
	return;
    }
    switch (n) {
    case 0:
	sprintf(pscurfont, "/Times-Roman findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 1:
	sprintf(pscurfont, "/Times-Bold findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 2:
	sprintf(pscurfont, "/Times-Italic findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 3:
	sprintf(pscurfont, "/Times-BoldItalic findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 4:
	sprintf(pscurfont, "/Helvetica findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 5:
	sprintf(pscurfont, "/Helvetica-Bold findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 6:
	sprintf(pscurfont, "/Helvetica-Oblique findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 7:
	sprintf(pscurfont, "/Helvetica-BoldOblique findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 8:
	sprintf(pscurfont, "/Courier findfont \n%d scalefont\n setfont", psfontsize);
        break; 
    case 9:
	sprintf(pscurfont, "/Courier-Bold findfont \n%d scalefont\n setfont", psfontsize);
        break; 
    case 10:
	sprintf(pscurfont, "/Courier-Oblique findfont \n%d scalefont\n setfont", psfontsize);
        break; 
    case 11:
	sprintf(pscurfont, "/Courier-BoldOblique findfont \n%d scalefont\n setfont", psfontsize);
        break; 
    case 12:
	sprintf(pscurfont, "/Symbol findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 13:
	sprintf(pscurfont, "/Symbol findfont \n%d scalefont\n setfont", psfontsize);
	break;
    case 14:
	sprintf(pscurfont, "/Symbol findfont \n%d scalefont\n setfont", psfontsize);
	break;
    }

    fprintf(psout, "%s\n", pscurfont);
    psfont = n;
}

void pssetfontsize(size)
    double size;
{
    static double	prev_size;
    int sf = psfont;

    if (size == prev_size)
       return;


    psfontsize = (int) (size * 4);	/* to account for the 0.25 scaling */
    psfont = -1;
    pssetfont(sf);
}

static void escape_paren(s)
    char *s;
{
    char t[256];
    int i, cnt = 0;
    for (i = 0; i < strlen(s); i++) {
	if (s[i] == '(' || s[i] == ')') {
	    t[cnt++] = '\\';
	}
	t[cnt++] = s[i];
    }
    t[cnt] = 0;
    strcpy(s, t);
}

void dispstrps(x, y, rot, s, just, fudge)
    int x, y, rot, just, fudge;
    char *s;
{
    char tmpstr[256];

    stroke();
    if (psfontsize == 0 || s == NULL || strlen(s) == 0) {
	return;
    }
    fprintf(psout, "%d %d m\n", x, y);
    fprintf(psout, "gsave\n");
    fprintf(psout, "%d %d translate\n", x, y);
    fprintf(psout, "%d rotate\n", rot);
    if (fudge) {
	fprintf(psout, "%d 0  m\n", psfontsize / 4);
    } else {
	fprintf(psout, "0 0  m\n");
    }
    switch (just) {
    case 0:
	break;
    case 1:
	stripspecial(s, tmpstr);
	escape_paren(tmpstr);
	fprintf(psout, "(%s) RJ\n", tmpstr);
	break;
    case 2:
	stripspecial(s, tmpstr);
	escape_paren(tmpstr);
	fprintf(psout, "(%s) CS\n", tmpstr);
	break;
    }
    putps(s);
    fprintf(psout, "grestore\n");
    fprintf(psout, "newpath\n");
}

static void putps(s)
    char *s;
{
    int i, slen = strlen(s), curcnt = 0;
    int underline = 0, offset = 0;
    double saves = psfontsize / 60.0, scale = psfontsize / 60.0;
    char curstr[256];
    int upperset = 0;
    int symfont = 0;

    if (psfont == 9) {
	symfont = 1;
	upperset = 0x80;
    } else {
	symfont = 0;
	upperset = 0;
    }
    for (i = 0; i < slen; i++) {
	if (s[i] == '-' && isdigit(s[i + 1])) {
	    /* s[i] = 0261; */
	} else if (s[i] == '\\' && isdigit(s[i + 1])) {
	    curstr[curcnt] = 0;
	    if (curcnt >= 1) {
		fprintf(psout, "(%s) show\n", curstr);
	    }
	    curcnt = 0;
	    if (symfont) {
		symfont = 0;
		upperset = 0;
	    }
	    pssetfont(s[i + 1] - '0');
	    if (psfont == 9) {
		symfont = 1;
		upperset = 0x80;
	    }
	    i++;
	    continue;
	} else if (s[i] == '(' || s[i] == ')') {
	    curstr[curcnt++] = '\\';
	} else if (s[i] == '\\' && isoneof(s[i + 1], "cCbxsSNuU+-")) {
	    switch (s[i + 1]) {
	    case 'x':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		if (symfont == 0) {
		    symfont = 1;
		    upperset = 0x80;
		}
		pssetfont(10);
		i++;
		break;
	    case 's':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		pssetfontsize(scale = 0.6 * saves);
		offset -= psfontsize / 2;
		fprintf(psout, "0 %d rmoveto\n", -(psfontsize / 2));
		i++;
		break;
	    case 'S':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		pssetfontsize(scale = 0.6 * saves);
		offset += psfontsize;
		fprintf(psout, "0 %d rmoveto\n", psfontsize);
		i++;
		break;
	    case 'N':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		scale = saves;
		pssetfontsize(scale);
		fprintf(psout, "0 %d rmoveto\n", -offset);
		offset = 0;
/*
		fprintf(psout, "0 %d rmoveto\n", psfontsize);
*/
		i++;
		break;
	    case 'b':
		i++;
		break;
	    case 'c':
		upperset = 0x80;
		i++;
		break;
	    case 'C':
		upperset = 0;
		i++;
		break;
	    case 'u':
		underline = 1;
		i++;
		break;
	    case 'U':
		underline = 0;
		i++;
		break;
	    case '-':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		scale -= 0.2;
		if (scale < 0.2) {
		    scale = 0.2;
		}
		pssetfontsize(scale);
		i++;
		break;
	    case '+':
		curstr[curcnt] = 0;
		if (curcnt >= 1) {
		    fprintf(psout, "(%s) show\n", curstr);
		}
		curcnt = 0;
		scale += 0.2;
		pssetfontsize(scale);
		i++;
		break;
	    }
	    continue;
	} else if (s[i] == '\\' && s[i + 1] == '\\') {
	    curstr[curcnt++] = '\\';
	    curstr[curcnt++] = s[i];
	    i++;
	    continue;
	}
	curstr[curcnt++] = s[i] + upperset;
    }
    curstr[curcnt] = 0;
    fprintf(psout, "(%s) show\n", curstr);
}

int pssetpat(k)
    int k;
{
    stroke();
    if (k > PSMAXPAT) {
	k = PSMAXPAT;
    } else if (k < 0) {
	k = 0;
	fprintf(psout, "0.0 setgray\n");
    }
    return (pspattern = k);
}

void psfill(n, px, py)
    int n;
    int px[], py[];

{
    int i;

    stroke();
    drawps(px[0], py[0], 0);
    for (i = 1; i < n; i++) {
	drawps(px[i], py[i], 1);
    }
    fprintf(psout, "closepath\n");
    fprintf(psout, "%f setgray\n", 1.0 - pspattern / (double) PSMAXPAT);
    fprintf(psout, "gsave eofill grestore\n");
    stroke();
    fprintf(psout, "0 setgray\n");
}

void psfillcolor(n, px, py)
    int n;
    int px[], py[];

{
    int i;

    stroke();
    drawps(px[0], py[0], 0);
    for (i = 1; i < n; i++) {
	drawps(px[i], py[i], 1);
    }
    fprintf(psout, "closepath\n");
    fprintf(psout, "gsave eofill grestore\n");
    stroke();
}

void psdrawarc(x, y, r, start, end) 
    int x, y, r, start, end;
{
    stroke();
    fprintf(psout, "%d %d %d %d %d arc\n", x, y, r, start, end);
    fprintf(psout, "stroke\n");
}

void psfillarc(x, y, r, start, end)
    int x, y, r, start, end;
{
    stroke();
    fprintf(psout, "%d %d %d %d %d arc\n", x, y, r, start, end);
    fprintf(psout, "gsave fill grestore\n");
    fprintf(psout, "stroke\n");
}

void psdrawellipse(x, y, xm, ym, start, end)
    int x, y, xm, ym, start, end;
{
    double scalex = (double) xm / (double) ym, scaley = 1.0;

    stroke();
    fprintf(psout, "gsave\n");
    fprintf(psout, "%f %f scale\n", scalex, scaley);
    fprintf(psout, "%d %d %d %d %d arc\n", (int) (x * 1.0 / scalex), 
                                           y, ym, start, end);
    fprintf(psout, "stroke\n");
    fprintf(psout, "grestore\n");
}

void psfillellipse(x, y, xm, ym, start, end)
    int x, y, xm, ym, start, end;
{
    double scalex = (double) xm / (double) ym, scaley = 1.0;

    stroke();
    fprintf(psout, "gsave\n");
    fprintf(psout, "%f %f scale\n", scalex, scaley);
/*
    fprintf(psout, "%d %d %d %d %d arc\n", x, y, ym, start, end);
*/
    fprintf(psout, "%d %d %d %d %d arc\n", (int) (x * 1.0 / scalex), 
                                            y, ym, start, end);
    fprintf(psout, "gsave fill grestore\n");
    fprintf(psout, "stroke\n");
    fprintf(psout, "grestore\n");
}

int psgetextents (x, y)
    int		*x, *y;
{
    *x = psdx;
    *y = psdy;

    return 0;
}

void psleavegraphics(fp)
     FILE *fp;
{
     pssetmode(psdmode + 1, fp);
}

/*           postscript initialization routine  */


int psinitgraphics(dmode, ps_fp)
    int   dmode;
    FILE  *ps_fp;
{
    psdmode = dmode;
    if (!pssetmode(psdmode, ps_fp)) {
	return -1;
    }

    fprintf(psout, "%%!PostScript\n");

    fprintf(psout, "%%%%Creator: Velvet %s\n", VERSION);
    fprintf(psout, "%%%%Title: %s\n", fname);

    fprintf(psout, "%%%%EndComments\n");
    fprintf(psout, "/m {moveto} bind def\n");
    fprintf(psout, "/l {lineto} bind def\n");
    fprintf(psout, "/RJ {\n");
    fprintf(psout, " stringwidth neg exch neg exch\n");
    fprintf(psout, " rmoveto\n");
    fprintf(psout, "} bind def\n");

    fprintf(psout, "/CS {\n");
    fprintf(psout, " stringwidth\n");
    fprintf(psout, " 2 div neg exch 2 div neg exch\n");
    fprintf(psout, " rmoveto\n");
    fprintf(psout, "} bind def\n");

    fprintf(psout, "0.25 0.25 scale\n");
    fprintf(psout, "1 setlinecap\n");

/*
 * rotate if in landscape mode
 */

    if (dmode == 1) {
	fprintf(psout, "%d 0 translate\n", 2 * psymin + psdy);
	fprintf(psout, "90 rotate\n");
    }

    pssetcolor(1);
    pssetlinewidth(1);
    pssetlinestyle(0);
    psfont = -1;
    pssetfont(2);
    return 0;
}

static int isoneof(c, s)
    int c;
    char *s;
{
    while (*s) {
	if (c == *s) {
	    return 1;
	} else {
	    s++;
	}
    }
    return 0;
}

static void stripspecial(s, cs)
    char *s, *cs;
{
    int i, slen = strlen(s), curcnt = 0;

    for (i = 0; i < slen; i++) {
	if (s[i] == '\\' && isdigit(s[i + 1])) {
	    i++;
	} else if (s[i] == '\\' && isoneof(s[i + 1], "cCbxsSNuU+-")) {
	    i++;
	} else if (s[i] == '\\' && s[i + 1] == '\\') {
	    i++;
	} else {
	    cs[curcnt++] = s[i];
	}
    }
    cs[curcnt] = 0;
}
