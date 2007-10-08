/*****************************************************************************
 *
 * File:	oogl.c
 * 
 * Description:	contains code to read and write basic ASCII data files that
 *		are formatted for input into graphing packages like GEOMVIEW
 *
 * Author:	Stefan Majer (stefan@cae001.cae.fh-furtwangen.de) was
 *		kind enough to put this together and make it available
 *              to us.
 *
 ****************************************************************************/

# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <math.h>
# include "problem.h"
# include "objects.h"
# include "mesh.h"
# include "patchwork.h"
# include "definition.h"
# include "error.h"
# include "oogl.h"

# define X 1
# define Y 2
# define Z 3

extern int InitializeProblem (void);

/******************************************************* 
 * function place_symbol
 * translate into x,y,z  and rotate into "direct" a symbol 
 *
 ******************************************************/
 
static void
place_symbol(FILE *output, int direct, double scale, 
             double x, double y, double z, char *symbol)
{
  double	s;
  
  
  fprintf (output, "\n{");
  fprintf (output, "\nINST\ntransform\n");

  if ( direct == 1){
  fprintf (output, "0 %f 0 0\n",scale);
  fprintf (output, "0 0 %f 0\n",scale);
  fprintf (output, "%f 0 0 0\n",scale);
  fprintf (output, "%f %f %f 1",x,y,z);
  }
  else if ( direct == 2){
  fprintf (output, "0 0 %f 0\n",scale);
  fprintf (output, "%f 0 0 0\n",scale);
  fprintf (output, "0 %f 0 0\n",scale);
  fprintf (output, "%f %f %f 1",x,y,z);
  }
  else if ( direct == 3){
  fprintf (output, "%f 0 0 0\n",scale);
  fprintf (output, "0 %f 0 0\n",scale);
  fprintf (output, "0 0 %f 0\n",scale);
  fprintf (output, "%f %f %f 1",x,y,z);
  }
  fputs (symbol,output);
  fprintf (output, "}");

}

static void
place_number(FILE *output, int direct, double scale,
             double x, double y, double z, int number)
{
   int		anzahl=1,
   		digit=0,
   		i,
   		exponent=1;
   		
   if ( (number/10 < 1) && (number >= 1) ) anzahl = 1 ;
   if ( (number/100 < 1) && (number/10 >= 1) ) anzahl = 2;
   if ( (number/1000 < 1) && (number/100 >= 1) ) anzahl = 3;
   if ( (number/10000 < 1) && (number/1000 >= 1) ) anzahl = 3;
   z = z + 20*scale;
   y = y + 20*scale;
  
   for (i=anzahl;i>0;i--){
   digit = number;
   
   if ( i == 1 ) exponent = 1;
   if ( i == 2 ) exponent = 10;
   if ( i == 3 ) exponent = 100;
   if ( i == 4 ) exponent = 1000;
   
   digit = (int) ( digit / exponent) ;
     
   switch (digit)
       {
       case 0:place_symbol(output,X,scale,x,y,z,zero);
              break;
       case 1:place_symbol(output,X,scale,x,y,z,one);
              break;
       case 2:place_symbol(output,X,scale,x,y,z,two);
              break;
       case 3:place_symbol(output,X,scale,x,y,z,three);
              break;
       case 4:place_symbol(output,X,scale,x,y,z,four);
              break;
       case 5:place_symbol(output,X,scale,x,y,z,five);
              break;
       case 6:place_symbol(output,X,scale,x,y,z,six);
              break;
       case 7:place_symbol(output,X,scale,x,y,z,seven);
              break;
       case 8:place_symbol(output,X,scale,x,y,z,eight);
              break;
       case 9:place_symbol(output,X,scale,x,y,z,nine);      
              break;
       }
    y = y + 40*scale;
    number = number - (digit*exponent);  
   }
  
}


/******************************************************* 
 * function dcomp
 * is needed for qsort to check what double is greater
 *
 ******************************************************/

static int
dcomp(const void *aa, const void *bb)
 {
      double a = *((const double *) aa);
      double b = *((const double *) bb);

   if (a > b) 	return(1);
   else if (a < b) 	return(-1);
   else 	 	return(0);
   
 }


/******************************************************* 
 * function WriteOoglFile
 * printout the OOGL Ascii code
 *
 ******************************************************/

int WriteOoglFile (char *filename)
{
   FILE		*output;
   unsigned	i,j;
   Element	e;
   double	pnx,
   		pny,
   		pnz,
   		px[10000],
   		py[10000],
   		pz[10000],
   		dx,
   		dy,
   		dz,
   		force[10],
   		forcex[10000],
   		forcey[10000],
   		forcez[10000],
   		fmax,
   		scalefx,
   		scalefy,
   		scalefz,
   		scale;
   		
   char		*element_name,*element_color;

   if (strcmp (filename, "-") == 0)
      output = stdout;
   else {   
      output = fopen (filename, "w");
      if (output == NULL) {
         error ("graph: could not open %s for writing.", filename);
         return 1;
      }
   }
   element_name = problem.elements [1] -> definition -> name;
   
   fprintf (output,"LIST\n");
   
if (strncmp (element_name, "beam", 4) == 0) {
   fprintf (output,"{VECT\n");
   fprintf (output,"%d %d 1\n",problem.num_elements,2*problem.num_elements);
   fprintf (output,"\n");
   for (i = 1 ; i <= problem.num_elements ; i++) fprintf (output,"2 ");
   fprintf (output,"\n");
   fprintf (output,"1 ");
   for (i = 1 ; i <= problem.num_elements-1 ; i++) fprintf (output,"0 ");
   fprintf (output,"\n\n");

   for (i = 1 ; i <= problem.num_elements ; i++) {
   
      e = problem.elements [i];
      
      element_color = e -> material -> color;
      
      for (j = 1 ; j <= e -> definition -> shapenodes ; j++) {
         if (e -> node[j] == NULL)
            break;
         fprintf (output,"%.1f %.1f %.1f   ", e -> node [j] -> x,
      	     	  e -> node [j] -> y, e -> node [j] -> z);
      }
      fprintf (output,"\n");
   }
   fputs (color1,output);
}
else if (strncmp (element_name, "CST", 3) == 0) {      
   for (i = 1 ; i <= problem.num_elements ; i++) {
   fprintf (output,"{ QUAD  ");
    
      e = problem.elements [i];
      fprintf (output,"%.1f %.1f %.1f  ", e -> node [1] -> x,
      	     	  e -> node [1] -> y, e -> node [1] -> z); 
      for (j = 1 ; j <= e -> definition -> shapenodes ; j++) {
         if (e -> node[j] == NULL)
            break;
      fprintf (output,"%.1f %.1f %.1f  ", e -> node [j] -> x,
      	     	  e -> node [j] -> y, e -> node [j] -> z); 
      }
     
      fprintf (output,"}\n");
   }     
}
else if (strncmp (element_name, "quad", 4) == 0) {      
   for (i = 1 ; i <= problem.num_elements ; i++) {
   fprintf (output,"{ QUAD  ");
    
      e = problem.elements [i];
      for (j = 1 ; j <= e -> definition -> shapenodes ; j++) {
         if (e -> node[j] == NULL)
            break;
      fprintf (output,"%.1f %.1f %.1f  ", e -> node [j] -> x,
      	     	  e -> node [j] -> y, e -> node [j] -> z); 
      }
     
      fprintf (output,"}\n");
   }     
}
else if (strncmp (element_name, "truss", 5) == 0) {      
   for (i = 1 ; i <= problem.num_elements ; i++) {
   fprintf (output,"{ VECT \n1 2 1\n2\n1\n\n");
    
      e = problem.elements [i];
      element_color = e -> material -> color;
      
      for (j = 1 ; j <= e -> definition -> shapenodes ; j++) {
         if (e -> node[j] == NULL)
            break;
      fprintf (output,"%.1f %.1f %.1f\n", e -> node [j] -> x,
      	     	  e -> node [j] -> y, e -> node [j] -> z); 
      }
     
      fprintf (output,"\n0.36 0.9 0.9 1\n}\n");
      /*fprintf (output,"\n");
      fprintf (output,element_color);
      fprintf (output,"\n}\n");*/
   }     
}

/***************************************************************
 * Find the minimum and the maximum of the coordinates and
 * calculate the needet scale for thhe symbols
 *
 ***************************************************************/
   for (i = 1  ; i <= problem.num_nodes ; i++) {
   
     px[i-1] = problem.nodes [i] -> x;
     py[i-1] = problem.nodes [i] -> y;
     pz[i-1] = problem.nodes [i] -> z;     
     forcex[i-1] = fabs(problem.nodes [i] -> force -> force [1].value);	  
     forcey[i-1] = fabs(problem.nodes [i] -> force -> force [2].value);	  
     forcez[i-1] = fabs(problem.nodes [i] -> force -> force [3].value);	  
   }     
       
   qsort(px,problem.num_nodes,sizeof(double),dcomp);  
   qsort(py,problem.num_nodes,sizeof(double),dcomp);  
   qsort(pz,problem.num_nodes,sizeof(double),dcomp); 
   qsort(forcex,problem.num_nodes,sizeof(double),dcomp);  
   qsort(forcey,problem.num_nodes,sizeof(double),dcomp);  
   qsort(forcez,problem.num_nodes,sizeof(double),dcomp); 
   force[0]=forcex[problem.num_nodes-1]; 
   force[1]=forcey[problem.num_nodes-1]; 
   force[2]=forcez[problem.num_nodes-1]; 
   qsort(force,3,sizeof(double),dcomp);  
   fmax = force[2];
     
   dx = px[0]-px[problem.num_nodes-1];
   dy = py[0]-py[problem.num_nodes-1];
   dz = pz[0]-pz[problem.num_nodes-1];
   
   scale = (sqrt ( dx*dx + dy*dy + dz*dz ))/10;
   
/***************************************************************
 * check for boundary conditions and place the desired symbol
 *
 *
 ***************************************************************/
    
   
   for (i = 1 ; i <= problem.num_nodes ; i++) {
     pnx = problem.nodes [i] -> x;
     pny = problem.nodes [i] -> y;
     pnz = problem.nodes [i] -> z;
     
       place_symbol(output,X,scale,pnx,pny,pnz,cross);
       place_number(output,X,scale/1000,pnx,pny,pnz,i);
      
     if (problem.nodes [i] -> constraint -> constraint [1]) 
	{
          place_symbol(output,X,scale,pnx,pny,pnz,constraint); 
	}
     if (problem.nodes [i] -> constraint -> constraint [2]) 
	{
          place_symbol(output,Y,scale,pnx,pny,pnz,constraint); 
	}
     if (problem.nodes [i] -> constraint -> constraint [3]) 
	{
          place_symbol(output,Z,scale,pnx,pny,pnz,constraint); 
	}
     if (problem.nodes [i] -> constraint -> constraint [4]) 
	{
          place_symbol(output,X,scale,pnx,pny,pnz,constraintrot); 
	}
     if (problem.nodes [i] -> constraint -> constraint [5]) 
	{
          place_symbol(output,Y,scale,pnx,pny,pnz,constraintrot); 
	}
     if (problem.nodes [i] -> constraint -> constraint [6]) 
	{
          place_symbol(output,Z,scale,pnx,pny,pnz,constraintrot); 
	}

     if (problem.nodes [i] -> force != NULL) {
        if (problem.nodes [i] -> force -> force [1].value)
   	   {
	     scalefx = problem.nodes [i] -> force -> force [1].value;
             place_symbol(output,X,5*scale*scalefx/fmax,pnx,pny,pnz,fz);
   	   }
        if (problem.nodes [i] -> force -> force [2].value)
   	   {
	     scalefy = problem.nodes [i] -> force -> force [2].value;
             place_symbol(output,Y,5*scale*scalefy/fmax,pnx,pny,pnz,fz); 
	   }
        if (problem.nodes [i] -> force -> force [3].value)
    	   {
	     scalefz = problem.nodes [i] -> force -> force [3].value;	  
             place_symbol(output,Z,5*scale*scalefz/fmax,pnx,pny,pnz,fz); 
	   }
     }

     if (problem.nodes [i] -> m)
	{
          place_symbol(output,X,scale/3,pnx,pny,pnz,sphere);
	}
   }
   
   place_symbol(output,Z,scale,0.0,0.0,0.0,origin); 
   
   if (output != stdout)
      fclose (output);

   return 0;
}
