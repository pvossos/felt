# include <stdio.h>
# include <math.h>
# include <string.h>
# include "complex.h"

double re(complex x)
{
   return x.r;
}

double im(complex x)
{
   return x.i;
}

complex negate(complex x)
{
   complex	y;

   y.r = -x.r;
   y.i = -x.i;
 
   return y;
}

complex recip(complex x)
{
   complex	one;

   one.r = 1.0;
   one.i = 0.0;

   return cdiv(one, x); 
}
  
double modulus(complex x)
{
   return sqrt(x.r*x.r + x.i*x.i);
}
 
complex mult (complex x, complex y)
{
   complex	a;

   a.r = x.r*y.r - x.i*y.i;
   a.i = x.i*y.r + x.r*y.i;

   return a;
}

complex add (complex x, complex y)
{
   complex	a;

   a.r = x.r + y.r;
   a.i = x.i + y.i;

   return a;
}

complex sub (complex x, complex y)
{
   complex	a;

   a.r = x.r - y.r;
   a.i = x.i - y.i;

   return a;
}

complex cdiv (complex x, complex y)
{
   complex	a;
   double	factor;

   a = mult (x, cnjgt (y)); 
   factor = y.r*y.r + y.i*y.i; 
    
   a.r = a.r / factor;
   a.i = a.i / factor;

   return a;
}

complex felt_csqrt (complex x)
{
   complex	y;
   double	theta, radius;

   radius = sqrt(x.r*x.r + x.i*x.i);
   theta = atan2(x.i, x.r);

   y.r = sqrt(radius)*cos(theta/2);
   y.i = sqrt(radius)*sin(theta/2);

   return y;
}
 
complex felt_cexp (double x)
{
   complex	y;

   y.r = cos(x);
   y.i = sin(x);

   return y;
}

complex felt_ccos (complex x)
{
   complex	y;

   y = scale(felt_cexp(x.r), exp(-x.i)*0.5, 0.0);
   y = add(y, scale(felt_cexp(-x.r), exp(x.i)*0.5, 0.0));

   return y;
}

complex felt_csin (complex x)
{
   complex	y;
   complex	i2;

   i2.r = 0.0;
   i2.i = 2;

   y = scale(felt_cexp(x.r), exp(-x.i), 0.0);
   y = sub(y, scale(felt_cexp(-x.r), exp(x.i), 0.0));
   y = cdiv(y, i2);

   return y;
}

complex cnjgt (complex x)
{
   complex	a;
  
   a.r = x.r;
   a.i = -x.i;

   return a;
}

unsigned is_zero(complex x)
{
   return (x.r == 0 && x.i == 0);
}

complex zero(void)
{
   complex	x;

   x.r = 0.0;
   x.i = 0.0;

   return x;
}

complex scale(complex x, double factor, double offset)
{
   complex	y;

   y.r = x.r * factor + offset;
   y.i = x.i * factor + offset;

   return y;
}

char *cprint (complex x)
{
   char		buffer [32];

   sprintf (buffer, "%7.5f %s %7.5fi  ", x.r, (x.i < 0 ? "-" : "+"), fabs (x.i));

   return strdup(buffer);
}
