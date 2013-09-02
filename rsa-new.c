unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);
void output_bob(unsigned int);

#define N 2

unsigned int a[2*N], b[2*N], s[2*N], o[2*N], m[2*N];


void add(unsigned int * aa, unsigned int * ss, unsigned int q)
{
  unsigned int c = 0;
  unsigned int i = 0;

  for(i = 0; i < 2*N; i++)
    {
      ss[i] = ((aa[i] + c)&q) + ss[i] ;
      c = 0;
      if(ss[i] < aa[i])
        c = 1;
    }
}

void sub(unsigned int * ss, unsigned int * aa,  unsigned int q)
{
  unsigned int c = 0;
  unsigned int i = 0;

  for(i = 0; i < 2*N; i++)
    {
      ss[i] =  ss[i] - ((aa[i] + c)&q);
      c = 0;
      if(ss[i] > aa[i])
        c = 1;
    }
}

void shift_r(unsigned int * aa)
{
  unsigned int c = 0, k = 0, i = 0;
  for(i = N-1; i != 0; i--)
    {
      k = 0;
      if((aa[i] & 0x01) != 0)
        k = 0x80000000;
      aa[i] = (aa[i] >> 1) | c;
      c = k;
    }
      
  aa[0] = (aa[0] >> 1) | c;
}

void shift_l(unsigned int * aa)
{
  unsigned int q=0,k=0,i=0;
  for(i = 0; i < 2*N; i++)
    {
      k = 0;
      if((aa[i] & 0x80000000) != 0)
        k = 0x00000001;

      aa[i] = (aa[i] << 1) | q;

      q = k;
    }
}

void mod_reduce(unsigned int * ss, unsigned int * mm)
{
  /* 
   * Reduce ss modulo mm
   */


  /* Check for overflow */
  unsigned int i, g1 = 0, g2 = 0;

  for(i = 0; i < 2 * N; i++)
    {
      g1 = g2 & 0x01;
      g2 = 0;
      if (mm[i] < (ss[i] + g1))
        g2 = 0xFFFFFFFF;
    }

  /* Subtract mm from ss if there was an overflow */
  /* i.e. above we checked if we could subtract m from s without wrapping around */
  sub(ss, mm, g2);
}

void copy(unsigned int * ss, unsigned int * oo)
{
  unsigned int i = 0;
  for(i = 0; i < 2*N; i++)
    oo[i] = ss[i];
}

void mod_mul(unsigned int * aa, unsigned int * bb, unsigned int * ss, unsigned int * mm)
{
  unsigned int i = 0, q = 0;
  /* Loop across each bit of b[] */
  for(i = 0; i < 32*N; i++) 
    {
      q = 0;
      if(bb[0] & 0x01)
        q = 0xFFFFFFFF;

      add(aa,ss,q);

      mod_reduce(ss, mm);

      /* Shift a[] one bit left */
      shift_l(aa);

      mod_reduce(aa, mm);

      /* Shift each b[] element one position to the right */
      shift_r(bb);
    }
}

#define e 17

void mod_exp(unsigned int * aa, unsigned int * ss, unsigned int * oo, unsigned int * mm)
{
  unsigned int i = 0, j = 0;
  unsigned aa2[2*N];
  for(i = 0; i < 2*N; i++)
    {
      ss[i] = 0;
      aa2[i] = aa[i];
    }
  ss[0] = 1;
  for(i = 0; i < e; i++)
    {
      mod_mul(aa, ss, oo, mm);
      copy(oo, ss);
      copy(aa2, aa);
      for(j = 0; j < 2 * N; j++) 
        oo[j] = 0;
    }
}

#ifdef _TEST
#include <stdio.h>

unsigned int alice(unsigned int pos)
{
  if(pos == 0)
    return 0x3;
  else if(pos == 32)
    return 0x0;
  else
    return 0;
}

unsigned int bob(unsigned int pos)
{
  if(pos == 0)
    return 0x9;
  else if(pos == 32)
    return 0x0;
  else
    return 0;
}

void output_alice(unsigned int x)
{
  printf("Output for alice: %x\n", x);
}

void output_bob(unsigned int x)
{
  printf("Output for bob: %x\n", x);
}
#endif

#ifndef _TEST
void main(void)
#else
int main(int argc, char ** argv)
#endif
{
  unsigned int i;
  m[0] = 113*101;
  for(i = 0; i < N; i++)
    {
      a[i] = alice(i * 32);
      b[i] = bob(i * 32);
      s[i] = 0;
    }

  mod_exp(a, s, o, m);
  for(i = 0; i < 2; i++)
    {
      output_alice(s[i]);
    }
#ifdef _TEST
  return 0;
#endif
}
