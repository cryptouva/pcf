unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);
void output_bob(unsigned int);

#define N 2

unsigned int a[2*N], b[2*N], bb[2*N], s[2*N], d[2*N];

void shift_add(unsigned int * aa, unsigned int * ss, unsigned int c, unsigned int q)
{
  unsigned int k,z,i;
  for(i = 0; i < N; i++)
    {
      /* Addition */
      ss[i] = ((aa[i] + c) & q) + ss[i];
      c = 0;
      if(ss[i] < aa[i])
        c = 0x00000001;

      /* Shift left */
      k = 0;
      if((aa[i] & 0x80000000) != 0)
        k = 0x00000001;
      aa[i] = (aa[i] << 1) | z;
      z = k;
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

#ifdef _TEST
#include <stdio.h>

unsigned int alice(unsigned int pos)
{
  if(pos == 0)
    return 0xFF00;
  else if(pos == 32)
    return 0xF1;
  else
    return 0;
}

unsigned int bob(unsigned int pos)
{
  if(pos == 0)
    return 0xEEFF;
  else if(pos == 32)
    return 0xFF;
  else
    return 0;
}

void output_alice(unsigned int x)
{
  printf("Output for alice: %x\n", x);
}
#endif

#ifndef _TEST
void main(void)
#else
int main(int argc, char ** argv)
#endif
{

  /* Use exponent 3 */
  unsigned int e = 3;
  unsigned int i = 0, j = 0, c = 0, k = 0, q = 0, z = 0, g1 = 0, g2 = 0;

  /* As in KSS12 */
  a[N-1] = 0xFF000000;
  a[0] = 0x01;

  for(i = 0; i < N; i++)
    {
      /* modulus = XOR of alice and bob inputs*/
      d[i] = alice(i * 32) ^ bob(i * 32);

      /*      a[i] = alice((i + N) * 32);*/

      s[i] = 0;

      /* Bob will hold the message */ 
      bb[i] = b[i] = bob((i + N)*32);
    }

  /* mod exp */
  for(e = 0; e < 32*N; e++)
    {
      /* mod mul */
      for(j = 0; j < 32*N; j++)
        {
          /* Shift and add*/
          q = 0;
          if(d[0] & 0x01)
            q = 0xFFFFFFFF;
          if(b[0] & 0x01)
            q = q & 0xFFFFFFFF;

          z = 0;
          g1 = 0;
          g2 = 0;

          shift_add(bb, s, c, q);

          shift_r(b);

          g1 = 0;
          g2 = 0;
          for(i = 0; i < N; i++)
            {
              /* Check for overflowing the modulus */
              k = g1 & 0x00000001;
              g1 = 0;
              if((s[i] + k) > a[i])
                g1 = 0xFFFFFFFF;

              k = g2 & 0x00000001;
              g2 = 0;
              if((bb[i] + k) > a[i])
                g2 = 0xFFFFFFFF;
            }

          c = 0;
          z = 0;
          for(i = 0; i < N; i++)
            {
              k = 0;
              if((a[i] + c) > s[i])
                k = 1;

              s[i] = s[i] - ((a[i] + c) & g1);

              c = k;

              k = 0;
              if((a[i] + z) > bb[i])
                k = 1;
              bb[i] = bb[i] - ((a[i] + z) & g2);
              z = k;

            }
        }

      c = 0;
      for(i = N-1; i != 0; i--)
        {
          k = 0;
          if((d[i] & 0x01) != 0)
            k = 0x80000000;
          d[i] = (d[i] >> 1) | c;
          c = k;
        }
      d[0] = (d[0] >> 1) | c;

    }

  for(i = 0; i < N; i++)
    {
      output_alice(s[i]);
    }
#ifdef _TEST
  return 0;
#endif
}
