unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);
void output_bob(unsigned int);

#define N 2

unsigned int a[2*N], b[2*N], s[2*N];

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

/* TODO: this should work, but it says that there is a non-constant
   pointer.  It is possible that this is the ss pointer, because q
   will be placed in the same position in the arguments array when
   add() is called. */
void mul(unsigned int * aa, unsigned int * bb)
{
  unsigned int i = 0, q = 0;
  /* Loop across each bit of b[] */
  for(i = 0; i < 32*N; i++) 
    {
      q = 0;
      if(bb[0] & 0x01)
        q = 0xFFFFFFFF;

      add(aa,s,q);

      /* Shift a[] one bit left */
      shift_l(aa);

      /* Shift each b[] element one position to the right */
      shift_r(bb);
    }
}

void main(void)
{
  unsigned int i = 0, j = 0, c = 0, k = 0, q = 0;

  for(i = 0; i < N; i++)
    {
      a[i] = alice(i * 32);
      b[i] = bob(i*32);
      a[i+N] = b[i+N] = s[i] = s[i+N] = 0;
    }

  mul(a,b);
  /* for(i = 0; i < 32*N; i++)  */
  /*   { */
  /*     q = 0; */
  /*     if(b[0] & 0x01) */
  /*       q = 0xFFFFFFFF; */

  /*     add(a,s,q); */

  /*     /\* Shift a[] one bit left *\/ */
  /*     shift_l(a); */

  /*     /\* Shift each b[] element one position to the right *\/ */
  /*     shift_r(b); */
  /*   } */

  for(i = 0; i < 2*N; i++)
    {
      output_alice(s[i]);
    }
}
