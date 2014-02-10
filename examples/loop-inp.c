unsigned int alice(unsigned int);
unsigned int bob(unsigned int);
void output_alice(unsigned int);

unsigned int s[2];

void main(void)
{
  unsigned int i;
  for(i = 0; i < 2; i++)
    {
      s[i] = alice(i*32);
    }
  for(i = 0; i < 2; i++)
    output_alice(s[i]);
}
