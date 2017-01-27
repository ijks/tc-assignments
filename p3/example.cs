class Hello
{
    int g;
    
    void main()
    {
        int a;

        if (a)
        {
        }

        if (false)
        {
          int i;
        }
        else
          for(int j;j<2;j++)
          {
              a--;
          }
    }
    
    int square( int x )
    {
        int y;
        y = x*x;
        return y;
    }

    int abs(int x)
    {
    	
        if (x<0)
            x = 0-x;
        return x;
    }
    
    int fac(int x, char x)
    {
        int r; int t;
        t=1; r=1;
        while (t<=x)
        {
            r = r*t;
            t = t+1;
        }
        return r;
   }
}
