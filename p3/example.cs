class Hello
{
    int g;
    
    void main()
    {
        char b;
        // single-line comments should work now
        b = 'A';    /*
            but
                what
                    about
                        multiline
                            comments?
                */
        b = '\n';
        b = '\t';/* Ow, multiple multiline comments conflict :/ */// NOT ANYMORE :D
        int i;
        i = 3 * 3 + 3;
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
    
    int fac(int x)
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
